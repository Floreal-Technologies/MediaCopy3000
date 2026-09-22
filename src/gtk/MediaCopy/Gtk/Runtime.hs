{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( Runtime (..)
  , start
  , dispatch
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forM_, void, when)
import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Data.GI.Base.GError (catchGErrorJustDomain)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Effectful (runEff)
import Effectful.Time (runTime)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Network.HostName qualified as HostName
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.File.OsPath (writeFile')
import System.OsPath (OsPath, encodeFS)

import MediaCopy.Domain.Job (JobEvent (..), JobId, JobSpec (..))
import MediaCopy.Domain.Plan (JobPlan (..))
import MediaCopy.Effects.Emit (runEmitIO)
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (runHasher)
import MediaCopy.Engine
import MediaCopy.EventLog (withEventLog)
import MediaCopy.Gtk.Environment (Environment, withEnvironment)
import MediaCopy.Gtk.Reload (loadCss)
import MediaCopy.Gtk.Screenshot (Startup (..), seeded)
import MediaCopy.Gtk.Theme
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Interface.Theme (PaletteMode (..), themeSections)
import MediaCopy.Interface.Wording (noHistoryText)
import MediaCopy.Model
import MediaCopy.Report (renderPlanText)

data Runtime = Runtime
  { modelRef :: IORef Model
  , widgets :: Widgets
  , engine :: IORef (Maybe (JobId, Async ()))
  }

start :: Maybe Startup -> IO ()
start startup = withEnvironment $ \environment -> do
  runtimeRef <- newIORef Nothing
  app <-
    new
      Adw.Application
      [ #applicationId := "eu.choutri.MediaCopy3000"
      , On #activate (activate runtimeRef environment startup ?self)
      ]
  status <- Gio.applicationRun app Nothing
  when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))

activate :: IORef (Maybe Runtime) -> Environment -> Maybe Startup -> Adw.Application -> IO ()
activate runtimeRef environment startup app =
  readIORef runtimeRef >>= \case
    Just runtime -> Gtk.windowPresent runtime.widgets.window
    Nothing -> buildAndPresent runtimeRef environment startup app

buildAndPresent
  :: IORef (Maybe Runtime)
  -> Environment
  -> Maybe Startup
  -> Adw.Application
  -> IO ()
buildAndPresent runtimeRef environment startup app = do
  startedAt <- getCurrentTime
  themeAdapter <- newThemeAdapter environment
  desktop <- readDesktopBase themeAdapter
  modelRef <- newIORef (initialModel startedAt desktop)
  engine <- newIORef Nothing
  let dispatchNow msg =
        readIORef runtimeRef >>= \case
          Nothing -> pure ()
          Just runtime -> dispatch runtime msg
  palettes <- loadPalettes environment
  widgets <- buildWidgets app (apply themeAdapter) (themeSections LightPalette palettes) (themeSections DarkPalette palettes) (\intent -> dispatchNow (Ui intent))
  loadCss environment
  let runtime = Runtime {modelRef, widgets, engine}
  writeIORef runtimeRef (Just runtime)
  onDesktopBase themeAdapter (\observed -> postMessage runtime (DesktopBase observed))
  installCloseRequest widgets.window dispatchNow
  installTicker startup dispatchNow
  model <- readIORef modelRef
  widgets.render model
  Gtk.windowPresent widgets.window
  let showFrame frame = do
        writeIORef modelRef frame
        widgets.render frame
  mapM_ (seeded environment widgets.window app showFrame) startup

dispatch :: Runtime -> Message -> IO ()
dispatch runtime msg = do
  old <- readIORef runtime.modelRef
  let (current, cmds) = update msg old
  writeIORef runtime.modelRef current
  runtime.widgets.render current
  mapM_ (\cmd -> guarded runtime (runCommand runtime cmd)) cmds

postMessage :: Runtime -> Message -> IO ()
postMessage runtime msg =
  void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatch runtime msg >> pure False))

guarded :: Runtime -> IO () -> IO ()
guarded runtime action =
  try @SomeException action >>= \case
    Left err -> case fromException @SomeAsyncException err of
      Just _ -> throwIO err
      Nothing -> postMessage runtime (ShowToast (T.pack (displayException err)))
    Right () -> pure ()

runCommand :: Runtime -> Command -> IO ()
runCommand runtime = \case
  OpenFolderDialog toMessage -> openFolderDialog runtime toMessage
  OpenSaveDialog title suggested toMessage -> openSaveDialog runtime title suggested toMessage
  ComputePlan spec -> planWorker runtime spec
  StartJob plan -> startJob runtime plan
  CancelRunning jobId ->
    readIORef runtime.engine >>= \case
      Just (held, worker)
        | held == jobId -> void (async (cancel worker))
      _ -> pure ()
  LoadHistory jobId folder -> loadHistory runtime jobId folder
  WriteFile path text -> writeFile' path (encodeUtf8 text)
  CloseWindow -> Gtk.windowDestroy runtime.widgets.window

installCloseRequest :: Adw.ApplicationWindow -> (Message -> IO ()) -> IO ()
installCloseRequest window dispatchNow =
  void $ on window #closeRequest $ do
    dispatchNow (Ui RequestClose)
    pure True

installTicker :: Maybe Startup -> (Message -> IO ()) -> IO ()
installTicker startup dispatchNow =
  when (isNothing startup) $
    void
      ( GLib.timeoutAdd
          GLib.PRIORITY_DEFAULT
          1_000
          ( getCurrentTime >>= \t ->
              dispatchNow (Tick t) >> pure True
          )
      )

openFolderDialog :: Runtime -> (OsPath -> Message) -> IO ()
openFolderDialog runtime toMessage = do
  dialog <- new Gtk.FileDialog [#title := "Choose a Folder"]
  Gtk.fileDialogSelectFolder dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    guarded runtime (sendPicked runtime toMessage (Gtk.fileDialogSelectFolderFinish dialog result))

openSaveDialog :: Runtime -> Text -> Text -> (OsPath -> Message) -> IO ()
openSaveDialog runtime title suggested toMessage = do
  dialog <- new Gtk.FileDialog [#title := title, #initialName := suggested]
  Gtk.fileDialogSave dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    guarded runtime (sendPicked runtime toMessage (Gtk.fileDialogSaveFinish dialog result))

startJob :: Runtime -> JobPlan -> IO ()
startJob runtime plan = do
  let spec = plan.spec
  host <- HostName.getHostName
  let sink event = postMessage runtime (EngineEvent spec.jobId event)
  previous <- readIORef runtime.engine
  worker <- async $ do
    mapM_ (\held -> void (waitCatch (snd held))) previous
    withEventLog spec (renderPlanText spec plan) $ \logPath logLine -> do
      forM_ logPath (\p -> sink (LogOpened p))
      runEff (runFileSystemIO defaultChunkSize (runHasher (runTime (runEmitIO (\ev -> logLine ev >> sink ev) (executePlan (T.pack host) plan)))))
  writeIORef runtime.engine (Just (spec.jobId, worker))
  void $ async $ do
    outcome <- waitCatch worker
    case outcome of
      Left err
        | not (wasCancelled err) ->
            postMessage runtime (EngineEvent spec.jobId (JobFailed (T.pack (displayException err))))
      _ -> pure ()
    atomicModifyIORef' runtime.engine (\held -> (clearWhen spec.jobId held, ()))

loadHistory :: Runtime -> JobId -> OsPath -> IO ()
loadHistory runtime jobId folder = void $ async $ do
  loaded <- runEff (runFileSystemIO defaultChunkSize (readHistory folder))
  case loaded of
    Left e -> postMessage runtime (ShowToast (display e))
    Right Nothing -> postMessage runtime (ShowToast (noHistoryText folder))
    Right (Just hist) -> postMessage runtime (HistoryLoaded jobId hist)

planWorker :: Runtime -> JobSpec -> IO ()
planWorker runtime spec =
  void $ async $ do
    attempt <- try @SomeException (runEff (runFileSystemIO defaultChunkSize (planJob spec)))
    case attempt of
      Left err -> case fromException @SomeAsyncException err of
        Just _ -> throwIO err
        Nothing -> postMessage runtime (PlanComputed spec (Left (T.pack (displayException err))))
      Right plan -> postMessage runtime (PlanComputed spec (Right plan))

clearWhen :: JobId -> Maybe (JobId, Async ()) -> Maybe (JobId, Async ())
clearWhen finished held = case held of
  Just (jobId, _) | jobId == finished -> Nothing
  _ -> held

wasCancelled :: SomeException -> Bool
wasCancelled err = case fromException err of
  Just AsyncCancelled -> True
  Nothing -> False

class PickedFile file where
  pickedFile :: file -> Maybe Gio.File

instance PickedFile Gio.File where
  pickedFile = Just

instance PickedFile (Maybe Gio.File) where
  pickedFile = id

sendPicked :: (PickedFile file) => Runtime -> (OsPath -> Message) -> IO file -> IO ()
sendPicked runtime toMessage finish =
  catchGErrorJustDomain
    (finish >>= \picked -> mapM_ (sendPath runtime toMessage) (pickedFile picked))
    (\dialogError message -> reportDialogError runtime dialogError message)

reportDialogError :: Runtime -> Gtk.DialogError -> Text -> IO ()
reportDialogError runtime dialogError message = case dialogError of
  (Gtk.DialogErrorCancelled; Gtk.DialogErrorDismissed) -> pure ()
  _ -> postMessage runtime (ShowToast message)

sendPath :: Runtime -> (OsPath -> Message) -> Gio.File -> IO ()
sendPath runtime toMessage file =
  Gio.fileGetPath file >>= \case
    Nothing -> postMessage runtime (ShowToast "Chosen location has no filesystem path")
    Just raw -> encodeFS raw >>= \path -> postMessage runtime (toMessage path)
