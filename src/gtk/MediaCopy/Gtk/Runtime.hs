{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( Runtime (..)
  , start
  , dispatch
  ) where

import Control.Concurrent.Async
import Control.Exception (SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forM_, void, when, (>=>))
import Data.GI.Base (AttrOp ((:=)), new)
import Data.GI.Base.GError (catchGErrorJustDomain)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Effectful (Eff, liftIO, runEff)
import Effectful.Exception (catchSync, isSyncException)
import Effectful.Time (runTime)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Network.HostName qualified as HostName
import System.File.OsPath (writeFile')
import System.OsPath (OsPath, encodeFS)

import MediaCopy.Domain.Job (JobEvent (..), JobId, JobSpec (..))
import MediaCopy.Domain.Plan (JobPlan (..))
import MediaCopy.Effects.Emit (runEmitIO)
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (runHasher)
import MediaCopy.Engine
import MediaCopy.EventLog (withEventLog)
import MediaCopy.Gtk.Eff (idleE, lowerE, onE, timeoutE, toIO1, withStreak)
import MediaCopy.Gtk.Environment (Ui, abortApp, exitOnFault, logFault, runUi, withEnvironment)
import MediaCopy.Gtk.Reload (loadCss)
import MediaCopy.Gtk.Screenshot (Startup (..), seeded)
import MediaCopy.Gtk.Theme
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Interface.Theme (PaletteMode (..), themeSections)
import MediaCopy.Interface.Wording (noHistoryText)
import MediaCopy.Model
import MediaCopy.Report (renderPlanText)

data Runtime es = Runtime
  { modelRef :: IORef Model
  , widgets :: Widgets es
  , engine :: IORef (Maybe (JobId, Async ()))
  }

start :: Maybe Startup -> IO ()
start startup = withEnvironment $ \environment -> do
  runtimeRef <- newIORef Nothing
  startupFailed <- newIORef False
  status <- runUi environment (reportFault runtimeRef) $ do
    app <- new Adw.Application [#applicationId := "eu.choutri.MediaCopy3000"]
    _ <- onE app #activate (activate runtimeRef startupFailed startup ?self)
    Gio.applicationRun app Nothing
  exitOnFault status startupFailed

reportFault :: (Ui es) => IORef (Maybe (Runtime es)) -> SomeException -> Eff es ()
reportFault runtimeRef err = do
  message <- logFault err
  liftIO (readIORef runtimeRef) >>= mapM_ (\runtime -> postFaultToast runtime message)

activate :: (Ui es) => IORef (Maybe (Runtime es)) -> IORef Bool -> Maybe Startup -> Adw.Application -> Eff es ()
activate runtimeRef startupFailed startup app =
  liftIO (readIORef runtimeRef) >>= \case
    Just runtime -> Gtk.windowPresent runtime.widgets.window
    Nothing -> buildAndPresent runtimeRef startup app abort `catchSync` abort
  where
    abort = abortApp startupFailed app

buildAndPresent
  :: (Ui es)
  => IORef (Maybe (Runtime es))
  -> Maybe Startup
  -> Adw.Application
  -> (SomeException -> Eff es ())
  -> Eff es ()
buildAndPresent runtimeRef startup app abort = do
  startedAt <- liftIO getCurrentTime
  themeAdapter <- newThemeAdapter
  desktop <- readDesktopBase themeAdapter
  modelRef <- liftIO (newIORef (initialModel startedAt desktop))
  engine <- liftIO (newIORef Nothing)
  let dispatchNow msg =
        liftIO (readIORef runtimeRef) >>= \case
          Nothing -> pure ()
          Just runtime -> dispatch runtime msg
  palettes <- loadPalettes
  widgets <- buildWidgets app (apply themeAdapter) (themeSections LightPalette palettes) (themeSections DarkPalette palettes) (\intent -> dispatchNow (Ui intent))
  loadCss
  let runtime = Runtime {modelRef, widgets, engine}
  liftIO (writeIORef runtimeRef (Just runtime))
  onDesktopBase themeAdapter (\observed -> postMessage runtime (DesktopBase observed))
  installCloseRequest widgets.window dispatchNow
  installTicker startup runtime
  model <- liftIO (readIORef modelRef)
  widgets.render model
  Gtk.windowPresent widgets.window
  let showFrame frame = do
        liftIO (writeIORef modelRef frame)
        widgets.render frame
  mapM_ (seeded widgets.window app showFrame abort) startup

dispatch :: (Ui es) => Runtime es -> Message -> Eff es ()
dispatch runtime msg = do
  old <- liftIO (readIORef runtime.modelRef)
  let (current, cmds) = update msg old
  liftIO (writeIORef runtime.modelRef current)
  runtime.widgets.render current
  mapM_ (\cmd -> guarded runtime (runCommand runtime cmd)) cmds

postMessage :: (Ui es) => Runtime es -> Message -> Eff es ()
postMessage runtime msg = idleE (dispatch runtime msg)

guarded :: (Ui es) => Runtime es -> Eff es () -> Eff es ()
guarded runtime action = action `catchSync` (logFault >=> postFaultToast runtime)

postFaultToast :: (Ui es) => Runtime es -> Text -> Eff es ()
postFaultToast runtime message =
  idleE (dispatch runtime (ShowToast message) `catchSync` (void . logFault))

runCommand :: (Ui es) => Runtime es -> Command -> Eff es ()
runCommand runtime = \case
  OpenFolderDialog toMessage -> openFolderDialog runtime toMessage
  OpenSaveDialog title suggested toMessage -> openSaveDialog runtime title suggested toMessage
  ComputePlan spec -> planWorker runtime spec
  StartJob plan -> startJob runtime plan
  CancelRunning jobId ->
    liftIO (readIORef runtime.engine) >>= \case
      Just (held, worker)
        | held == jobId -> liftIO (void (async (cancel worker)))
      _ -> pure ()
  LoadHistory jobId folder -> loadHistory runtime jobId folder
  WriteFile path text -> liftIO (writeFile' path (encodeUtf8 text))
  CloseWindow -> Gtk.windowDestroy runtime.widgets.window

installCloseRequest :: (Ui es) => Adw.ApplicationWindow -> (Message -> Eff es ()) -> Eff es ()
installCloseRequest window dispatchNow =
  void $ onE window #closeRequest $ do
    dispatchNow (Ui RequestClose)
    pure True

installTicker :: (Ui es) => Maybe Startup -> Runtime es -> Eff es ()
installTicker startup runtime =
  when (isNothing startup) $ do
    failing <- liftIO (newIORef False)
    void
      ( timeoutE
          1_000
          ( do
              withStreak failing (logFault >=> postFaultToast runtime) (void . logFault) $ do
                t <- liftIO getCurrentTime
                dispatch runtime (Tick t)
              pure True
          )
      )

openFolderDialog :: (Ui es) => Runtime es -> (OsPath -> Message) -> Eff es ()
openFolderDialog runtime toMessage = do
  dialog <- new Gtk.FileDialog [#title := "Choose a Folder"]
  send <- toIO1 (postMessage runtime)
  callback <- lowerE @Gio.AsyncReadyCallback $ \_source result ->
    liftIO (sendPicked send toMessage (Gtk.fileDialogSelectFolderFinish dialog result))
  Gtk.fileDialogSelectFolder dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) (Just callback)

openSaveDialog :: (Ui es) => Runtime es -> Text -> Text -> (OsPath -> Message) -> Eff es ()
openSaveDialog runtime title suggested toMessage = do
  dialog <- new Gtk.FileDialog [#title := title, #initialName := suggested]
  send <- toIO1 (postMessage runtime)
  callback <- lowerE @Gio.AsyncReadyCallback $ \_source result ->
    liftIO (sendPicked send toMessage (Gtk.fileDialogSaveFinish dialog result))
  Gtk.fileDialogSave dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) (Just callback)

startJob :: (Ui es) => Runtime es -> JobPlan -> Eff es ()
startJob runtime plan = do
  send <- toIO1 (postMessage runtime)
  liftIO $ do
    let spec = plan.spec
    host <- HostName.getHostName
    let sink event = send (EngineEvent spec.jobId event)
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
              send (EngineEvent spec.jobId (JobFailed (T.pack (displayException err))))
        _ -> pure ()
      atomicModifyIORef' runtime.engine (\held -> (clearWhen spec.jobId held, ()))

loadHistory :: (Ui es) => Runtime es -> JobId -> OsPath -> Eff es ()
loadHistory runtime jobId folder = do
  send <- toIO1 (postMessage runtime)
  liftIO $ void $ async $ do
    loaded <- runEff (runFileSystemIO defaultChunkSize (readHistory folder))
    case loaded of
      Left e -> send (ShowToast (display e))
      Right Nothing -> send (ShowToast (noHistoryText folder))
      Right (Just hist) -> send (HistoryLoaded jobId hist)

planWorker :: (Ui es) => Runtime es -> JobSpec -> Eff es ()
planWorker runtime spec = do
  send <- toIO1 (postMessage runtime)
  liftIO $ void $ async $ do
    attempt <- try @SomeException (runEff (runFileSystemIO defaultChunkSize (planJob spec)))
    case attempt of
      Left err
        | isSyncException err -> send (PlanComputed spec (Left (T.pack (displayException err))))
        | otherwise -> throwIO err
      Right plan -> send (PlanComputed spec (Right plan))

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

sendPicked :: (PickedFile file) => (Message -> IO ()) -> (OsPath -> Message) -> IO file -> IO ()
sendPicked send toMessage finish =
  catchGErrorJustDomain
    (finish >>= \picked -> mapM_ (sendPath send toMessage) (pickedFile picked))
    (\dialogError message -> reportDialogError send dialogError message)

reportDialogError :: (Message -> IO ()) -> Gtk.DialogError -> Text -> IO ()
reportDialogError send dialogError message = case dialogError of
  (Gtk.DialogErrorCancelled; Gtk.DialogErrorDismissed) -> pure ()
  _ -> send (ShowToast message)

sendPath :: (Message -> IO ()) -> (OsPath -> Message) -> Gio.File -> IO ()
sendPath send toMessage file =
  Gio.fileGetPath file >>= \case
    Nothing -> send (ShowToast "Chosen location has no filesystem path")
    Just raw -> encodeFS raw >>= \path -> send (toMessage path)
