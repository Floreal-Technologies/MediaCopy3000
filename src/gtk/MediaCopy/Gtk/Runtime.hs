{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( Runtime (..)
  , start
  , dispatch
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception
import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Data.GI.Base.GError (catchGErrorJustDomain)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Data.Vector (Vector)
import Effectful (runEff)
import Effectful.Time (runTime)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Network.HostName qualified as HostName
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.File.OsPath (writeFile')
import System.IO (stderr)
import System.OsPath (OsPath, encodeFS)

import MediaCopy.Domain.Job (JobEvent (..), JobId, JobSpec (..))
import MediaCopy.Domain.Plan (JobPlan (..), planBlocked)
import MediaCopy.Domain.Plugin (PluginReport (..))
import MediaCopy.Domain.PluginCatalog (CatalogChange)
import MediaCopy.Effects.Emit (runEmitIO)
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (runHasher)
import MediaCopy.Effects.Plugins (runPluginsSession)
import MediaCopy.Engine
import MediaCopy.EventLog (withEventLog)
import MediaCopy.Gtk.Environment (Environment, withEnvironment)
import MediaCopy.Gtk.Reload (loadCss, loadWording)
import MediaCopy.Gtk.Screenshot (Startup (..), seeded)
import MediaCopy.Gtk.Theme
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Guard (guarded)
import MediaCopy.Interface.Theme (PaletteMode (..), themeSections)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Embedded
import MediaCopy.Interface.Wording (noHistoryText)
import MediaCopy.Model
import MediaCopy.Plugin (PluginSetup (..), loadPluginSetup, planWithPlugins)
import MediaCopy.Plugin.Catalog (applyChange, loadCatalog)
import MediaCopy.Plugin.Grants (Ready)
import MediaCopy.Plugin.Session (SessionConfig (..), Stage (RunStage), observe, withSession)
import MediaCopy.Report (renderPlanText)
import MediaCopy.Signals (onStopSignal)

data Runtime = Runtime
  { modelRef :: IORef Model
  , widgets :: Widgets
  , engine :: IORef (Maybe (JobId, Async ()))
  , workers :: IORef (Set (Async ()))
  , catalogLock :: MVar ()
  , planner :: IORef (Maybe (JobId, Async ()))
  }

start :: Maybe Startup -> IO ()
start startup = withEnvironment $ \environment -> do
  runtimeRef <- newIORef Nothing
  app <-
    new
      Adw.Application
      [ #applicationId := "tech.floreal.MediaCopy3000"
      , On #activate (activate runtimeRef environment startup ?self)
      ]
  onStopSignal (void (GLib.idleAdd GLib.PRIORITY_DEFAULT (Gio.applicationQuit app >> pure False)))
  status <- Gio.applicationRun app Nothing `finally` (readIORef runtimeRef >>= mapM_ stopBackground)
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
  workers <- newIORef Set.empty
  catalogLock <- newMVar ()
  planner <- newIORef Nothing
  let dispatchNow msg =
        readIORef runtimeRef >>= \case
          Nothing -> pure ()
          Just runtime -> dispatch runtime msg
  palettes <- loadPalettes environment
  wording <- (.wording) <$> readIORef modelRef
  widgets <-
    buildWidgets
      app
      (apply themeAdapter)
      wording
      (themeSections (embeddedWording English) LightPalette palettes)
      (themeSections (embeddedWording English) DarkPalette palettes)
      (dispatchNow . Ui)
  loadCss environment
  let runtime = Runtime {modelRef, widgets, engine, workers, catalogLock, planner}
  writeIORef runtimeRef (Just runtime)
  loadWording
    environment
    wording.language
    (postMessage runtime . WordingReloaded)
    (postMessage runtime . ShowToast)
  onDesktopBase themeAdapter (postMessage runtime . DesktopBase)
  installCloseRequest widgets.window dispatchNow
  installTicker startup dispatchNow
  model <- readIORef modelRef
  widgets.render model
  Gtk.windowPresent widgets.window
  when (isNothing startup) (catalogWorker runtime)
  let showFrame frame = do
        writeIORef modelRef frame
        widgets.render frame
  forM_ startup (seeded environment widgets.window widgets.activate showFrame)

dispatch :: Runtime -> Message -> IO ()
dispatch runtime msg = do
  old <- readIORef runtime.modelRef
  let (current, cmds) = update msg old
  writeIORef runtime.modelRef current
  runtime.widgets.render current
  mapM_ (toasting runtime . runCommand runtime) cmds

postMessage :: Runtime -> Message -> IO ()
postMessage runtime msg =
  void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatch runtime msg >> pure False))

tracked :: Runtime -> IO () -> IO (Async ())
tracked runtime action = mask_ $ do
  worker <- asyncWithUnmask (\unmask -> unmask action)
  atomicModifyIORef' runtime.workers (\held -> (Set.insert worker held, ()))
  void $ async $ do
    void (waitCatch worker)
    atomicModifyIORef' runtime.workers (\held -> (Set.delete worker held, ()))
  pure worker

stopBackground :: Runtime -> IO ()
stopBackground runtime = readIORef runtime.workers >>= mapConcurrently_ cancel

toasting :: Runtime -> IO () -> IO ()
toasting runtime action = guarded action >>= either (toastFailure runtime) pure

toastFailure :: Runtime -> SomeException -> IO ()
toastFailure runtime err = postMessage runtime (ShowToast (T.pack (displayException err)))

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
  OpenFileDialog toMessage -> openFileDialog runtime toMessage
  LoadCatalog -> catalogWorker runtime
  ApplyChange change -> changeWorker runtime change
  ShowFolder folder -> showFolder runtime folder
  Activate command -> runtime.widgets.activate command

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
    toasting runtime (sendPicked runtime toMessage (Gtk.fileDialogSelectFolderFinish dialog result))

openSaveDialog :: Runtime -> Text -> Text -> (OsPath -> Message) -> IO ()
openSaveDialog runtime title suggested toMessage = do
  dialog <- new Gtk.FileDialog [#title := title, #initialName := suggested]
  Gtk.fileDialogSave dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    toasting runtime (sendPicked runtime toMessage (Gtk.fileDialogSaveFinish dialog result))

openFileDialog :: Runtime -> (OsPath -> Message) -> IO ()
openFileDialog runtime toMessage = do
  dialog <- new Gtk.FileDialog [#title := "Choose a File"]
  Gtk.fileDialogOpen dialog (Just runtime.widgets.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    toasting runtime (sendPicked runtime toMessage (Gtk.fileDialogOpenFinish dialog result))

showFolder :: Runtime -> Text -> IO ()
showFolder runtime folder = do
  file <- Gio.fileNewForPath (T.unpack folder)
  launcher <- Gtk.fileLauncherNew (Just file)
  Gtk.fileLauncherLaunch launcher (Just runtime.widgets.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    toasting runtime (Gtk.fileLauncherLaunchFinish launcher result)

catalogWorker :: Runtime -> IO ()
catalogWorker runtime = void $ async (withMVar runtime.catalogLock (const (reloadCatalog runtime)))

changeWorker :: Runtime -> CatalogChange -> IO ()
changeWorker runtime change =
  void $ async $ withMVar runtime.catalogLock $ \() -> do
    guarded (applyChange change) >>= \case
      Left err -> toastFailure runtime err
      Right (Left problem) -> postMessage runtime (ShowToast problem)
      Right (Right ()) -> pure ()
    reloadCatalog runtime

reloadCatalog :: Runtime -> IO ()
reloadCatalog runtime =
  guarded loadCatalog >>= \case
    Left err -> toastFailure runtime err
    Right catalog -> postMessage runtime (CatalogLoaded catalog)

startJob :: Runtime -> JobPlan -> IO ()
startJob runtime plan = do
  let spec = plan.spec
  host <- HostName.getHostName
  let sink event = postMessage runtime (EngineEvent spec.jobId event)
  previous <- readIORef runtime.engine
  worker <- tracked runtime $ do
    mapM_ (waitCatch . snd) previous
    setup <- loadPluginSetup
    model <- readIORef runtime.modelRef
    withEventLog spec (renderPlanText spec plan) $ \logPath logLine -> do
      forM_ logPath (sink . LogOpened)
      let reported report = do
            logLine (PluginReported report)
            unless (logOnly report) (sink (PluginReported report))
      if planBlocked plan
        then runEff (runFileSystemIO defaultChunkSize (runHasher (runTime (runEmitIO (\ev -> logLine ev >> sink ev) (executePlan (T.pack host) plan)))))
        else withSession (pluginConfig model.wording setup.ready reported) RunStage plan $ \session ->
          runEff (runFileSystemIO defaultChunkSize (runHasher (runTime (runEmitIO (\ev -> logLine ev >> observe session ev >> sink ev) (runPluginsSession session (executePlanWithPlugins (T.pack host) plan))))))
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
    Right Nothing -> do
      model <- readIORef runtime.modelRef
      postMessage runtime (ShowToast (noHistoryText model.wording folder))
    Right (Just hist) -> postMessage runtime (HistoryLoaded jobId hist)

planWorker :: Runtime -> JobSpec -> IO ()
planWorker runtime spec = do
  worker <- tracked runtime $ do
    model <- readIORef runtime.modelRef
    attempt <- guarded $ do
      plan <- runEff (runFileSystemIO defaultChunkSize (planJob spec))
      setup <- loadPluginSetup
      forM_ setup.grantsProblem (postMessage runtime . ShowToast)
      planWithPlugins (pluginConfig model.wording setup.ready (TIO.hPutStrLn stderr . display)) plan
    case attempt of
      Left err -> postMessage runtime (PlanComputed spec (Left (T.pack (displayException err))))
      Right plan -> postMessage runtime (PlanComputed spec (Right plan))
  previous <- atomicModifyIORef' runtime.planner (\held -> (Just (spec.jobId, worker), held))
  forM_ previous $ \(jid, old) -> when (jid == spec.jobId) (void (async (cancel old)))

pluginConfig :: Wording -> Vector Ready -> (PluginReport -> IO ()) -> SessionConfig
pluginConfig wording plugins report =
  SessionConfig
    { plugins
    , locale = languageCode wording.language
    , report
    }

logOnly :: PluginReport -> Bool
logOnly = \case
  Logged _ _ -> True
  _ -> False

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
    (reportDialogError runtime)

reportDialogError :: Runtime -> Gtk.DialogError -> Text -> IO ()
reportDialogError runtime dialogError message = case dialogError of
  (Gtk.DialogErrorCancelled; Gtk.DialogErrorDismissed) -> pure ()
  _ -> postMessage runtime (ShowToast message)

sendPath :: Runtime -> (OsPath -> Message) -> Gio.File -> IO ()
sendPath runtime toMessage file =
  Gio.fileGetPath file >>= \case
    Nothing -> postMessage runtime (ShowToast "Chosen location has no filesystem path")
    Just raw -> encodeFS raw >>= \path -> postMessage runtime (toMessage path)
