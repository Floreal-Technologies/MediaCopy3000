module MediaCopy.Gtk.Interpret
  ( Production
  , newProduction
  , runEffect
  , stopWorkers
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception
import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.GI.Base.GError (catchGErrorJustDomain)
import Data.IORef
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Network.HostName qualified as HostName
import System.File.OsPath (writeFile')
import System.IO (stderr)
import System.OsPath (OsPath, encodeFS)

import MediaCopy.Domain.Job (JobEvent (..), JobId, JobSpec (..))
import MediaCopy.Domain.Plan (JobPlan (..), planBlocked)
import MediaCopy.Domain.Plugin (PluginReport (..))
import MediaCopy.Domain.PluginCatalog (CatalogChange)
import MediaCopy.Effects.Emit (runEmitIO)
import MediaCopy.Effects.Plugins (runPluginsNone, runPluginsSession)
import MediaCopy.Effects.Run (runApp)
import MediaCopy.Engine
import MediaCopy.EventLog (withEventLog)
import MediaCopy.Gtk.View (Widgets (..))
import MediaCopy.Guard (guarded)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording (noHistoryText)
import MediaCopy.Model
import MediaCopy.Plugin.Catalog (applyChange, loadCatalog)
import MediaCopy.Plugin.Grants (PluginSetup (..), Ready, loadPluginSetup)
import MediaCopy.Plugin.Session (SessionConfig (..), Stage (RunStage), planWithPlugins, withSession)
import MediaCopy.Report (renderPlanText)

data Production = Production
  { window :: Adw.ApplicationWindow
  , present :: Chrome -> IO ()
  , post :: Message -> IO ()
  , engine :: IORef (Maybe (JobId, Async ()))
  , planner :: IORef (Maybe (JobId, Async ()))
  , workers :: IORef (Set (Async ()))
  , catalogLock :: MVar ()
  }

newProduction :: Widgets -> (Message -> IO ()) -> IO Production
newProduction widgets post = do
  engine <- newIORef Nothing
  planner <- newIORef Nothing
  workers <- newIORef Set.empty
  catalogLock <- newMVar ()
  pure Production {window = widgets.window, present = widgets.present, post, engine, planner, workers, catalogLock}

stopWorkers :: Production -> IO ()
stopWorkers site = readIORef site.workers >>= mapConcurrently_ cancel

runEffect :: Production -> Model -> AppEffect -> IO ()
runEffect site model effect = toasting site $ case effect of
  OpenDialog kind toMessage -> openDialog site kind toMessage
  ComputePlan spec -> planWorker site wording spec
  StartJob plan -> startJob site wording plan
  CancelRunning jobId ->
    readIORef site.engine >>= \case
      Just (held, worker)
        | held == jobId -> void (async (cancel worker))
      _ -> pure ()
  LoadHistory jobId folder -> loadHistory site wording jobId folder
  WriteFile path text -> writeFile' path (encodeUtf8 text)
  DestroyWindow -> Gtk.windowDestroy site.window
  LoadCatalog -> catalogWorker site
  ApplyChange change -> changeWorker site change
  ShowFolder folder -> showFolder site folder
  ShowChrome chrome -> site.present chrome
  where
    wording = model.wording

tracked :: Production -> IO () -> IO (Async ())
tracked site action = mask_ $ do
  worker <- asyncWithUnmask (\unmask -> unmask action)
  atomicModifyIORef' site.workers (\held -> (Set.insert worker held, ()))
  void $ async $ do
    void (waitCatch worker)
    atomicModifyIORef' site.workers (\held -> (Set.delete worker held, ()))
  pure worker

toasting :: Production -> IO () -> IO ()
toasting site action = guarded action >>= either (toastFailure site) pure

toastFailure :: Production -> SomeException -> IO ()
toastFailure site err = site.post (ShowToast (T.pack (displayException err)))

openDialog :: Production -> DialogKind -> (OsPath -> Message) -> IO ()
openDialog site kind toMessage = do
  dialog <- new Gtk.FileDialog []
  let picked finish = toasting site (sendPicked site toMessage finish)
  case kind of
    PickFolder -> do
      set dialog [#title := "Choose a Folder"]
      Gtk.fileDialogSelectFolder dialog (Just site.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogSelectFolderFinish dialog result)
    PickFile -> do
      set dialog [#title := "Choose a File"]
      Gtk.fileDialogOpen dialog (Just site.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogOpenFinish dialog result)
    SaveAs title suggested -> do
      set dialog [#title := title, #initialName := suggested]
      Gtk.fileDialogSave dialog (Just site.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogSaveFinish dialog result)

showFolder :: Production -> Text -> IO ()
showFolder site folder = do
  file <- Gio.fileNewForPath (T.unpack folder)
  launcher <- Gtk.fileLauncherNew (Just file)
  Gtk.fileLauncherLaunch launcher (Just site.window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    toasting site (Gtk.fileLauncherLaunchFinish launcher result)

catalogWorker :: Production -> IO ()
catalogWorker site = void $ async (withMVar site.catalogLock (const (reloadCatalog site)))

changeWorker :: Production -> CatalogChange -> IO ()
changeWorker site change =
  void $ async $ withMVar site.catalogLock $ \() -> do
    guarded (applyChange change) >>= \case
      Left err -> toastFailure site err
      Right (Left problem) -> site.post (ShowToast problem)
      Right (Right ()) -> pure ()
    reloadCatalog site

reloadCatalog :: Production -> IO ()
reloadCatalog site =
  guarded loadCatalog >>= \case
    Left err -> toastFailure site err
    Right catalog -> site.post (CatalogLoaded catalog)

startJob :: Production -> Wording -> JobPlan -> IO ()
startJob site wording plan = do
  let spec = plan.spec
  host <- HostName.getHostName
  let sink event = site.post (EngineEvent spec.jobId event)
  previous <- readIORef site.engine
  worker <- tracked site $ do
    forM_ previous (waitCatch . snd)
    setup <- loadPluginSetup
    withEventLog spec (renderPlanText spec plan) $ \logPath logLine -> do
      forM_ logPath (sink . LogOpened)
      let reported report = do
            logLine (PluginReported report)
            unless (logOnly report) (sink (PluginReported report))
          emitted ev = logLine ev >> sink ev
      if planBlocked plan
        then runApp (runEmitIO emitted (runPluginsNone (executePlan (T.pack host) plan)))
        else withSession (pluginConfig wording setup.ready reported) RunStage plan $ \session ->
          runApp (runEmitIO emitted (runPluginsSession session (executePlan (T.pack host) plan)))
  writeIORef site.engine (Just (spec.jobId, worker))
  void $ async $ do
    outcome <- waitCatch worker
    case outcome of
      Left err
        | not (wasCancelled err) ->
            site.post (EngineEvent spec.jobId (JobFailed (T.pack (displayException err))))
      _ -> pure ()
    atomicModifyIORef' site.engine (\held -> (clearWhen spec.jobId held, ()))

loadHistory :: Production -> Wording -> JobId -> OsPath -> IO ()
loadHistory site wording jobId folder = void $ async $ do
  loaded <- runApp (readHistory folder)
  case loaded of
    Left e -> site.post (ShowToast (display e))
    Right Nothing -> site.post (ShowToast (noHistoryText wording folder))
    Right (Just hist) -> site.post (HistoryLoaded jobId hist)

planWorker :: Production -> Wording -> JobSpec -> IO ()
planWorker site wording spec = do
  worker <- tracked site $ do
    attempt <- guarded $ do
      plan <- runApp (planJob spec)
      setup <- loadPluginSetup
      forM_ setup.grantsProblem (site.post . ShowToast)
      planWithPlugins (pluginConfig wording setup.ready (TIO.hPutStrLn stderr . display)) plan
    case attempt of
      Left err -> site.post (PlanComputed spec (Left (T.pack (displayException err))))
      Right plan -> site.post (PlanComputed spec (Right plan))
  previous <- atomicModifyIORef' site.planner (\held -> (Just (spec.jobId, worker), held))
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

-- | Different Gio versions give different nullability information,
-- so we have to abstract with a simple typeclass.
class PickedFile file where
  pickedFile :: file -> Maybe Gio.File

instance PickedFile Gio.File where
  pickedFile = Just

instance PickedFile (Maybe Gio.File) where
  pickedFile = id

sendPicked :: (PickedFile file) => Production -> (OsPath -> Message) -> IO file -> IO ()
sendPicked site toMessage finish =
  catchGErrorJustDomain
    (finish >>= \picked -> forM_ (pickedFile picked) (sendPath site toMessage))
    (reportDialogError site)

reportDialogError :: Production -> Gtk.DialogError -> Text -> IO ()
reportDialogError site dialogError message = case dialogError of
  (Gtk.DialogErrorCancelled; Gtk.DialogErrorDismissed) -> pure ()
  _ -> site.post (ShowToast message)

sendPath :: Production -> (OsPath -> Message) -> Gio.File -> IO ()
sendPath site toMessage file =
  Gio.fileGetPath file >>= \case
    Nothing -> site.post (ShowToast "Chosen location has no filesystem path")
    Just raw -> encodeFS raw >>= \path -> site.post (toMessage path)
