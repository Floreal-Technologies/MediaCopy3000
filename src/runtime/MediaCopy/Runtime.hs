-- | The message loop and the effect wiring. The window gives a 'Shell', the outside world gives a
-- 'Runner', and the loop folds each 'Message' through 'update' and runs the effects it returns.
module MediaCopy.Runtime
  ( Shell (..)
  , Runner (..)
  , Runtime
  , newRuntime
  , dispatch
  , perform
  , currentModel
  , replaceModel
  , inFlight
  , stopWorkers
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (inject)
import Effectful.Log (logAttention_)
import Floreal.Commons.Environment (getHostName)
import Floreal.Commons.Exception (guarded)

import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog
import MediaCopy.Domain.PluginReport (PluginReport)
import MediaCopy.Domain.PluginReport qualified as PluginReport
import MediaCopy.Effects.AppStack (AppStack)
import MediaCopy.Effects.Emit qualified as Emit
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Effects.Plugins qualified as Plugins
import MediaCopy.Engine qualified as Engine
import MediaCopy.Engine.Planner qualified as Planner
import MediaCopy.EventLog qualified as EventLog
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording (noHistoryText)
import MediaCopy.Mhl.Store qualified as Store
import MediaCopy.Model
import MediaCopy.Plugin.Catalog qualified as Catalog
import MediaCopy.Plugin.Ready qualified as Plugin (Ready)
import MediaCopy.Plugin.Session qualified as Session
import MediaCopy.Plugin.Setup qualified as Setup
import MediaCopy.Report (renderPlanText)

-- | What the window gives the runtime. All calls happen on the loop thread, except 'post'.
data Shell = Shell
  { render :: Model -> IO ()
  , post :: Message -> IO ()
  -- ^ Hand a message from a worker thread to the loop.
  , openDialog :: DialogKind -> (OsPath -> Message) -> IO ()
  , destroyWindow :: IO ()
  , showChrome :: Chrome -> IO ()
  , showFolder :: Text -> IO ()
  }

newtype Runner = Runner (forall a. Eff AppStack a -> IO a)

-- | The state of the loop and of the threads that its effects start.
data Runtime = Runtime
  { shell :: Shell
  , runner :: Runner
  , modelRef :: IORef Model
  , busy :: IORef Bool
  , pending :: IORef (Vector Message)
  , engine :: IORef (Maybe (JobId, Async ()))
  , planner :: IORef (Maybe (JobId, Async ()))
  , workers :: IORef (Set (Async ()))
  , catalogLock :: MVar ()
  , threads :: TVar Int
  }

newRuntime :: Shell -> Runner -> Model -> IO Runtime
newRuntime shell runner model = do
  modelRef <- newIORef model
  busy <- newIORef False
  pending <- newIORef V.empty
  engine <- newIORef Nothing
  planner <- newIORef Nothing
  workers <- newIORef Set.empty
  catalogLock <- newMVar ()
  threads <- newTVarIO 0
  pure Runtime {shell, runner, modelRef, busy, pending, engine, planner, workers, catalogLock, threads}

-- | Fold a message through 'update', render the new model, and run its effects. A message sent
-- while the loop runs waits in a queue and runs after the current one. Call it from one thread
-- only: the loop state is in plain 'IORef's.
dispatch :: Runtime -> Message -> IO ()
dispatch runtime msg = do
  modifyIORef' runtime.pending (`V.snoc` msg)
  running <- readIORef runtime.busy
  unless running $ do
    writeIORef runtime.busy True
    drain runtime `finally` (writeIORef runtime.busy False >> writeIORef runtime.pending V.empty)

drain :: Runtime -> IO ()
drain runtime =
  readIORef runtime.pending >>= \queued -> case V.uncons queued of
    Nothing -> pure ()
    Just (next, rest) -> do
      writeIORef runtime.pending rest
      step runtime next
      drain runtime

step :: Runtime -> Message -> IO ()
step runtime msg = do
  old <- readIORef runtime.modelRef
  let (current, cmds) = update msg old
  writeIORef runtime.modelRef current
  runtime.shell.render current
  forM_ cmds (runEffect runtime current)

-- | Run one effect against the current model.
perform :: Runtime -> AppEffect -> IO ()
perform runtime effect = do
  model <- readIORef runtime.modelRef
  runEffect runtime model effect

currentModel :: Runtime -> IO Model
currentModel runtime = readIORef runtime.modelRef

-- | Write a model and render it, with no message and no effect.
replaceModel :: Runtime -> Model -> IO ()
replaceModel runtime model = do
  writeIORef runtime.modelRef model
  runtime.shell.render model

-- | The number of threads started by effects that have not ended. A worker that 'stopWorkers' can
-- cancel and the watcher that waits for it count as one: the count goes up before the worker
-- starts and down when the watcher ends, after it has posted what the outcome needs. So a worker
-- that waits in 'post' counts 1, and @0@ means that no effect thread can post any more. Every other
-- effect thread counts 1 from before its start to its end. Threads that a thread starts on its own,
-- such as the job log writer or a plug-in session, are not counted.
--
-- Read it on the thread that calls 'dispatch', after 'dispatch' returns.
inFlight :: Runtime -> STM Int
inFlight runtime = readTVar runtime.threads

-- | Cancel the engine and planner workers.
stopWorkers :: Runtime -> IO ()
stopWorkers runtime = readIORef runtime.workers >>= mapConcurrently_ cancel

runEffect :: Runtime -> Model -> AppEffect -> IO ()
runEffect runtime model effect = toasting runtime $ case effect of
  OpenDialog kind toMessage -> runtime.shell.openDialog kind toMessage
  ComputePlan spec -> planWorker runtime wording spec
  StartJob plan -> startJob runtime wording plan
  CancelRunning jobId ->
    readIORef runtime.engine >>= \case
      Just (held, worker)
        | held == jobId -> void (counted runtime (cancel worker))
      _ -> pure ()
  LoadHistory jobId folder -> loadHistory runtime wording jobId folder
  WriteFile path text -> run runtime (FileSystem.writeTextAtomically path text)
  DestroyWindow -> runtime.shell.destroyWindow
  LoadCatalog -> void $ counted runtime (withMVar runtime.catalogLock (const (reloadCatalog runtime)))
  ApplyChange change -> changeWorker runtime change
  ShowFolder folder -> runtime.shell.showFolder folder
  ShowChrome chrome -> runtime.shell.showChrome chrome
  where
    wording = model.wording

-- | Start a thread that 'inFlight' counts until it ends. The body runs with the mask state of the
-- caller.
counted :: Runtime -> IO a -> IO (Async a)
counted runtime body = mask $ \restore -> do
  atomically (modifyTVar' runtime.threads (+ 1))
  async (restore body `finally` ended)
    `onException` ended
  where
    ended = atomically (modifyTVar' runtime.threads (subtract 1))

-- | Start a worker that 'stopWorkers' can cancel, and a watcher that waits for it. The two count
-- as one thread in 'inFlight', until the watcher has run @after@ with the outcome. @registered@
-- runs on the calling thread, before the watcher starts.
tracked
  :: Runtime
  -> (Async () -> IO ())
  -> (Either SomeException () -> IO ())
  -> IO ()
  -> IO (Async ())
tracked runtime registered after action = mask_ $ do
  atomically (modifyTVar' runtime.threads (+ 1))
  flip onException ended $ do
    worker <- asyncWithUnmask (\unmask -> unmask action)
    atomicModifyIORef' runtime.workers (\held -> (Set.insert worker held, ()))
    registered worker
    void $ asyncWithUnmask $ \unmask ->
      flip finally ended $ do
        outcome <- waitCatch worker
        atomicModifyIORef' runtime.workers (\held -> (Set.delete worker held, ()))
        unmask (after outcome)
    pure worker
  where
    ended = atomically (modifyTVar' runtime.threads (subtract 1))

toasting :: Runtime -> IO () -> IO ()
toasting runtime action = guarded action >>= either (toastFailure runtime) pure

toastFailure :: Runtime -> SomeException -> IO ()
toastFailure runtime err = runtime.shell.post (ShowToast (T.pack (displayException err)))

changeWorker :: Runtime -> PluginCatalog.Change -> IO ()
changeWorker runtime change =
  void $ counted runtime $ withMVar runtime.catalogLock $ \() -> do
    guarded (run runtime (Catalog.apply change)) >>= \case
      Left err -> toastFailure runtime err
      Right (Left problem) -> runtime.shell.post (ShowToast problem)
      Right (Right ()) -> pure ()
    reloadCatalog runtime

reloadCatalog :: Runtime -> IO ()
reloadCatalog runtime =
  guarded (run runtime Catalog.load) >>= \case
    Left err -> toastFailure runtime err
    Right catalog -> runtime.shell.post (CatalogLoaded catalog)

startJob :: Runtime -> Wording -> Plan -> IO ()
startJob runtime wording plan = do
  let spec = plan.spec
  host <- run runtime getHostName
  let sink event = runtime.shell.post (EngineEvent spec.id event)
  previous <- readIORef runtime.engine
  let registered worker = writeIORef runtime.engine (Just (spec.id, worker))
      after outcome = do
        case outcome of
          Left err
            | not (wasCancelled err) ->
                runtime.shell.post (EngineEvent spec.id (JobEvent.Failed (T.pack (displayException err))))
          _ -> pure ()
        atomicModifyIORef' runtime.engine (\held -> (clearWhen spec.id held, ()))
  void $ tracked runtime registered after $ do
    forM_ previous (waitCatch . snd)
    setup <- run runtime Setup.load
    run runtime $
      EventLog.with spec (renderPlanText spec plan) $ \logPath logLine -> do
        forM_ logPath (\logFile -> logFile & JobEvent.LogOpened & sink & liftIO)
        let reported report = do
              logLine (JobEvent.PluginReported report)
              unless (logOnly report) (liftIO (sink (JobEvent.PluginReported report)))
            emitted ev = logLine ev >> liftIO (sink ev)
        if Plan.blocked plan
          then Emit.runWith emitted (Plugins.runNone (Engine.execute host plan))
          else Session.with (pluginConfig wording setup.ready reported) Session.RunStage plan $ \session ->
            Plugins.runSession session (Emit.runWith (inject . emitted) (Engine.execute host plan))

loadHistory :: Runtime -> Wording -> JobId -> OsPath -> IO ()
loadHistory runtime wording jobId folder = void $ counted runtime $ do
  loaded <- run runtime (Store.readHistory folder)
  case loaded of
    Left e -> runtime.shell.post (ShowToast (display e))
    Right Nothing -> runtime.shell.post (ShowToast (noHistoryText wording folder))
    Right (Just hist) -> runtime.shell.post (HistoryLoaded jobId hist)

planWorker :: Runtime -> Wording -> JobSpec -> IO ()
planWorker runtime wording spec = do
  worker <- tracked runtime (\_ -> pure ()) (\_ -> pure ()) $ do
    attempt <- guarded $ do
      plan <- run runtime (Planner.plan spec)
      setup <- run runtime Setup.load
      forM_ setup.grantsProblem (runtime.shell.post . ShowToast)
      run runtime (Session.inspectPlan (pluginConfig wording setup.ready (logAttention_ . display)) plan)
    case attempt of
      Left err -> runtime.shell.post (PlanComputed spec (Left (T.pack (displayException err))))
      Right plan -> runtime.shell.post (PlanComputed spec (Right plan))
  previous <- atomicModifyIORef' runtime.planner (\held -> (Just (spec.id, worker), held))
  forM_ previous $ \(jid, old) -> when (jid == spec.id) (void (counted runtime (cancel old)))

run :: Runtime -> Eff AppStack a -> IO a
run Runtime {runner = Runner through} = through

pluginConfig :: Wording -> Vector Plugin.Ready -> (PluginReport -> Eff AppStack ()) -> Session.Config AppStack
pluginConfig wording plugins report =
  Session.Config
    { plugins
    , locale = languageCode wording.language
    , report
    }

logOnly :: PluginReport -> Bool
logOnly = \case
  PluginReport.Logged _ _ -> True
  _ -> False

clearWhen :: JobId -> Maybe (JobId, Async ()) -> Maybe (JobId, Async ())
clearWhen finished held = case held of
  Just (jobId, _) | jobId == finished -> Nothing
  _ -> held

wasCancelled :: SomeException -> Bool
wasCancelled err = case fromException err of
  Just AsyncCancelled -> True
  Nothing -> False
