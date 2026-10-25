{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Drive whole user flows through 'MediaCopy.Runtime': the same loop, effect wiring, planner and
-- engine as the window, over an in-memory disk and fake plug-ins.
--
-- The driver is the loop thread. It takes one message at a time from its queue and calls
-- 'dispatch'. A worker that posts waits until the driver lets it go on, so the engine moves in
-- lockstep with the loop, and a 'During' act runs at an exact event of a job.
module MediaCopy.Test.Harness
  ( Rig (..)
  , FakeInstall (..)
  , Act (..)
  , Settled (..)
  , rig
  , drive
  , expectClean
  , onStatus
  , user
  ) where

import Ascmhl.OsPath qualified as OsPath
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception (onException)
import Data.Aeson (Result (..), Value, encode, fromJSON, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Manifest qualified as Manifest
import MediaCopy.Plugin.Manifest qualified as Plugin (Manifest)
import System.OsPath (normalise, unsafeEncodeUtf, (</>))
import splice System.OsPath (osp)
import System.OsString (isPrefixOf)
import System.Timeout (timeout)
import Test.Tasty.HUnit (Assertion, (@?=))

import MediaCopy.Demo.Fixtures (at)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Interface.Theme (Accent (Blue), PaletteMode (LightPalette))
import MediaCopy.Model (JobEntry (..), Message (..), Model (..), UiMessage, initialModel, Notification (..))
import MediaCopy.Runtime (Runtime, Shell (..), currentModel, dispatch, inFlight, newRuntime, stopWorkers)
import MediaCopy.Test.FakeWorld (FakePlugin)
import MediaCopy.Test.FakeWorld qualified as FakeWorld
import MediaCopy.Test.InMemoryFS (MemFS)
import MediaCopy.Test.InMemoryFS qualified as InMemoryFS

-- | The world a flow starts in.
data Rig = Rig
  { disk :: MemFS
  , picks :: List OsPath
  -- ^ The answers to the file dialogs, first in first out. When none is left, the dialog is
  -- dismissed.
  , plugins :: List FakeInstall
  }

data FakeInstall = FakeInstall
  { manifest :: Value
  , grant :: Value
  , behaviour :: FakePlugin
  }

-- | One step of a flow.
data Act
  = -- | Post a message on the loop thread, then run until quiet.
    Send Message
  | -- | Change the disk between two acts.
    Alter (MemFS -> MemFS)
  | -- | Run the acts at the first event of the job that matches, before the engine goes on. Every
    -- 'During' of a list is armed when the list starts, whatever its place in the list, and fires
    -- once. A 'During' that never fires is a violation.
    --
    -- Do not send a message from a 'During' on a job's terminal event while another job is queued.
    -- The next job starts at that event, and its engine worker waits for the held one to end, so
    -- the loop never gets quiet and 'drive' fails at its time limit.
    During JobId (JobEvent -> Bool) (List Act)

-- | The world after a flow.
data Settled = Settled
  { model :: Model
  , disk :: MemFS
  , events :: List (JobId, JobEvent)
  -- ^ Every 'EngineEvent' the loop received, in order.
  , logFiles :: Map OsPath Text
  -- ^ The files in the job log folder of the disk, read after the effect threads have ended. They
  -- are not in 'disk'.
  , closed :: Bool
  -- ^ The window was destroyed.
  , chrome :: List Text
  -- ^ The 'showChrome' and 'showFolder' calls, in order.
  , models :: List Model
  -- ^ Every model the loop rendered, in order.
  , violations :: List Text
  -- ^ The invariants that a model broke.
  }

-- | An in-memory disk, no pick, no plug-in.
rig :: MemFS -> Rig
rig disk = Rig {disk, picks = [], plugins = []}

-- | A message from the queue, and the gate of the worker that waits on it.
data Hand = Hand
  { message :: Message
  , gate :: Maybe (TVar Bool)
  }

-- | A 'During' that has not fired yet.
data Armed = Armed
  { jobId :: JobId
  , matches :: JobEvent -> Bool
  , acts :: List Act
  }

data Harness = Harness
  { loopThread :: ThreadId
  , queue :: TQueue Hand
  , blocked :: TVar Int
  -- ^ The workers that wait in 'post' for their gate.
  , stopping :: TVar Bool
  , held :: IORef (List (TVar Bool))
  -- ^ The gates the driver did not open.
  , diskRef :: IORef MemFS
  , picksRef :: IORef (List OsPath)
  , armed :: IORef (List Armed)
  , current :: IORef (Maybe Message)
  , shown :: IORef Model
  , eventsRef :: IORef (List (JobId, JobEvent))
  , closedRef :: IORef Bool
  , chromeRef :: IORef (List Text)
  , modelsRef :: IORef (List Model)
  , notificationsRef :: IORef (List Notification)
  , violationsRef :: IORef (List Text)
  }

-- | Run the acts from the start model, then wait until the loop is quiet.
drive :: Rig -> List Act -> IO Settled
drive setup acts = do
  placed <- traverse place setup.plugins
  harness <- newHarness Rig {disk = setup.disk & installed placed, picks = setup.picks, plugins = setup.plugins}
  let world = FakeWorld.World {disk = harness.diskRef, plugins = Map.fromList (fmap (\plugin -> (plugin.executable, plugin.install.behaviour)) placed)}
  runtime <- newRuntime (testShell harness) (FakeWorld.fakeRunner world) startModel
  settled <-
    ( timeout 60_000_000 (play harness runtime acts) >>= \case
        Nothing -> ioError (userError "the loop did not get quiet within 60 s")
        Just () -> snapshot harness runtime
    )
      `finally` stop harness runtime
  final <- readIORef harness.diskRef
  pure settled {logFiles = readLogs final, disk = withoutXdg settled.disk}

startModel :: Model
startModel = initialModel at LightPalette Blue

newHarness :: Rig -> IO Harness
newHarness setup = do
  loopThread <- myThreadId
  queue <- newTQueueIO
  blocked <- newTVarIO 0
  stopping <- newTVarIO False
  held <- newIORef []
  diskRef <- newIORef setup.disk
  picksRef <- newIORef setup.picks
  armed <- newIORef []
  current <- newIORef Nothing
  shown <- newIORef startModel
  eventsRef <- newIORef []
  closedRef <- newIORef False
  chromeRef <- newIORef []
  modelsRef <- newIORef []
  violationsRef <- newIORef []
  notificationsRef <- newIORef []
  pure Harness {loopThread, queue, blocked, stopping, held, diskRef, picksRef, armed, current, shown, eventsRef, closedRef, chromeRef, modelsRef, violationsRef, notificationsRef}

testShell :: Harness -> Shell
testShell harness =
  Shell
    { render = checkInvariants harness
    , post = posted harness
    , openDialog = \_ answer -> picked harness answer
    , destroyWindow = writeIORef harness.closedRef True
    , showChrome = \chrome -> modifyIORef' harness.chromeRef (T.show chrome :)
    , showFolder = \folder -> modifyIORef' harness.chromeRef (("ShowFolder " <> folder) :)
    , notify = \notif -> modifyIORef' harness.notificationsRef (notif :)
    }

data Placed = Placed
  { pluginId :: Text
  , folder :: OsPath
  , executable :: OsPath
  , install :: FakeInstall
  }

place :: FakeInstall -> IO Placed
place install = case fromJSON install.manifest of
  Error problem -> ioError (userError ("a fake plug-in manifest: " <> problem))
  Success (manifest :: Plugin.Manifest) -> do
    let Id.Id pluginId = manifest.id
        folder = pluginsRoot </> unsafeEncodeUtf (T.unpack pluginId)
    relative <- maybe (ioError (userError ("a fake plug-in with no executable for " <> T.unpack Manifest.platformKey))) pure (Map.lookup Manifest.platformKey manifest.executable)
    pure Placed {pluginId, folder, executable = normalise (folder </> unsafeEncodeUtf relative), install}

installed :: List Placed -> MemFS -> MemFS
installed placed disk = case placed of
  [] -> disk
  _ -> foldr written disk placed & InMemoryFS.withFile [osp|/xdg/config/mediacopy3000/plugins.json|] (json (object ["plugins" .= object (fmap granted placed)]))
  where
    written plugin before =
      before
        & InMemoryFS.withFile (plugin.folder </> [osp|plugin.json|]) (json plugin.install.manifest)
        & InMemoryFS.withFile plugin.executable ""
    granted plugin = Key.fromText plugin.pluginId .= plugin.install.grant
    json value = LBS.toStrict (encode value)

pluginsRoot :: OsPath
pluginsRoot = [osp|/xdg/data/mediacopy3000/plugins|]

-- | On the loop thread, only queue the message. On a worker thread, queue it and wait until the
-- driver opens the gate. A cancel that reaches the worker while it waits closes its count.
posted :: Harness -> Message -> IO ()
posted harness message = do
  self <- myThreadId
  if self == harness.loopThread
    then atomically (writeTQueue harness.queue Hand {message, gate = Nothing})
    else do
      gate <- newTVarIO False
      atomically $
        readTVar harness.stopping >>= \case
          True -> writeTVar gate True
          False -> do
            writeTQueue harness.queue Hand {message, gate = Just gate}
            modifyTVar' harness.blocked (+ 1)
      atomically (readTVar gate >>= check) `onException` open harness gate

-- | Let a waiting worker go on. Opening a gate twice counts once.
open :: Harness -> TVar Bool -> IO ()
open harness gate =
  atomically $
    readTVar gate >>= \case
      True -> pure ()
      False -> writeTVar gate True >> modifyTVar' harness.blocked (subtract 1)

picked :: Harness -> (OsPath -> Message) -> IO ()
picked harness answer =
  atomicModifyIORef' harness.picksRef (\case [] -> ([], Nothing); next : rest -> (rest, Just next))
    >>= traverse_ (posted harness . answer)

play :: Harness -> Runtime -> List Act -> IO ()
play harness runtime acts = do
  modifyIORef' harness.armed (<> mapMaybe arming acts)
  forM_ acts $ \case
    Send message -> posted harness message >> settle harness runtime
    Alter change -> InMemoryFS.alter harness.diskRef change
    During {} -> pure ()
  where
    arming = \case
      During jobId matches later -> Just Armed {jobId, matches, acts = later}
      _ -> Nothing

-- | Dispatch the queued messages until the loop is quiet: the queue is empty, and every thread
-- the effects started waits in 'post'. A worker and its watcher count as one in 'inFlight', so a
-- worker held in 'post' counts once on each side. Threads that 'inFlight' does not count, such as the writer
-- of the job log, are not waited for.
settle :: Harness -> Runtime -> IO ()
settle harness runtime =
  atomically ((Just <$> readTQueue harness.queue) `orElse` (quiet $> Nothing))
    >>= traverse_ (\hand -> step harness runtime hand >> settle harness runtime)
  where
    quiet = do
      running <- inFlight runtime
      waiting <- readTVar harness.blocked
      check (running == waiting)

step :: Harness -> Runtime -> Hand -> IO ()
step harness runtime hand = do
  wasClosed <- readIORef harness.closedRef
  case hand.message of
    EngineEvent jobId event -> modifyIORef' harness.eventsRef ((jobId, event) :)
    _ -> pure ()
  writeIORef harness.current (Just hand.message)
  checked <- readIORef harness.violationsRef
  dispatch runtime hand.message
  -- The model of a confirmed close is rendered before the window is destroyed. It leaves the job
  -- it stopped in 'Running', and no window shows it any more, so it is not checked.
  closedNow <- readIORef harness.closedRef
  when (closedNow && not wasClosed) (writeIORef harness.violationsRef checked)
  case hand.message of
    EngineEvent jobId event -> fire harness runtime jobId event
    _ -> pure ()
  model <- currentModel runtime
  closed <- readIORef harness.closedRef
  let stays = closed || cancelledEvent model hand.message
  forM_ hand.gate $ \gate ->
    if stays then modifyIORef' harness.held (gate :) else open harness gate

-- | An event of a job that the model holds as cancelled. Its worker stays in 'post', so the
-- runtime's cancel reaches it at that event.
cancelledEvent :: Model -> Message -> Bool
cancelledEvent model = \case
  EngineEvent jobId _ -> fmap (\entry -> entry.state.phase) (Map.lookup jobId model.jobs) == Just JobState.Cancelled
  _ -> False

fire :: Harness -> Runtime -> JobId -> JobEvent -> IO ()
fire harness runtime jobId event = do
  (due, later) <- readIORef harness.armed <&> partition (\armed -> armed.jobId == jobId && armed.matches event)
  writeIORef harness.armed later
  forM_ due (\armed -> play harness runtime armed.acts)

snapshot :: Harness -> Runtime -> IO Settled
snapshot harness runtime = do
  model <- currentModel runtime
  disk <- readIORef harness.diskRef
  events <- readIORef harness.eventsRef <&> reverse
  closed <- readIORef harness.closedRef
  chrome <- readIORef harness.chromeRef <&> reverse
  models <- readIORef harness.modelsRef <&> reverse
  broken <- readIORef harness.violationsRef <&> reverse
  unfired <- readIORef harness.armed <&> fmap (\armed -> "a During on job " <> T.show armed.jobId <> " never fired")
  let violations = broken <> unfired
  pure Settled {model, disk, events, logFiles = Map.empty, closed, chrome, models, violations}

-- | Cancel the workers, let every waiting thread go, and wait until no counted thread is left.
stop :: Harness -> Runtime -> IO ()
stop harness runtime = do
  atomically (writeTVar harness.stopping True)
  stopWorkers runtime
  readIORef harness.held >>= traverse_ (open harness)
  atomically (flushTQueue harness.queue) >>= traverse_ (\hand -> traverse_ (open harness) hand.gate)
  timeout 60_000_000 (atomically (inFlight runtime >>= \running -> check (running == 0))) >>= \case
    Nothing -> ioError (userError "the effect threads did not end within 60 s of the stop")
    Just () -> pure ()

readLogs :: MemFS -> Map OsPath Text
readLogs disk =
  disk.files
    & Map.filterWithKey (\path _ -> isLog path)
    & Map.map (\(bytes, _) -> TE.decodeUtf8 bytes)

withoutXdg :: MemFS -> MemFS
withoutXdg disk = InMemoryFS.withFiles (Map.filterWithKey (\path _ -> not (isPrefixOf [osp|/xdg/|] path)) disk.files) disk

isLog :: OsPath -> Bool
isLog = isPrefixOf [osp|/xdg/state/mediacopy3000/jobs/|]

-- | Record the new model, and each invariant that it breaks against the one rendered before it,
-- while the window is open.
checkInvariants :: Harness -> Model -> IO ()
checkInvariants harness new = do
  modifyIORef' harness.modelsRef (new :)
  old <- readIORef harness.shown
  message <- readIORef harness.current
  closed <- readIORef harness.closedRef
  unless closed (modifyIORef' harness.violationsRef (reverse (breaches message old new) <>))
  writeIORef harness.shown new

breaches :: Maybe Message -> Model -> Model -> List Text
breaches message old new =
  wentBack <> unsettled <> miscounted <> runners
  where
    wentBack =
      Map.intersectionWith (,) old.jobs new.jobs
        & Map.toList
        & mapMaybe
          ( \(jobId, (before, after)) ->
              if after.state.bytesDone < before.state.bytesDone
                then Just (named jobId <> ": bytesDone went from " <> T.show before.state.bytesDone <> " to " <> T.show after.state.bytesDone)
                else Nothing
          )
    unsettled = case message of
      Just (EngineEvent jobId event)
        | JobEvent.isTerminal event
        , old.running == Just jobId ->
            case Map.lookup jobId new.jobs of
              Just entry
                | isNothing entry.state.doing && JobState.isTerminal entry.state.phase -> []
                | otherwise -> [named jobId <> ": not settled after " <> display event]
              Nothing -> [named jobId <> ": gone after " <> display event]
      _ -> []
    miscounted =
      Map.toList new.jobs
        & mapMaybe
          ( \(jobId, entry) ->
              let statuses = Map.elems entry.state.files <&> (.status)
                  counts = JobState.counts entry.state
                  countWhere keep = filter keep statuses & length
                  done = counts.verified + counts.failed + counts.missing + counts.new + counts.replaced
              in if done == countWhere FileStatus.isDone && counts.failed + counts.missing == countWhere FileStatus.isFailure
                   then Nothing
                   else Just (named jobId <> ": the counts do not partition the finished files")
          )
    runners =
      case Map.keys (Map.filter (\entry -> entry.state.phase == JobState.Running) new.jobs) of
        [] -> []
        [jobId] | new.running == Just jobId -> []
        jobIds -> ["running jobs " <> T.show jobIds <> " but model.running is " <> T.show new.running]
    named jobId = "job " <> T.show jobId

-- | No invariant was broken, and no part copy is left on the disk.
expectClean :: Settled -> Assertion
expectClean settled = do
  settled.violations @?= []
  filter (T.isInfixOf ".mc3k-part" . OsPath.toText) (Map.keys settled.disk.files) @?= []

-- | An event that sets a file to this status.
onStatus :: FileStatus -> JobEvent -> Bool
onStatus wanted = \case
  JobEvent.FileStatusChanged _ status -> status == wanted
  _ -> False

user :: UiMessage -> Act
user = Send . Ui
