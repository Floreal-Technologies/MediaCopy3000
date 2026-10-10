module MediaCopy.Plugin.Session
  ( Stage (..)
  , SessionConfig (..)
  , Session
  , withSession
  , planWithPlugins
  , fileVerified
  , finishInspections
  , logFault
  ) where

import Control.Concurrent.Async (Async, AsyncCancelled (..), asyncThreadId, asyncWithUnmask, cancel, forConcurrently, waitCatch, waitCatchSTM)
import Control.Concurrent.STM
import Control.Exception (displayException, finally, mask_, throwTo, uninterruptibleMask_)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON, ToJSON, Value (Null, String), toJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Bifunctor (first)
import Data.Either (lefts, partitionEithers)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (partition, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import MediaCopy.Plugin.Manifest
import MediaCopy.Plugin.Protocol qualified as P
import System.Timeout (timeout)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Guard (guarded)
import MediaCopy.Plugin.Discovery (Installed (..))
import MediaCopy.Plugin.Fields (fieldValues, mergeAuthorSettings)
import MediaCopy.Plugin.Grants (Ready (..))
import MediaCopy.Plugin.Process
import MediaCopy.Plugin.Trace (TraceTarget (..))
import MediaCopy.Plugin.Wire
import Paths_mediacopy3000 (version)

-- $setup
-- >>> import Data.Set qualified as Set

data Stage = PlanStage | RunStage
  deriving stock (Eq, Show)

data SessionConfig = SessionConfig
  { plugins :: Vector Ready
  , locale :: Text
  , report :: PluginReport -> IO ()
  }

data Session = Session
  { config :: SessionConfig
  , plan :: JobPlan
  , members :: Vector Member
  , startFaults :: Vector PluginFinding
  }

data Member = Member
  { ref :: PluginRef
  , ready :: Ready
  , mailbox :: TQueue Work
  , queued :: TVar Int
  , skippedFiles :: TVar Int
  , broken :: TVar (Maybe Text)
  , worker :: Async ()
  }

data Work
  = Request Text Value (TMVar (Either CallFault Value))
  | Inspect VerifiedFile
  | Stop (TMVar ())

silence :: Limit
silence = Silence 30

withSession :: SessionConfig -> Stage -> JobPlan -> (Session -> IO a) -> IO a
withSession config stage plan use = do
  let approved = Set.fromList (V.toList (V.map (.id) plan.plugins.active))
      chosen ready = stage == PlanStage || Set.member (pluginRef ready.installed).id approved
      (needed, idle) = partition (neededAt stage . (.granted)) (V.toList config.plugins)
      wanted = needed & sortOn (.installed.manifest.id) & filter chosen
      present = Set.fromList (map (\ready -> (pluginRef ready.installed).id) (V.toList config.plugins))
      vanished = V.filter (\ref -> not (Set.member ref.id present)) plan.plugins.active
      unneeded = if stage == PlanStage then idle else []
  when (stage == RunStage) $
    forM_ vanished (\ref -> config.report (Warned PluginFinding {plugin = ref, severity = Warning, about = Faulted (Unavailable "is no longer installed and enabled")}))
  registry <- newTVarIO []
  flip finally (stopWorkers registry) $ do
    (members, startFaults) <- startAll registry config stage plan wanted
    let fieldFaults = lefts (map (fieldCheck plan.spec.pluginFields stage) unneeded)
    result <- use Session {config, plan, members, startFaults = startFaults <> V.fromList fieldFaults}
    void (timeout 5_000_000 (forConcurrently members stopMember))
    pure result

stopWorkers :: TVar [Async ()] -> IO ()
stopWorkers registry = uninterruptibleMask_ $ do
  workers <- readTVarIO registry
  forM_ workers (\worker -> throwTo (asyncThreadId worker) AsyncCancelled)
  expired <- registerDelay 10_000_000
  atomically $ traverse_ waitCatchSTM workers `orElse` (readTVar expired >>= check)

-- |
-- >>> neededAt PlanStage (Set.fromList [FilesInspect])
-- True
-- >>> neededAt RunStage (Set.fromList [PlanInspect, ManifestWrite])
-- False
-- >>> neededAt PlanStage (Set.fromList [FilesRead, Block])
-- False
neededAt :: Stage -> Set Capability -> Bool
neededAt stage granted = case stage of
  PlanStage -> any (`elem` hooks) granted
  RunStage -> Set.member FilesInspect granted

-- |
-- >>> mayBlock (Set.fromList [Block])
-- False
-- >>> mayBlock (Set.fromList [Block, FilesInspect])
-- False
-- >>> mayBlock (Set.fromList [Block, PlanInspect])
-- True
mayBlock :: Set Capability -> Bool
mayBlock granted = Set.member Block granted && Set.member PlanInspect granted

-- |
-- >>> mustHold (Set.fromList [ManifestWrite])
-- True
-- >>> mustHold (Set.fromList [PlanInspect])
-- False
-- >>> mustHold (Set.fromList [Block, FilesInspect])
-- False
mustHold :: Set Capability -> Bool
mustHold granted = mayBlock granted || Set.member ManifestWrite granted

startAll :: TVar [Async ()] -> SessionConfig -> Stage -> JobPlan -> [Ready] -> IO (Vector Member, Vector PluginFinding)
startAll registry config stage plan wanted = do
  attempts <- forConcurrently wanted (startMember registry config stage plan)
  let (faults, members) = partitionEithers attempts
  when (stage == RunStage) (traverse_ (config.report . Warned) faults)
  pure (V.fromList members, V.fromList faults)

fieldCheck :: Map Text (Map Text Text) -> Stage -> Ready -> Either PluginFinding (Map Text Value, Map Text Value)
fieldCheck given stage ready = case fields of
  Left key -> Left (fault (FieldMissing key))
  Right values -> Right values
  where
    fault reason = PluginFinding {plugin = pluginRef ready.installed, severity = startSeverity stage ready, about = Faulted reason}
    manifest = ready.installed.manifest
    PluginId pluginId = manifest.id
    perJob = Map.findWithDefault Map.empty pluginId given
    fields = do
      stored <- fieldValues manifest.settings ready.settings
      jobFields <- fieldValues manifest.jobFields (Map.map String perJob)
      settings <- mergeAuthorSettings manifest.settings perJob stored
      Right (settings, jobFields)

startMember :: TVar [Async ()] -> SessionConfig -> Stage -> JobPlan -> Ready -> IO (Either PluginFinding Member)
startMember registry config stage plan ready = do
  let ref = pluginRef ready.installed
      fault about = PluginFinding {plugin = ref, severity = startSeverity stage ready, about = Faulted about}
  case fieldCheck plan.spec.pluginFields stage ready of
    Left finding -> pure (Left finding)
    Right (settings, jobFields) -> do
      mailbox <- newTQueueIO
      queued <- newTVarIO 0
      skippedFiles <- newTVarIO 0
      broken <- newTVarIO Nothing
      started <- newEmptyTMVarIO
      let params =
            P.InitializeParams
              { api = apiMajor
              , minor = apiMinor
              , app = T.pack (showVersion version)
              , locale = config.locale
              , job = (jobInfo plan).kind
              , settings
              , jobFields
              , grants = V.fromList (Set.toList ready.granted)
              , entitlement = Null
              }
          launch =
            Launch
              { executable = ready.installed.executable
              , folder = ready.installed.folder
              , onLog = config.report . Logged ref . oneLine
              , trace =
                  if ready.trace
                    then Just TraceTarget {pluginId = ref.id, stage = stageName stage}
                    else Nothing
              }
      worker <- mask_ $ do
        forked <- asyncWithUnmask (\unmask -> unmask (runWorker config launch params ref mailbox queued skippedFiles broken started))
        atomically (modifyTVar' registry (forked :))
        pure forked
      atomically (readTMVar started) >>= \case
        Left reason -> cancel worker >> pure (Left (fault (Unavailable reason)))
        Right () ->
          pure . Right $
            Member
              { ref
              , ready
              , mailbox
              , queued
              , skippedFiles
              , broken
              , worker
              }

stageName :: Stage -> Text
stageName = \case
  PlanStage -> "plan"
  RunStage -> "run"

startSeverity :: Stage -> Ready -> Severity
startSeverity stage ready
  | stage == RunStage = Warning
  | mustHold ready.granted = Blocker
  | otherwise = Warning

runWorker
  :: SessionConfig
  -> Launch
  -> P.InitializeParams
  -> PluginRef
  -> TQueue Work
  -> TVar Int
  -> TVar Int
  -> TVar (Maybe Text)
  -> TMVar (Either Text ())
  -> IO ()
runWorker config launch params ref mailbox queued skippedFiles broken started = attempt (1 :: Int)
  where
    attempt restartsLeft = do
      outcome <- guarded (withConnection launch connected)
      case outcome of
        Left e -> giveUp (T.pack (displayException e))
        Right (Left reason) -> giveUp reason
        Right (Right Nothing) -> pure ()
        Right (Right (Just (file, reason)))
          | restartsLeft > 0 -> do
              config.report (Logged ref ("restarting after a fault: " <> reason))
              atomically (unGetTQueue mailbox (Inspect file))
              attempt (restartsLeft - 1)
          | otherwise -> do
              atomically (unGetTQueue mailbox (Inspect file))
              giveUp reason
    connected conn =
      call conn (Fixed 10) P.methodInitialize params >>= \case
        Left fault -> pure (Left (faultText fault))
        Right (answer :: P.InitializeResult)
          | answer.api /= apiMajor -> pure (Left ("answered with plug-in API " <> T.show answer.api))
          | otherwise -> do
              void (atomically (tryPutTMVar started (Right ())))
              Right <$> serve conn
    serve conn =
      atomically (readTQueue mailbox) >>= \case
        Request method value box -> do
          answer <- call conn silence method value
          atomically (putTMVar box answer)
          serve conn
        Inspect file ->
          call conn silence P.methodInspectFile (inspectFileParams file) >>= \case
            Left fault -> pure (Just (file, faultText fault))
            Right result -> do
              config.report (Annotated file.path (annotationsOf ref result))
              forM_ result.warnings (config.report . Warned . pluginSaid ref False)
              atomically (modifyTVar' queued (subtract 1))
              serve conn
        Stop done -> do
          stopSoftly conn
          atomically (putTMVar done ())
          pure Nothing
    giveUp reason = do
      firstTime <- atomically (tryPutTMVar started (Left reason))
      unless firstTime $ do
        atomically (writeTVar broken (Just reason))
        config.report (Warned PluginFinding {plugin = ref, severity = Warning, about = Faulted (Unavailable reason)})
      drain reason
    drain reason =
      atomically (readTQueue mailbox) >>= \case
        Request _ _ box -> atomically (putTMVar box (Left (Unreachable reason))) >> drain reason
        Inspect _ -> atomically (modifyTVar' queued (subtract 1) >> modifyTVar' skippedFiles (+ 1)) >> drain reason
        Stop done -> atomically (putTMVar done ())

faultText :: CallFault -> Text
faultText = \case
  Unreachable reason -> reason
  Malformed reason -> reason

faultAbout :: CallFault -> PluginFault
faultAbout = \case
  Unreachable reason -> Unavailable reason
  Malformed reason -> BadOutput reason

request :: (ToJSON p, FromJSON r) => Member -> Text -> p -> IO (Either CallFault r)
request member method params = do
  box <- newEmptyTMVarIO
  atomically (writeTQueue member.mailbox (Request method (toJSON params) box))
  answer <- atomically (takeTMVar box)
  pure $
    answer >>= \value -> first (\problem -> Malformed ("the answer to " <> method <> " does not follow the protocol: " <> T.pack problem)) (parseEither parseJSON value)

stopMember :: Member -> IO ()
stopMember member = do
  done <- newEmptyTMVarIO
  atomically (writeTQueue member.mailbox (Stop done))
  void (waitCatch member.worker)

planHooks :: Session -> IO PluginPlan
planHooks session = do
  results <- forConcurrently (V.toList session.members) (memberPlan session)
  let findings = V.fromList (sortOn (.plugin.id) (V.toList (session.startFaults <> V.concat (map fst results))))
      faulted = Set.fromList [finding.plugin.id | finding <- V.toList findings, isFault finding]
      ready = map (\entry -> pluginRef entry.installed) (V.toList session.config.plugins)
  pure
    PluginPlan
      { active = V.fromList (sortOn (.id) (filter (\ref -> not (Set.member ref.id faulted)) ready))
      , findings
      , contributions = mergeContributions (V.fromList (mapMaybe snd results))
      }
  where
    isFault finding = case finding.about of
      Faulted _ -> True
      Said _ -> False

memberPlan :: Session -> Member -> IO (Vector PluginFinding, Maybe Contributions)
memberPlan session member = do
  let job = jobInfo session.plan
      files = fileInfos session.plan
      granted = member.ready.granted
      fault severity callFault = V.singleton PluginFinding {plugin = member.ref, severity, about = Faulted (faultAbout callFault)}
  inspected <-
    if Set.member PlanInspect granted
      then do
        answer <- request member P.methodInspectPlan P.InspectPlanParams {job, files, readBudgetBytes = 1_048_576}
        pure $ case answer of
          Left callFault -> Left (fault (if mustHold granted then Blocker else Warning) callFault)
          Right (result :: P.InspectPlanResult) -> Right (V.map (pluginSaid member.ref (mayBlock granted)) result.findings)
      else pure (Right V.empty)
  case inspected of
    Left findings -> pure (findings, Nothing)
    Right findings
      | Set.member ManifestWrite granted -> do
          contributed <- request member P.methodContribute P.ContributeParams {job, files}
          let namespace = fromMaybe "" member.ready.installed.manifest.namespace
              jobFiles = Set.fromList (V.toList (V.map (.path) session.plan.steps))
          pure $ case contributed of
            Left callFault -> (findings <> fault Blocker callFault, Nothing)
            Right result -> case contributionOf member.ref namespace jobFiles result of
              Left problem -> (findings <> fault Blocker (Malformed problem), Nothing)
              Right contribution -> (findings, Just contribution)
      | otherwise -> pure (findings, Nothing)

fileVerified :: Session -> VerifiedFile -> IO ()
fileVerified session file =
  forM_ (inspectors session) $ \member -> atomically $ do
    readTVar member.broken >>= \case
      Just _ -> modifyTVar' member.skippedFiles (+ 1)
      Nothing -> modifyTVar' member.queued (+ 1) >> writeTQueue member.mailbox (Inspect file)

queuedFiles :: Session -> STM Int
queuedFiles session = sum <$> traverse (\member -> readTVar member.queued) (V.toList (inspectors session))

inspectors :: Session -> Vector Member
inspectors session = V.filter (\member -> Set.member FilesInspect member.ready.granted) session.members

finishInspections :: Session -> IO ()
finishInspections session = do
  left <- atomically (queuedFiles session)
  when (left > 0) (loop left)
  forM_ (inspectors session) $ \member -> do
    skipped <- readTVarIO member.skippedFiles
    when (skipped > 0) (session.config.report (NotInspected member.ref skipped))
  where
    loop shown = do
      session.config.report (InspectionsLeft shown)
      next <- atomically $ do
        now <- queuedFiles session
        when (now == shown) retry
        pure now
      if next <= 0 then session.config.report (InspectionsLeft 0) else loop next

logFault :: Session -> Text -> IO ()
logFault session problem = session.config.report (Warned PluginFinding {plugin = PluginRef {id = "mediacopy3000", name = "MediaCopy 3000"}, severity = Warning, about = Faulted (Unavailable problem)})

planWithPlugins :: SessionConfig -> JobPlan -> IO JobPlan
planWithPlugins config plan = do
  pluginPlan <- withSession config PlanStage plan planHooks
  pure (withPluginPlan pluginPlan plan)
