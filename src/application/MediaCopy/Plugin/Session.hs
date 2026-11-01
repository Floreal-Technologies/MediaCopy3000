module MediaCopy.Plugin.Session
  ( Session
  , Config (..)
  , Stage (..)
  , Crash (..)
  , with
  , inspectPlan
  , inspect
  , finish
  , logFault
  ) where

import Control.Exception (Exception (..))
import Data.Aeson (FromJSON, ToJSON, Value (Null, String), toJSON)
import Data.Either (lefts)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Version (showVersion)
import Effectful (Persistence (Persistent), UnliftStrategy (ConcUnlift), withUnliftStrategy)
import Effectful qualified
import Effectful.Concurrent (throwTo)
import Effectful.Concurrent.Async (Async, AsyncCancelled (..), asyncThreadId, asyncWithUnmask, cancel, forConcurrently, forConcurrently_, waitCatch, waitCatchSTM)
import Effectful.Concurrent.STM
import Effectful.Exception qualified as E
import Effectful.Timeout (timeout)
import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Manifest qualified as Manifest
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.Initialise qualified as Initialise
import MediaCopy.Plugin.Protocol.InspectFile qualified as InspectFile
import MediaCopy.Plugin.Protocol.InspectPlan qualified as InspectPlan
import MediaCopy.Plugin.Protocol.JobFinished qualified as JobFinished
import MediaCopy.Plugin.Protocol.JobInfo qualified as JobInfo
import MediaCopy.Plugin.Protocol.Method qualified as Method

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.JobEnd (JobEnd)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginPlan (PluginPlan)
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginReport (PluginReport)
import MediaCopy.Domain.PluginReport qualified as PluginReport
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Domain.Severity (Severity)
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.VerifiedFile (VerifiedFile)
import MediaCopy.Domain.VerifiedFile qualified as VerifiedFile
import MediaCopy.Plugin.Connection qualified as Connection
import MediaCopy.Plugin.Fields qualified as Fields
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Ready qualified as Plugin (Ready)
import MediaCopy.Plugin.Ready qualified as Ready
import MediaCopy.Plugin.Tracer qualified as Tracer
import MediaCopy.Plugin.Wire qualified as Wire
import Paths_mediacopy3000 (version)

data Stage = PlanStage | RunStage
  deriving stock (Eq, Show)

data Config es = Config
  { plugins :: Vector Plugin.Ready
  , locale :: Text
  , report :: PluginReport -> Eff es ()
  }

data Session es = Session
  { config :: Config es
  , plan :: Plan
  , members :: Vector Member
  , startFaults :: Vector PluginFinding
  , crashes :: List Crash
  }

data Crash
  = NotStarted PluginRef Text
  | Crashed PluginRef Text
  deriving stock (Show)

instance Exception Crash where
  displayException = \case
    NotStarted ref reason -> "the plug-in " <> T.unpack ref.name <> " did not start: " <> T.unpack reason
    Crashed ref reason -> "the plug-in " <> T.unpack ref.name <> " stopped while it planned the job: " <> T.unpack reason

data Member = Member
  { ref :: PluginRef
  , ready :: Plugin.Ready
  , mailbox :: TQueue Work
  , queued :: TVar Int
  , skippedFiles :: TVar Int
  , broken :: TVar (Maybe Text)
  , worker :: Async ()
  }

data Work
  = Request Text Value (TMVar (Either Connection.CallFault Value))
  | Inspect VerifiedFile
  | Stop (TMVar ())

silence :: Connection.Limit
silence = Connection.Silence 30

with :: (Connection.Host es) => Config es -> Stage -> Plan -> (Session es -> Eff es a) -> Eff es a
with config stage plan use = withUnliftStrategy (ConcUnlift Persistent Effectful.Unlimited) $ do
  let approved = Set.fromList (V.toList (V.map (.id) plan.plugins.active))
      chosen ready = stage == PlanStage || Set.member (Wire.pluginRef ready.installed).id approved
      (needed, idle) = partition (neededAt stage . (.granted)) (V.toList config.plugins)
      wanted = needed & sortOn (.installed.manifest.id) & filter chosen
      present = Set.fromList (map (\ready -> (Wire.pluginRef ready.installed).id) (V.toList config.plugins))
      vanished = V.filter (\ref -> not (Set.member ref.id present)) plan.plugins.active
      unneeded = if stage == PlanStage then idle else []
  when (stage == RunStage) $
    forM_ vanished (\ref -> config.report (faultWarning ref (PluginFinding.Unavailable "is no longer installed and enabled")))
  registry <- newTVarIO []
  flip E.finally (stopWorkers registry) $ do
    (members, startFaults, crashes) <- startAll registry config stage plan wanted
    let fieldFaults = lefts (map (fieldCheck plan.spec.pluginFields stage) unneeded)
    result <- use Session {config, plan, members, startFaults = startFaults <> V.fromList fieldFaults, crashes}
    void (timeout 5_000_000 (forConcurrently members stopMember))
    pure result

stopWorkers :: (Concurrent :> es) => TVar [Async ()] -> Eff es ()
stopWorkers registry = E.uninterruptibleMask_ $ do
  workers <- readTVarIO registry
  forM_ workers (\worker -> throwTo (asyncThreadId worker) AsyncCancelled)
  expired <- registerDelay 10_000_000
  atomically $ traverse_ waitCatchSTM workers `orElse` (readTVar expired >>= check)

neededAt :: Stage -> Set Plugin.Capability -> Bool
neededAt stage = any (elem stage . stagesOf)

stagesOf :: Plugin.Capability -> List Stage
stagesOf = \case
  Capability.FilesRead -> []
  Capability.PlanInspect -> [PlanStage]
  Capability.FilesInspect -> [PlanStage, RunStage]
  Capability.Block -> []
  Capability.ManifestWrite -> [PlanStage]
  Capability.JobFinished -> [RunStage]

mayBlock :: Set Plugin.Capability -> Bool
mayBlock granted = Set.member Capability.Block granted && Set.member Capability.PlanInspect granted

mustHold :: Set Plugin.Capability -> Bool
mustHold granted = mayBlock granted || Set.member Capability.ManifestWrite granted

startAll
  :: (Connection.Host es)
  => TVar [Async ()]
  -> Config es
  -> Stage
  -> Plan
  -> [Plugin.Ready]
  -> Eff es (Vector Member, Vector PluginFinding, List Crash)
startAll registry config stage plan wanted = do
  attempts <- forConcurrently wanted (startMember registry config stage plan)
  let (failures, members) = partitionEithers attempts
      faults = map fst failures
      crashes = [NotStarted finding.plugin reason | (finding, Just (Connection.Ended reason)) <- failures]
  when (stage == RunStage) (traverse_ (config.report . PluginReport.Warned) faults)
  pure (V.fromList members, V.fromList faults, crashes)

fieldCheck :: Map Text (Map Text Text) -> Stage -> Plugin.Ready -> Either PluginFinding (Map Text Value, Map Text Value)
fieldCheck given stage ready = case fields of
  Left key -> Left (fault (PluginFinding.FieldMissing key))
  Right values -> Right values
  where
    fault reason = PluginFinding.PluginFinding {plugin = Wire.pluginRef ready.installed, severity = startSeverity stage ready, about = PluginFinding.Faulted reason}
    manifest = ready.installed.manifest
    Id.Id pluginId = manifest.id
    perJob = Map.findWithDefault Map.empty pluginId given
    fields = do
      stored <- Fields.values manifest.settings ready.settings
      jobFields <- Fields.values manifest.jobFields (Map.map String perJob)
      settings <- Fields.mergeAuthorSettings manifest.settings perJob stored
      Right (settings, jobFields)

startMember
  :: (Connection.Host es)
  => TVar [Async ()]
  -> Config es
  -> Stage
  -> Plan
  -> Plugin.Ready
  -> Eff es (Either (PluginFinding, Maybe Connection.CallFault) Member)
startMember registry config stage plan ready = do
  let ref = Wire.pluginRef ready.installed
      fault about = PluginFinding.PluginFinding {plugin = ref, severity = startSeverity stage ready, about = PluginFinding.Faulted about}
  case fieldCheck plan.spec.pluginFields stage ready of
    Left finding -> pure (Left (finding, Nothing))
    Right (settings, jobFields) -> do
      mailbox <- newTQueueIO
      queued <- newTVarIO 0
      skippedFiles <- newTVarIO 0
      broken <- newTVarIO Nothing
      started <- newEmptyTMVarIO
      let params =
            Initialise.Params
              { api = Api.major
              , minor = Api.minor
              , app = T.pack (showVersion version)
              , locale = config.locale
              , job = (Wire.jobInfo plan).kind
              , settings
              , jobFields
              , grants = V.fromList (Set.toList ready.granted)
              , entitlement = Null
              }
          launch =
            Connection.Launch
              { executable = ready.installed.executable
              , folder = ready.installed.folder
              , onLog = \line -> line & Prose.oneLine & PluginReport.Logged ref & config.report
              , trace =
                  if ready.trace
                    then Just Tracer.Target {pluginId = ref.id, stage = stageName stage}
                    else Nothing
              }
      worker <- E.mask_ $ do
        forked <- asyncWithUnmask (\unmask -> unmask (runWorker config launch params ref mailbox queued skippedFiles broken started))
        atomically (modifyTVar' registry (forked :))
        pure forked
      atomically (readTMVar started) >>= \case
        Left callFault -> cancel worker >> pure (Left (fault (PluginFinding.Unavailable (faultText callFault)), Just callFault))
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

startSeverity :: Stage -> Plugin.Ready -> Severity
startSeverity stage ready
  | stage == RunStage = Severity.Warning
  | mustHold ready.granted = Severity.Blocker
  | otherwise = Severity.Warning

runWorker
  :: (Connection.Host es)
  => Config es
  -> Connection.Launch es
  -> Initialise.Params
  -> PluginRef
  -> TQueue Work
  -> TVar Int
  -> TVar Int
  -> TVar (Maybe Text)
  -> TMVar (Either Connection.CallFault ())
  -> Eff es ()
runWorker config launch params ref mailbox queued skippedFiles broken started = attempt (1 :: Int)
  where
    attempt restartsLeft = do
      outcome <- E.trySync (Connection.with launch connected)
      case outcome of
        Left e -> giveUp (Connection.Ended (T.pack (displayException e)))
        Right (Left callFault) -> giveUp callFault
        Right (Right Nothing) -> pure ()
        Right (Right (Just (file, callFault)))
          | restartsLeft > 0 -> do
              config.report (PluginReport.Logged ref ("restarting after a fault: " <> faultText callFault))
              atomically (unGetTQueue mailbox (Inspect file))
              attempt (restartsLeft - 1)
          | otherwise -> do
              atomically (unGetTQueue mailbox (Inspect file))
              giveUp callFault
    connected conn =
      Connection.call conn (Connection.Fixed 10) Method.initialise params >>= \case
        Left callFault -> pure (Left callFault)
        Right (answer :: Initialise.Result)
          | answer.api /= Api.major -> pure (Left (Connection.Refused ("answered with plug-in API " <> T.show answer.api)))
          | otherwise -> do
              void (atomically (tryPutTMVar started (Right ())))
              Right <$> serve conn
    serve conn =
      atomically (readTQueue mailbox) >>= \case
        Request method value box -> do
          answer <- Connection.call conn silence method value
          atomically (putTMVar box answer)
          serve conn
        Inspect file ->
          Connection.call conn silence Method.inspectFile (Wire.inspectFileParams file) >>= \case
            Left callFault -> pure (Just (file, callFault))
            Right result -> do
              config.report (PluginReport.Annotated file.path (Wire.annotationsOf ref result))
              reportSaid config.report ref result.warnings
              atomically (modifyTVar' queued (subtract 1))
              serve conn
        Stop done -> do
          Connection.stopSoftly conn
          atomically (putTMVar done ())
          pure Nothing
    giveUp callFault = do
      firstTime <- atomically (tryPutTMVar started (Left callFault))
      unless firstTime $ do
        atomically (writeTVar broken (Just (faultText callFault)))
        config.report (faultWarning ref (PluginFinding.Unavailable (faultText callFault)))
      drain callFault
    drain callFault =
      atomically (readTQueue mailbox) >>= \case
        Request _ _ box -> atomically (putTMVar box (Left callFault)) >> drain callFault
        Inspect _ -> atomically (modifyTVar' queued (subtract 1) >> modifyTVar' skippedFiles (+ 1)) >> drain callFault
        Stop done -> atomically (putTMVar done ())

faultText :: Connection.CallFault -> Text
faultText = \case
  Connection.Ended reason -> reason
  Connection.TimedOut reason -> reason
  Connection.Refused reason -> reason
  Connection.Malformed reason -> reason

faultAbout :: Connection.CallFault -> PluginFinding.Fault
faultAbout = \case
  Connection.Ended reason -> PluginFinding.Unavailable reason
  Connection.TimedOut reason -> PluginFinding.Unavailable reason
  Connection.Refused reason -> PluginFinding.BadOutput reason
  Connection.Malformed reason -> PluginFinding.BadOutput reason

request :: (Concurrent :> es, ToJSON p, FromJSON r) => Member -> Text -> p -> Eff es (Either Connection.CallFault r)
request member method params = do
  box <- newEmptyTMVarIO
  atomically (writeTQueue member.mailbox (Request method (toJSON params) box))
  answer <- atomically (takeTMVar box)
  pure (answer >>= Connection.decodeAnswer method)

planRequest :: (Concurrent :> es, ToJSON p, FromJSON r) => Member -> Text -> p -> Eff es (Either Connection.CallFault r)
planRequest member method params =
  request member method params >>= \case
    Left (Connection.Ended reason) -> E.throwIO (Crashed member.ref reason)
    answer -> pure answer

stopMember :: (Concurrent :> es) => Member -> Eff es ()
stopMember member = do
  done <- newEmptyTMVarIO
  atomically (writeTQueue member.mailbox (Stop done))
  void (waitCatch member.worker)

planHooks :: (Connection.Host es) => Session es -> Eff es PluginPlan
planHooks session = do
  for_ (listToMaybe session.crashes) E.throwIO
  results <- forConcurrently (V.toList session.members) (memberPlan session)
  let findings = V.fromList (sortOn (.plugin.id) (V.toList (session.startFaults <> V.concat (map fst results))))
      faulted = Set.fromList [finding.plugin.id | finding <- V.toList findings, isFault finding]
      ready = map (\entry -> Wire.pluginRef entry.installed) (V.toList session.config.plugins)
  pure
    PluginPlan.PluginPlan
      { active = V.fromList (sortOn (.id) (filter (\ref -> not (Set.member ref.id faulted)) ready))
      , findings
      , contributions = Contributions.merge (V.fromList (mapMaybe snd results))
      }
  where
    isFault finding = case finding.about of
      PluginFinding.Faulted _ -> True
      PluginFinding.Said _ -> False

memberPlan :: (Concurrent :> es) => Session es -> Member -> Eff es (Vector PluginFinding, Maybe Contributions)
memberPlan session member = do
  let job = Wire.jobInfo session.plan
      files = Wire.fileInfos session.plan
      granted = member.ready.granted
      fault severity callFault = V.singleton PluginFinding.PluginFinding {plugin = member.ref, severity, about = PluginFinding.Faulted (faultAbout callFault)}
  inspected <-
    if Set.member Capability.PlanInspect granted
      then do
        answer <- planRequest member Method.inspectPlan InspectPlan.Params {job, files, readBudgetBytes = 1_048_576}
        pure $ case answer of
          Left callFault -> Left (fault (if mustHold granted then Severity.Blocker else Severity.Warning) callFault)
          Right (result :: InspectPlan.Result) -> Right (V.map (Wire.pluginSaid member.ref (mayBlock granted)) result.findings)
      else pure (Right V.empty)
  case inspected of
    Left findings -> pure (findings, Nothing)
    Right findings
      | Set.member Capability.ManifestWrite granted -> do
          contributed <- planRequest member Method.contribute Contribute.Params {job, files}
          let namespace = fromMaybe "" member.ready.installed.manifest.namespace
              jobFiles = Set.fromList (V.toList (V.map (.path) session.plan.steps))
          pure $ case contributed of
            Left callFault -> (findings <> fault Severity.Blocker callFault, Nothing)
            Right result -> case Wire.contributionOf member.ref namespace jobFiles result of
              Left problem -> (findings <> fault Severity.Blocker (Connection.Malformed problem), Nothing)
              Right contribution -> (findings, Just contribution)
      | otherwise -> pure (findings, Nothing)

inspect :: (Connection.Host es) => Session es -> VerifiedFile -> Eff es ()
inspect session file =
  forM_ (inspectors session) $ \member -> atomically $ do
    readTVar member.broken >>= \case
      Just _ -> modifyTVar' member.skippedFiles (+ 1)
      Nothing -> modifyTVar' member.queued (+ 1) >> writeTQueue member.mailbox (Inspect file)

queuedFiles :: Session es -> STM Int
queuedFiles session = sum <$> traverse (\member -> readTVar member.queued) (V.toList (inspectors session))

inspectors :: Session es -> Vector Member
inspectors = holding Capability.FilesInspect

holding :: Plugin.Capability -> Session es -> Vector Member
holding capability session = V.filter (\member -> Set.member capability member.ready.granted) session.members

finish :: (Connection.Host es) => Session es -> JobEnd -> Eff es ()
finish session jobEnd = do
  left <- atomically (queuedFiles session)
  when (left > 0) (loop left)
  forM_ (inspectors session) $ \member -> do
    skipped <- readTVarIO member.skippedFiles
    when (skipped > 0) (session.config.report (PluginReport.NotInspected member.ref skipped))
  let params = Wire.jobFinishedParams session.plan jobEnd
  forConcurrently_ (holding Capability.JobFinished session) (jobFinished session params)
  where
    loop shown = do
      session.config.report (PluginReport.InspectionsLeft shown)
      next <- atomically $ do
        now <- queuedFiles session
        when (now == shown) retry
        pure now
      if next <= 0 then session.config.report (PluginReport.InspectionsLeft 0) else loop next

jobFinished :: (Concurrent :> es) => Session es -> JobFinished.Params -> Member -> Eff es ()
jobFinished session params member =
  readTVarIO member.broken >>= \case
    Just _ -> pure ()
    Nothing ->
      request member Method.jobFinished params >>= \case
        Left callFault -> session.config.report (faultWarning member.ref (faultAbout callFault))
        Right (result :: JobFinished.Result) -> reportSaid session.config.report member.ref result.warnings

faultWarning :: PluginRef -> PluginFinding.Fault -> PluginReport
faultWarning plugin fault = PluginReport.Warned PluginFinding.PluginFinding {plugin, severity = Severity.Warning, about = PluginFinding.Faulted fault}

reportSaid :: (PluginReport -> Eff es ()) -> PluginRef -> Vector Protocol.Finding -> Eff es ()
reportSaid report ref = traverse_ (\warning -> warning & Wire.pluginSaid ref False & PluginReport.Warned & report)

logFault :: Session es -> Text -> Eff es ()
logFault session problem = session.config.report (faultWarning PluginRef.PluginRef {id = "mediacopy3000", name = "MediaCopy 3000"} (PluginFinding.Unavailable problem))

inspectPlan :: (Connection.Host es) => Config es -> Plan -> Eff es Plan
inspectPlan config plan = do
  pluginPlan <- with config PlanStage plan planHooks
  pure (Plan.withPlugins pluginPlan plan)
