module MediaCopy.Engine
  ( execute
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Creator (Creator)
import Ascmhl.Creator qualified as Creator
import Ascmhl.FileEntry (FileEntry)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Data.Version (showVersion)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Exception (displayException, finally, throwIO, trySync)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Effectful.Time (Time, currentTime)
import Prelude hiding (finally, throwIO)

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.FileOutcome (FileOutcome)
import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobResult (JobResult)
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Domain.PlannedWrite (PlannedWrite)
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.VerifiedFile qualified as VerifiedFile
import MediaCopy.Effects.Emit (Emit)
import MediaCopy.Effects.Emit qualified as Emit
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Effects.Hasher (Hasher)
import MediaCopy.Effects.Hasher qualified as Hasher
import MediaCopy.Effects.Plugins (Plugins)
import MediaCopy.Effects.Plugins qualified as Plugins
import MediaCopy.Engine.Generation qualified as Generation
import MediaCopy.Engine.PlanViolation (PlanViolation)
import MediaCopy.Engine.PlanViolation qualified as PlanViolation
import MediaCopy.Mhl.Store qualified as Store
import Paths_mediacopy3000 (version)

type Hashing es =
  ( FileSystem :> es
  , Hasher :> es
  , Time :> es
  , Emit :> es
  , State JobProgress :> es
  , Reader Algo :> es
  )

type Copying es = (Hashing es, Reader Creator :> es, Reader Contributions :> es, Plugins :> es)

type Pass es = (Copying es, Error PlanViolation :> es)

data JobProgress = JobProgress
  { readSoFar :: Int64
  , lastEmit :: UTCTime
  }

data PassTally = PassTally
  { entries :: Vector FileEntry
  , failedPaths :: Set RelPath
  }

data FileStepResult = FileStepResult
  { outcome :: FileOutcome
  , manifestEntry :: Maybe FileEntry
  }

data SealStopped = SealStopped
  { failed :: Int
  , total :: Int
  }
  deriving stock (Eq, Show)

instance Display SealStopped where
  displayBuilder stopped =
    "the seal failed for "
      <> displayBuilder (T.show stopped.failed)
      <> " of "
      <> displayBuilder (T.show stopped.total)
      <> " files; nothing was copied"

execute
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es, Plugins :> es)
  => Text
  -> Plan
  -> Eff es ()
execute hostname plan
  | Plan.blocked plan =
      V.toList (V.map (\finding -> display finding.code <> ": " <> finding.detail) (Plan.blockers plan))
        <> V.toList (V.map display (PluginPlan.blockers plan.plugins))
          & T.intercalate "; "
          & JobEvent.Failed
          & Emit.event
  | otherwise = do
      startTime <- currentTime
      result <- trySync (runErrorNoCallStack @PlanViolation (evalState (start startTime) (runPlan hostname plan)))
      endJob $ case result of
        Left e -> JobEvent.Failed (T.pack (displayException e))
        Right (Left violation) -> JobEvent.Failed (display violation)
        Right (Right terminal) -> terminal
  where
    start t = JobProgress {readSoFar = 0, lastEmit = t}

endJob :: (Emit :> es, Plugins :> es) => JobEvent -> Eff es ()
endJob terminal = do
  Plugins.settle
  trySync (Emit.event terminal) >>= \case
    Left e -> Emit.event (JobEvent.Failed (T.pack (displayException e)))
    Right () -> pure ()

advanceProgress :: (Time :> es, Emit :> es, State JobProgress :> es) => Int64 -> Eff es ()
advanceProgress n = do
  modify (\st -> st {readSoFar = st.readSoFar + n})
  st <- get @JobProgress
  t <- currentTime
  when (diffUTCTime t st.lastEmit >= 0.1) $ do
    modify (\s -> s {lastEmit = t})
    Emit.event (JobEvent.Progress st.readSoFar)

flushProgress :: (Emit :> es, State JobProgress :> es) => Eff es ()
flushProgress = do
  st <- get @JobProgress
  Emit.event (JobEvent.Progress st.readSoFar)

runFileStep
  :: (Emit :> es, State JobProgress :> es)
  => PlanStep
  -> Eff es (Maybe FileEntry)
  -> Eff es FileStepResult
  -> Eff es FileStepResult
runFileStep step onFailure body = do
  before <- get @JobProgress
  attempt <- trySync body
  case attempt of
    Left e -> do
      let outcome = FileOutcome.IoError (T.pack (displayException e))
      manifestEntry <- trySync onFailure <&> fromRight Nothing
      Emit.event (JobEvent.FileStatusChanged step.path (FileStatus.Done outcome))
      modify (\st -> st {readSoFar = before.readSoFar + PlanStep.bytesToRead step})
      flushProgress
      pure FileStepResult {outcome, manifestEntry}
    Right result -> do
      Emit.event (JobEvent.FileStatusChanged step.path (FileStatus.Done result.outcome))
      pure result

hashVia
  :: (Hasher :> es, Reader Algo :> es)
  => ((ByteString -> Eff es ()) -> Eff es UTCTime)
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashVia readWith onChunk = do
  fmt <- ask @Algo
  Hasher.hashing fmt (\put -> readWith (\bs -> put bs >> onChunk bs))

hashOf
  :: (FileSystem :> es, Hasher :> es, Reader Algo :> es)
  => FileSystem.ReadCache
  -> OsPath
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashOf mode path = hashVia (FileSystem.streamFile mode path)

copyAndVerify
  :: (Copying es)
  => OsPath
  -> Vector PlannedWrite
  -> Maybe Hash
  -> RelPath
  -> FileSize
  -> Eff es FileStepResult
copyAndVerify source writes sealed rel size = step `finally` void (trySync (FileSystem.removeTemps writes))
  where
    step = do
      let srcPath = RelPath.toOsPath source rel
          (reusing, writing) = V.partition (\w -> w.mode == PlannedWrite.Reuse) writes
      (srcHash, mtime) <- sourceHashFor srcPath writing sealed (Emit.event (JobEvent.FileStatusChanged rel FileStatus.Flushing))
      flushProgress
      written <- publishCopy writing sealed rel size mtime srcHash
      case written.outcome of
        FileOutcome.Ok -> reuseOrReplace srcPath reusing rel mtime srcHash written
        _ -> pure written

failedCopyEntry :: (Copying es) => OsPath -> Maybe Hash -> RelPath -> FileSize -> Eff es (Maybe FileEntry)
failedCopyEntry source sealed rel size = do
  let srcPath = RelPath.toOsPath source rel
  (hash, mtime) <- case sealed of
    Just h -> FileSystem.mtimeOf srcPath <&> \t -> (h, t)
    Nothing -> hashOf FileSystem.FromCache srcPath (\_ -> pure ())
  t <- asks @Creator (.creationDate)
  pure (Just (FileEntry.new rel size mtime hash FileEntry.Failed t))

sourceHashFor
  :: (Hashing es)
  => OsPath
  -> Vector PlannedWrite
  -> Maybe Hash
  -> Eff es ()
  -> Eff es (Hash, UTCTime)
sourceHashFor srcPath writing sealed onFlush
  | not (V.null writing) = hashVia (FileSystem.writeTemps srcPath writing onFlush) (\bs -> bs & BS.length & fromIntegral & advanceProgress)
  | Just h <- sealed = FileSystem.mtimeOf srcPath <&> \t -> (h, t)
  | otherwise = hashOf FileSystem.FromCache srcPath (\bs -> bs & BS.length & fromIntegral & advanceProgress)

reuseOrReplace
  :: (Hashing es)
  => OsPath
  -> Vector PlannedWrite
  -> RelPath
  -> UTCTime
  -> Hash
  -> FileStepResult
  -> Eff es FileStepResult
reuseOrReplace srcPath reusing rel mtime srcHash written = do
  checks <- V.mapM check reusing
  flushProgress
  let replaced = V.mapMaybe (either Just (const Nothing)) checks
  pure $ case replaced V.!? 0 of
    Nothing -> written
    Just actual -> FileStepResult {outcome = FileOutcome.Replaced (FileOutcome.Mismatch srcHash actual), manifestEntry = written.manifestEntry}
  where
    check w = do
      (actual, _) <- hashOf FileSystem.FromDevice w.final (\bs -> bs & BS.length & fromIntegral & advanceProgress)
      if actual == srcHash
        then pure (Right ())
        else do
          let demoted = V.singleton w {PlannedWrite.mode = PlannedWrite.Overwrite}
          Emit.event (JobEvent.FileStatusChanged rel FileStatus.Copying)
          _ <-
            FileSystem.writeTemps
              srcPath
              demoted
              (Emit.event (JobEvent.FileStatusChanged rel FileStatus.Flushing))
              (\bs -> bs & BS.length & fromIntegral & advanceProgress)
          readBack <- publishAndVerify rel demoted mtime srcHash
          case readBack of
            FileOutcome.Ok -> pure (Left actual)
            failure -> throwIO $ userError $ case failure of
              FileOutcome.HashMismatch m -> T.unpack ("the copy written after a mismatch does not match either: " <> m.actual.value)
              other -> show other

publishCopy
  :: (Copying es)
  => Vector PlannedWrite
  -> Maybe Hash
  -> RelPath
  -> FileSize
  -> UTCTime
  -> Hash
  -> Eff es FileStepResult
publishCopy writes sealed rel size mtime srcHash = do
  destOutcome <- publishAndVerify rel writes mtime srcHash
  t <- asks @Creator (.creationDate)
  let (sourceOutcome, sourceAction, entryHash) = case sealed of
        Nothing -> (FileOutcome.Ok, FileEntry.Original, srcHash)
        Just expected
          | expected == srcHash -> (FileOutcome.Ok, FileEntry.Verified, srcHash)
          | otherwise -> (FileOutcome.HashMismatch (FileOutcome.Mismatch expected srcHash), FileEntry.Failed, srcHash)
      outcome = case destOutcome of
        FileOutcome.Ok -> sourceOutcome
        destFailure -> destFailure
      action = case outcome of
        FileOutcome.Ok -> Just sourceAction
        FileOutcome.HashMismatch _ -> Just FileEntry.Failed
        _ -> Nothing
  pure FileStepResult {outcome, manifestEntry = action <&> (\a -> FileEntry.new rel size mtime entryHash a t)}

publishAndVerify :: (Hashing es) => RelPath -> Vector PlannedWrite -> UTCTime -> Hash -> Eff es FileOutcome
publishAndVerify rel writes mtime srcHash = do
  unless (V.null writes) $ do
    Emit.event (JobEvent.FileStatusChanged rel FileStatus.Publishing)
    FileSystem.publish writes mtime
  Emit.event (JobEvent.FileStatusChanged rel FileStatus.Verifying)
  verifyDestinations writes srcHash

verifyDestinations
  :: (Hashing es)
  => Vector PlannedWrite
  -> Hash
  -> Eff es FileOutcome
verifyDestinations writes srcHash = go (V.toList writes)
  where
    go [] = pure FileOutcome.Ok
    go (w : ws) = do
      (actual, _) <- hashOf FileSystem.FromDevice w.final (\bs -> bs & BS.length & fromIntegral & advanceProgress)
      if actual == srcHash then go ws else pure (FileOutcome.HashMismatch (FileOutcome.Mismatch srcHash actual))

runPlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es, Plugins :> es, Error PlanViolation :> es, State JobProgress :> es)
  => Text
  -> Plan
  -> Eff es JobEvent
runPlan hostname plan = do
  fmt <- maybe (throwError PlanViolation.PlanFormatMissing) pure plan.format
  forM_ (Plan.raceChecks plan) (\check -> Generation.require (fst check) (snd check))
  Emit.event (JobEvent.Planned (JobEvent.PlannedWork (V.map (\step -> (step.path, step.size)) plan.steps) plan.bytesToRead))
  let contributions = plan.plugins.contributions
      creator = Creator.addAuthors contributions.authors (Creator.new plan.spec.createdAt hostname "mediacopy3000" (T.pack (showVersion version)))
  runReader creator $ runReader contributions $ runReader fmt $ case plan.execution of
    Plan.CopyInto copy -> runOffloadPlan plan copy
    Plan.RecordAt record -> runGenerationPlan plan record

runOffloadPlan
  :: (Pass es)
  => Plan
  -> Plan.CopyPass
  -> Eff es JobEvent
runOffloadPlan plan copy = do
  sealed <- case plan.sealPass of
    Nothing -> pure (Right PassTally {entries = V.empty, failedPaths = Set.empty})
    Just pass -> runSealPass plan.ignorePatterns copy.source pass
  either pure (copyAfter plan copy) sealed

copyAfter :: (Pass es) => Plan -> Plan.CopyPass -> PassTally -> Eff es JobEvent
copyAfter plan copy sealTally = do
  recorded <- originsForCopy plan copy
  copied <- runSteps (inspectVerified copy.source) copy.source recorded plan.steps
  forM_ copy.generations $ \planned -> do
    Emit.event JobEvent.ManifestWriting
    FileSystem.makeDirectories (V.map (RelPath.toOsPath planned.folder) planned.directories)
    when (copy.carried > 0) $
      void (Store.carryHistory copy.source planned.folder >>= PlanViolation.orThrow . first (PlanViolation.HistoryFaultAt copy.source))
    Generation.write planned plan.ignorePatterns copied.entries
  pure (JobEvent.Finished (resultOf (sealTally.failedPaths <> copied.failedPaths)))

originsForCopy
  :: (FileSystem :> es, Emit :> es, Error PlanViolation :> es, Reader Algo :> es)
  => Plan
  -> Plan.CopyPass
  -> Eff es (Map RelPath Hash)
originsForCopy plan copy = do
  fmt <- ask @Algo
  case plan.sealPass of
    Nothing -> do
      Emit.event (JobEvent.OriginalsResolved plan.originsUsed fmt)
      pure Map.empty
    Just _ -> do
      recorded <-
        Store.resolveOriginals copy.source >>= \case
          Left e -> throwError (PlanViolation.OriginalsUnresolved e)
          Right Nothing -> throwError (PlanViolation.OriginalsMissingAfterSeal copy.source)
          Right (Just hs) -> pure hs
      Emit.event (JobEvent.OriginalsResolved (Plan.originsDescription recorded) fmt)
      pure recorded

runSealPass
  :: (Pass es)
  => Vector Text
  -> OsPath
  -> Plan.SealPass
  -> Eff es (Either JobEvent PassTally)
runSealPass patterns source pass = do
  tally <- runSteps (\_ _ -> pure ()) source Map.empty pass.steps
  Emit.event JobEvent.ManifestWriting
  Generation.write pass.generation patterns tally.entries
  case pass.onFailure of
    Offload.CopyAnyway -> pure (Right tally)
    Offload.StopBeforeCopy
      | Set.null tally.failedPaths -> pure (Right tally)
      | otherwise -> pure (Left (JobEvent.Failed (display SealStopped {failed = Set.size tally.failedPaths, total = V.length pass.steps})))

runGenerationPlan
  :: (Pass es)
  => Plan
  -> Plan.RecordPass
  -> Eff es JobEvent
runGenerationPlan plan record = do
  tally <- runSteps (inspectVerified record.folder) record.folder Map.empty plan.steps
  Emit.event JobEvent.ManifestWriting
  Generation.write record.generation plan.ignorePatterns tally.entries
  pure (JobEvent.Finished (resultOf tally.failedPaths))

runStep
  :: (Copying es)
  => OsPath
  -> Map RelPath Hash
  -> PlanStep
  -> Eff es FileStepResult
runStep root recorded step = case step.op of
  PlanStep.Copy expected -> do
    Emit.event (JobEvent.FileStatusChanged step.path FileStatus.Copying)
    let sealed = expectedHash recorded step.path expected
    runFileStep step (failedCopyEntry root sealed step.path step.size) (copyAndVerify root step.writes sealed step.path step.size)
  PlanStep.ReportMissing -> runFileStep step (pure Nothing) (pure FileStepResult {outcome = FileOutcome.Missing, manifestEntry = Nothing})
  PlanStep.ReportNew -> do
    Emit.event (JobEvent.FileStatusChanged step.path FileStatus.Hashing)
    hashStep reportNew
  PlanStep.VerifyAgainst expected -> do
    Emit.event (JobEvent.FileStatusChanged step.path FileStatus.Verifying)
    hashStep (reportVerified expected)
  where
    path = RelPath.toOsPath root step.path
    hashStep report = runFileStep step (pure Nothing) $ do
      (actual, mtime) <- hashOf FileSystem.FromDevice path (\bs -> bs & BS.length & fromIntegral & advanceProgress)
      flushProgress
      t <- asks @Creator (.creationDate)
      pure (report actual mtime t)
    reportNew actual mtime t =
      FileStepResult {outcome = FileOutcome.New, manifestEntry = Just (FileEntry.new step.path step.size mtime actual FileEntry.Original t)}
    reportVerified expected actual mtime t
      | actual == expected =
          FileStepResult {outcome = FileOutcome.Ok, manifestEntry = Just (FileEntry.new step.path step.size mtime expected FileEntry.Verified t)}
      | otherwise =
          FileStepResult
            { outcome = FileOutcome.HashMismatch (FileOutcome.Mismatch expected actual)
            , manifestEntry = Just (FileEntry.new step.path step.size mtime actual FileEntry.Failed t)
            }

runSteps
  :: (Copying es)
  => (PlanStep -> FileStepResult -> Eff es ())
  -> OsPath
  -> Map RelPath Hash
  -> Vector PlanStep
  -> Eff es PassTally
runSteps onDone root recorded steps =
  traverse (\step -> runStep root recorded step >>= \result -> onDone step result >> pure result) steps <&> \results ->
    PassTally
      { entries = V.mapMaybe (.manifestEntry) results
      , failedPaths = Set.fromList [step.path | (step, result) <- zip (V.toList steps) (V.toList results), FileOutcome.failed result.outcome]
      }

inspectVerified :: (Plugins :> es) => OsPath -> PlanStep -> FileStepResult -> Eff es ()
inspectVerified root step result = case result.manifestEntry of
  Just entry
    | not (FileOutcome.failed result.outcome) ->
        Plugins.fileVerified
          VerifiedFile.VerifiedFile
            { path = step.path
            , location = maybe (RelPath.toOsPath root step.path) (\write -> write.final) (step.writes V.!? 0)
            , hashes = V.map (\recorded -> recorded.hash) entry.hashes
            , size = step.size
            }
  _ -> pure ()

resultOf :: Set RelPath -> JobResult
resultOf failed = if Set.null failed then JobResult.AllOk else JobResult.WithFailures (Set.size failed)

expectedHash :: Map RelPath Hash -> RelPath -> PlanStep.Expected -> Maybe Hash
expectedHash recorded path expected = case expected of
  PlanStep.NoOriginal -> Nothing
  PlanStep.Recorded sealed -> Just sealed
  PlanStep.FromSealPass -> Map.lookup path recorded
