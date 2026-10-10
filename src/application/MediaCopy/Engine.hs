module MediaCopy.Engine
  ( planJob
  , executePlan
  , readHistory
  ) where

import Ascmhl.Build (creatorInfo, fileEntry, withAuthors)
import Ascmhl.Hash
import Ascmhl.Path (RelPath, relToOsPath)
import Ascmhl.Types
import Control.Monad (forM_, unless, void, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Exception (displayException, finally, throwIO, trySync)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Effectful.Time (Time, currentTime)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin (Contributions (..), PluginPlan (..), VerifiedFile (..), pluginBlockers)
import MediaCopy.Effects.Emit
import MediaCopy.Effects.FileSystem
import MediaCopy.Effects.Hasher
import MediaCopy.Effects.Plugins
import MediaCopy.Engine.Generation (requireGeneration, writeGeneration)
import MediaCopy.Engine.Plan (planJob)
import MediaCopy.Engine.Violation (PlanViolation (..), orThrow)
import MediaCopy.Mhl.Store
import Paths_mediacopy3000 (version)

type Hashing es =
  ( FileSystem :> es
  , Hasher :> es
  , Time :> es
  , Emit :> es
  , State JobProgress :> es
  , Reader HashAlgo :> es
  )

type Copying es = (Hashing es, Reader CreatorInfo :> es, Reader Contributions :> es, Plugins :> es)

type Pass es = (Copying es, Error PlanViolation :> es)

data JobProgress = JobProgress
  { readSoFar :: Int64
  , lastEmit :: UTCTime
  }

data PassTally = PassTally
  { entries :: Vector HashEntry
  , failedPaths :: Set RelPath
  }

data FileStepResult = FileStepResult
  { outcome :: FileOutcome
  , manifestEntry :: Maybe HashEntry
  }

data SealCheck = SealCheck
  { outcome :: FileOutcome
  , action :: HashAction
  , hash :: Hash
  }

executePlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es, Plugins :> es)
  => Text
  -> JobPlan
  -> Eff es ()
executePlan hostname plan
  | planBlocked plan =
      V.toList (V.map (\finding -> display finding.code <> ": " <> finding.detail) (blockers plan))
        <> V.toList (V.map display (pluginBlockers plan.plugins))
        & T.intercalate "; "
        & JobFailed
        & emit
  | otherwise = do
      startTime <- currentTime
      result <- trySync (runErrorNoCallStack @PlanViolation (evalState (start startTime) (runPlan hostname plan)))
      endJob $ case result of
        Left e -> JobFailed (T.pack (displayException e))
        Right (Left violation) -> JobFailed (display violation)
        Right (Right terminal) -> terminal
  where
    start t = JobProgress {readSoFar = 0, lastEmit = t}

endJob :: (Emit :> es, Plugins :> es) => JobEvent -> Eff es ()
endJob terminal = do
  settle
  trySync (emit terminal) >>= \case
    Left e -> emit (JobFailed (T.pack (displayException e)))
    Right () -> pure ()

advanceProgress :: (Time :> es, Emit :> es, State JobProgress :> es) => Int64 -> Eff es ()
advanceProgress n = do
  modify (\st -> st {readSoFar = st.readSoFar + n})
  st <- get @JobProgress
  t <- currentTime
  when (diffUTCTime t st.lastEmit >= 0.1) $ do
    modify (\s -> s {lastEmit = t})
    emit (Progress st.readSoFar)

flushProgress :: (Emit :> es, State JobProgress :> es) => Eff es ()
flushProgress = do
  st <- get @JobProgress
  emit (Progress st.readSoFar)

runFileStep
  :: (Emit :> es, State JobProgress :> es)
  => PlanStep
  -> Eff es FileStepResult
  -> Eff es FileStepResult
runFileStep step body = do
  before <- get @JobProgress
  attempt <- trySync body
  case attempt of
    Left e -> do
      let outcome = IoError (T.pack (displayException e))
      emit (FileStatusChanged step.path (Done outcome))
      modify (\st -> st {readSoFar = before.readSoFar + stepBytesToRead step})
      flushProgress
      pure FileStepResult {outcome, manifestEntry = Nothing}
    Right result -> do
      emit (FileStatusChanged step.path (Done result.outcome))
      pure result

hashVia
  :: (Hasher :> es, Reader HashAlgo :> es)
  => ((ByteString -> Eff es ()) -> Eff es UTCTime)
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashVia readWith onChunk = do
  fmt <- ask @HashAlgo
  withHasher fmt $ \hasherH -> do
    mtime <- readWith (\bs -> feed hasherH bs >> onChunk bs)
    h <- finish hasherH
    pure (h, mtime)

hashOf
  :: (FileSystem :> es, Hasher :> es, Reader HashAlgo :> es)
  => ReadCache
  -> OsPath
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashOf mode path = hashVia (streamFile mode path)

checkAgainstSeal :: Maybe Hash -> Hash -> SealCheck
checkAgainstSeal sealed actual = case sealed of
  Nothing -> SealCheck {outcome = Ok, action = Original, hash = actual}
  Just expected
    | expected == actual -> SealCheck {outcome = Ok, action = Verified, hash = actual}
    | otherwise -> SealCheck {outcome = HashMismatch (Mismatch expected actual), action = FailedAction, hash = expected}

copyAndVerify
  :: (Copying es)
  => OsPath
  -> Vector PlannedWrite
  -> Maybe Hash
  -> RelPath
  -> FileSize
  -> Eff es FileStepResult
copyAndVerify source writes sealed rel size = step `finally` void (trySync (removeTemps writes))
  where
    step = do
      let srcPath = relToOsPath source rel
          (reusing, writing) = V.partition (\w -> w.mode == Reuse) writes
      (srcHash, mtime) <- sourceHashFor srcPath writing sealed (emit (FileStatusChanged rel Flushing))
      flushProgress
      attempt <- trySync $ do
        written <- publishCopy writing sealed rel size mtime srcHash
        case written.outcome of
          Ok -> reuseOrReplace srcPath reusing rel mtime srcHash written
          _ -> pure written
      case attempt of
        Left e -> do
          t <- asks @CreatorInfo (.creationDate)
          let check = checkAgainstSeal sealed srcHash
              outcome = IoError (T.pack (displayException e))
          pure FileStepResult {outcome, manifestEntry = Just (fileEntry rel size mtime check.hash FailedAction t)}
        Right result -> pure result

sourceHashFor
  :: (Hashing es)
  => OsPath
  -> Vector PlannedWrite
  -> Maybe Hash
  -> Eff es ()
  -> Eff es (Hash, UTCTime)
sourceHashFor srcPath writing sealed onFlush
  | not (V.null writing) = hashVia (writeTemps srcPath writing onFlush) (advanceProgress . fromIntegral . BS.length)
  | Just h <- sealed = mtimeOf srcPath <&> \t -> (h, t)
  | otherwise = hashOf FromCache srcPath (advanceProgress . fromIntegral . BS.length)

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
    Just actual -> FileStepResult {outcome = Replaced (Mismatch srcHash actual), manifestEntry = written.manifestEntry}
  where
    check w = do
      (actual, _) <- hashOf FromDevice w.final (advanceProgress . fromIntegral . BS.length)
      if actual == srcHash
        then pure (Right ())
        else do
          let demoted = V.singleton w {mode = Overwrite}
          emit (FileStatusChanged rel Copying)
          _ <-
            writeTemps
              srcPath
              demoted
              (emit (FileStatusChanged rel Flushing))
              (advanceProgress . fromIntegral . BS.length)
          readBack <- publishAndVerify rel demoted mtime srcHash
          case readBack of
            Ok -> pure (Left actual)
            failure -> throwIO $ userError $ case failure of
              HashMismatch m -> T.unpack ("the copy written after a mismatch does not match either: " <> m.actual.value)
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
  t <- asks @CreatorInfo (.creationDate)
  let sourceCheck = checkAgainstSeal sealed srcHash
      outcome = case destOutcome of
        Ok -> sourceCheck.outcome
        destFailure -> destFailure
      action = case outcome of
        Ok -> Just sourceCheck.action
        HashMismatch _ -> Just FailedAction
        _ -> Nothing
  pure FileStepResult {outcome, manifestEntry = action <&> (\a -> fileEntry rel size mtime sourceCheck.hash a t)}

publishAndVerify :: (Hashing es) => RelPath -> Vector PlannedWrite -> UTCTime -> Hash -> Eff es FileOutcome
publishAndVerify rel writes mtime srcHash = do
  unless (V.null writes) $ do
    emit (FileStatusChanged rel Publishing)
    publish writes mtime
  emit (FileStatusChanged rel Verifying)
  verifyDestinations writes srcHash

verifyDestinations
  :: (Hashing es)
  => Vector PlannedWrite
  -> Hash
  -> Eff es FileOutcome
verifyDestinations writes srcHash = go (V.toList writes)
  where
    go [] = pure Ok
    go (w : ws) = do
      (actual, _) <- hashOf FromDevice w.final (advanceProgress . fromIntegral . BS.length)
      if actual == srcHash then go ws else pure (HashMismatch (Mismatch srcHash actual))

runPlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es, Plugins :> es, Error PlanViolation :> es, State JobProgress :> es)
  => Text
  -> JobPlan
  -> Eff es JobEvent
runPlan hostname plan = do
  fmt <- maybe (throwError PlanFormatMissing) pure plan.format
  forM_ (planRaceChecks plan) (\check -> requireGeneration (fst check) (snd check))
  emit (Planned (PlannedWork (V.map (\step -> (step.path, step.size)) plan.steps) plan.bytesToRead))
  let contributions = plan.plugins.contributions
      creator = withAuthors contributions.authors (creatorInfo plan.spec.createdAt hostname "mediacopy3000" (T.pack (showVersion version)))
  runReader creator $ runReader contributions $ runReader fmt $ case plan.execution of
    CopyInto copy -> runOffloadPlan plan copy
    RecordAt record -> runGenerationPlan plan record

runOffloadPlan
  :: (Pass es)
  => JobPlan
  -> CopyPass
  -> Eff es JobEvent
runOffloadPlan plan copy = do
  sealed <- case plan.sealPass of
    Nothing -> pure (Right PassTally {entries = V.empty, failedPaths = Set.empty})
    Just pass -> runSealPass plan.ignorePatterns copy.source pass
  either pure (copyAfter plan copy) sealed

copyAfter :: (Pass es) => JobPlan -> CopyPass -> PassTally -> Eff es JobEvent
copyAfter plan copy sealTally = do
  recorded <- originsForCopy plan copy
  copied <- runSteps (inspectVerified copy.source) copy.source recorded plan.steps
  forM_ copy.generations $ \planned -> do
    emit ManifestWriting
    makeDirectories (V.map (relToOsPath planned.folder) planned.directories)
    when (copy.carried > 0) $
      void (carryHistory copy.source planned.folder >>= orThrow . first (HistoryFaultAt copy.source))
    writeGeneration planned plan.ignorePatterns copied.entries
  pure (JobFinished (resultOf (sealTally.failedPaths <> copied.failedPaths)))

originsForCopy
  :: (FileSystem :> es, Emit :> es, Error PlanViolation :> es, Reader HashAlgo :> es)
  => JobPlan
  -> CopyPass
  -> Eff es (Map RelPath Hash)
originsForCopy plan copy = do
  fmt <- ask @HashAlgo
  case plan.sealPass of
    Nothing -> do
      emit (OriginalsResolved plan.originsUsed fmt)
      pure Map.empty
    Just _ -> do
      recorded <-
        resolveOriginals copy.source >>= \case
          Left e -> throwError (OriginalsUnresolved e)
          Right Nothing -> throwError (OriginalsMissingAfterSeal copy.source)
          Right (Just hs) -> pure hs
      emit (OriginalsResolved (originsDescription recorded) fmt)
      pure recorded

runSealPass
  :: (Pass es)
  => Vector Text
  -> OsPath
  -> SealPass
  -> Eff es (Either JobEvent PassTally)
runSealPass patterns source pass = do
  tally <- runSteps (\_ _ -> pure ()) source Map.empty pass.steps
  emit ManifestWriting
  writeGeneration pass.generation patterns tally.entries
  case pass.onFailure of
    CopyAnyway -> pure (Right tally)
    StopBeforeCopy
      | Set.null tally.failedPaths -> pure (Right tally)
      | otherwise -> pure (Left (JobFailed (display SealStopped {failed = Set.size tally.failedPaths, total = V.length pass.steps})))

runGenerationPlan
  :: (Pass es)
  => JobPlan
  -> RecordPass
  -> Eff es JobEvent
runGenerationPlan plan record = do
  tally <- runSteps (inspectVerified record.folder) record.folder Map.empty plan.steps
  emit ManifestWriting
  writeGeneration record.generation plan.ignorePatterns tally.entries
  pure (JobFinished (resultOf tally.failedPaths))

runStep
  :: (Copying es)
  => OsPath
  -> Map RelPath Hash
  -> PlanStep
  -> Eff es FileStepResult
runStep root recorded step = case step.op of
  Copy expected -> do
    emit (FileStatusChanged step.path Copying)
    runFileStep step (copyAndVerify root step.writes (expectedHash recorded step.path expected) step.path step.size)
  ReportMissing -> runFileStep step (pure FileStepResult {outcome = Missing, manifestEntry = Nothing})
  ReportNew -> do
    emit (FileStatusChanged step.path Hashing)
    hashStep reportNew
  VerifyAgainst expected -> do
    emit (FileStatusChanged step.path Verifying)
    hashStep (reportVerified expected)
  where
    path = relToOsPath root step.path
    hashStep report = runFileStep step $ do
      (actual, mtime) <- hashOf FromDevice path (advanceProgress . fromIntegral . BS.length)
      flushProgress
      t <- asks @CreatorInfo (.creationDate)
      pure (report actual mtime t)
    reportNew actual mtime t =
      FileStepResult {outcome = New, manifestEntry = Just (fileEntry step.path step.size mtime actual Original t)}
    reportVerified expected actual mtime t
      | actual == expected =
          FileStepResult {outcome = Ok, manifestEntry = Just (fileEntry step.path step.size mtime expected Verified t)}
      | otherwise =
          FileStepResult
            { outcome = HashMismatch (Mismatch expected actual)
            , manifestEntry = Just (fileEntry step.path step.size mtime expected FailedAction t)
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
      , failedPaths = Set.fromList [step.path | (step, result) <- zip (V.toList steps) (V.toList results), outcomeFailed result.outcome]
      }

inspectVerified :: (Plugins :> es) => OsPath -> PlanStep -> FileStepResult -> Eff es ()
inspectVerified root step result = case result.manifestEntry of
  Just entry
    | not (outcomeFailed result.outcome) ->
        fileVerified
          VerifiedFile
            { path = step.path
            , location = maybe (relToOsPath root step.path) (\write -> write.final) (step.writes V.!? 0)
            , hashes = V.map (\recorded -> recorded.hash) entry.hashes
            , size = step.size
            }
  _ -> pure ()

resultOf :: Set RelPath -> JobResult
resultOf failed = if Set.null failed then AllOk else WithFailures (Set.size failed)

expectedHash :: Map RelPath Hash -> RelPath -> Expected -> Maybe Hash
expectedHash recorded path expected = case expected of
  NoOriginal -> Nothing
  Recorded sealed -> Just sealed
  FromSealPass -> Map.lookup path recorded
