module MediaCopy.Engine
  ( ToolInfo (..)
  , defaultToolInfo
  , runJob
  , planJob
  , executePlan
  , readHistory
  ) where

import Ascmhl.Build (fileEntry)
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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Exception (displayException, finally, throwIO, trySync)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Effectful.Time (Time, currentTime)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (JobFormat, formatAlgo)
import MediaCopy.Domain.Plan
import MediaCopy.Effects.Emit
import MediaCopy.Effects.FileSystem
import MediaCopy.Effects.Hasher
import MediaCopy.Engine.Config
import MediaCopy.Engine.Generation (requireNextGeneration, writeGeneration)
import MediaCopy.Engine.Plan (planJob)
import MediaCopy.Engine.Violation (PlanViolation (..), orThrow)
import MediaCopy.Mhl.Store

type Copying es =
  ( FileSystem :> es
  , Hasher :> es
  , Time :> es
  , Emit :> es
  , State JobProgress :> es
  , Reader JobFormat :> es
  , Reader JobInstant :> es
  )

type Hashing es =
  ( FileSystem :> es
  , Hasher :> es
  , Time :> es
  , Emit :> es
  , State JobProgress :> es
  , Reader JobFormat :> es
  )

type Pass es = (Copying es, Error PlanViolation :> es, Reader ToolInfo :> es)

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

runJob
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es)
  => ToolInfo
  -> JobSpec
  -> Eff es ()
runJob cfg spec = planJob spec >>= executePlan cfg

executePlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es)
  => ToolInfo
  -> JobPlan
  -> Eff es ()
executePlan cfg plan
  | planBlocked plan = emit (JobFailed (blockerText plan))
  | otherwise = do
      startTime <- currentTime
      result <- trySync (runErrorNoCallStack @PlanViolation (runReader cfg (evalState (start startTime) (runPlan plan))))
      case result of
        Left e -> emit (JobFailed (T.pack (displayException e)))
        Right (Left violation) -> emit (JobFailed (display violation))
        Right (Right ()) -> pure ()
  where
    start t = JobProgress {readSoFar = 0, lastEmit = t}

blockerText :: JobPlan -> Text
blockerText plan =
  plan
    & blockers
    & V.toList
    & map (\finding -> display finding.code <> ": " <> finding.detail)
    & T.intercalate "; "

getProgress :: (State JobProgress :> es) => Eff es JobProgress
getProgress = get @JobProgress

advanceProgress :: (Time :> es, Emit :> es, State JobProgress :> es) => Int64 -> Eff es ()
advanceProgress n = do
  modify (\st -> st {readSoFar = st.readSoFar + n})
  st <- getProgress
  t <- currentTime
  when (diffUTCTime t st.lastEmit >= 0.1) $ do
    modify (\s -> s {lastEmit = t})
    emit (Progress st.readSoFar)

flushProgress :: (Emit :> es, State JobProgress :> es) => Eff es ()
flushProgress = do
  st <- getProgress
  emit (Progress st.readSoFar)

runFileStep
  :: (Emit :> es, State JobProgress :> es)
  => PlanStep
  -> Eff es FileStepResult
  -> Eff es FileStepResult
runFileStep step body = do
  before <- getProgress
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
  :: (Hasher :> es, Reader JobFormat :> es)
  => ((ByteString -> Eff es ()) -> Eff es UTCTime)
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashVia readWith onChunk = do
  fmt <- ask @JobFormat
  withHasher fmt $ \hasherH -> do
    mtime <- readWith (\bs -> feed hasherH bs >> onChunk bs)
    h <- finish hasherH
    pure (h, mtime)

hashOf
  :: (FileSystem :> es, Hasher :> es, Reader JobFormat :> es)
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
          JobInstant t <- ask @JobInstant
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
  | not (V.null writing) = hashVia (writeTemps srcPath writing onFlush) (\bs -> advanceProgress (fromIntegral (BS.length bs)))
  | Just h <- sealed = mtimeOf srcPath <&> \t -> (h, t)
  | otherwise = hashOf FromCache srcPath (\bs -> advanceProgress (fromIntegral (BS.length bs)))

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
  let replaced = V.mapMaybe (\c -> either Just (const Nothing) c) checks
  pure $ case replaced V.!? 0 of
    Nothing -> written
    Just actual -> FileStepResult {outcome = Replaced (Mismatch srcHash actual), manifestEntry = written.manifestEntry}
  where
    check w = do
      (actual, _) <- hashOf FromDevice w.final (\bs -> advanceProgress (fromIntegral (BS.length bs)))
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
              (\bs -> advanceProgress (fromIntegral (BS.length bs)))
          readBack <- publishAndVerify rel demoted mtime srcHash
          case readBack of
            Ok -> pure (Left actual)
            failure -> throwIO (userError (T.unpack (readBackText failure)))

readBackText :: FileOutcome -> Text
readBackText = \case
  HashMismatch m -> "the copy written after a mismatch does not match either: " <> m.actual.value
  other -> T.pack (show other)

publishCopy
  :: (Hashing es, Reader JobInstant :> es)
  => Vector PlannedWrite
  -> Maybe Hash
  -> RelPath
  -> FileSize
  -> UTCTime
  -> Hash
  -> Eff es FileStepResult
publishCopy writes sealed rel size mtime srcHash = do
  destOutcome <- publishAndVerify rel writes mtime srcHash
  JobInstant t <- ask @JobInstant
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
      (actual, _) <- hashOf FromDevice w.final (\bs -> advanceProgress (fromIntegral (BS.length bs)))
      if actual == srcHash then go ws else pure (HashMismatch (Mismatch srcHash actual))

runPlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es, Error PlanViolation :> es, State JobProgress :> es, Reader ToolInfo :> es)
  => JobPlan
  -> Eff es ()
runPlan plan = do
  fmt <- maybe (throwError PlanFormatMissing) pure plan.format
  forM_ (planRaceChecks plan) (\check -> requireGeneration (fst check) (snd check))
  emit (Planned (PlannedWork (V.map (\step -> (step.path, step.size)) plan.steps) plan.bytesToRead))
  runReader (JobInstant plan.spec.createdAt) $ runReader fmt $ case plan.execution of
    CopyInto copy -> runOffloadPlan plan copy
    RecordAt record -> runGenerationPlan plan record

requireGeneration :: (FileSystem :> es, Error PlanViolation :> es) => OsPath -> Int -> Eff es ()
requireGeneration folder number = do
  found <- loadChain folder >>= orThrow . first (HistoryFaultAt folder)
  orThrow (requireNextGeneration folder number (fromMaybe (Chain {entries = V.empty}) found))

runOffloadPlan
  :: (Pass es)
  => JobPlan
  -> CopyPass
  -> Eff es ()
runOffloadPlan plan copy = do
  sealed <- case plan.sealPass of
    Nothing -> pure (Just PassTally {entries = V.empty, failedPaths = Set.empty})
    Just pass -> runSealPass plan.ignorePatterns copy.source pass
  forM_ sealed $ \sealTally -> do
    recorded <- originsForCopy plan copy
    copied <- runSteps copy.source recorded plan.steps
    forM_ copy.generations $ \planned -> do
      emit ManifestWriting
      makeDirectories (V.map (\dir -> relToOsPath planned.folder dir) planned.directories)
      when (copy.carried > 0) $
        void (carryHistory copy.source planned.folder >>= orThrow . first (HistoryFaultAt copy.source))
      writeGeneration planned plan.ignorePatterns copied.entries
    emit (JobFinished (resultOf (sealTally.failedPaths <> copied.failedPaths)))

originsForCopy
  :: (FileSystem :> es, Emit :> es, Error PlanViolation :> es, Reader JobFormat :> es)
  => JobPlan
  -> CopyPass
  -> Eff es (Map RelPath Hash)
originsForCopy plan copy = do
  fmt <- ask @JobFormat
  case plan.sealPass of
    Nothing -> do
      emit (OriginalsResolved plan.originsUsed (formatAlgo fmt))
      pure Map.empty
    Just _ -> do
      recorded <-
        resolveOriginals copy.source >>= \case
          Left e -> throwError (OriginalsUnresolved e)
          Right Nothing -> throwError (OriginalsMissingAfterSeal copy.source)
          Right (Just hs) -> pure hs
      emit (OriginalsResolved (originsDescription recorded) (formatAlgo fmt))
      pure recorded

runSealPass
  :: (Pass es)
  => Vector Text
  -> OsPath
  -> SealPass
  -> Eff es (Maybe PassTally)
runSealPass patterns source pass = do
  tally <- runSteps source Map.empty pass.steps
  emit ManifestWriting
  writeGeneration pass.generation patterns tally.entries
  case pass.onFailure of
    CopyAnyway -> pure (Just tally)
    StopBeforeCopy
      | Set.null tally.failedPaths -> pure (Just tally)
      | otherwise -> do
          emit (JobFailed (display SealStopped {failed = Set.size tally.failedPaths, total = V.length pass.steps}))
          pure Nothing

runGenerationPlan
  :: (Pass es)
  => JobPlan
  -> RecordPass
  -> Eff es ()
runGenerationPlan plan record = do
  tally <- runSteps record.folder Map.empty plan.steps
  emit ManifestWriting
  writeGeneration record.generation plan.ignorePatterns tally.entries
  emit (JobFinished (resultOf tally.failedPaths))

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
      (actual, mtime) <- hashOf FromDevice path (\bs -> advanceProgress (fromIntegral (BS.length bs)))
      flushProgress
      JobInstant t <- ask @JobInstant
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
  => OsPath
  -> Map RelPath Hash
  -> Vector PlanStep
  -> Eff es PassTally
runSteps root recorded steps =
  traverse (\step -> runStep root recorded step) steps <&> \results ->
    PassTally
      { entries = V.mapMaybe (\result -> result.manifestEntry) results
      , failedPaths = Set.fromList [step.path | (step, result) <- zip (V.toList steps) (V.toList results), outcomeFailed result.outcome]
      }

resultOf :: Set RelPath -> JobResult
resultOf failed = if Set.null failed then AllOk else WithFailures (Set.size failed)

expectedHash :: Map RelPath Hash -> RelPath -> Expected -> Maybe Hash
expectedHash recorded path expected = case expected of
  NoOriginal -> Nothing
  Recorded sealed -> Just sealed
  FromSealPass -> Map.lookup path recorded
