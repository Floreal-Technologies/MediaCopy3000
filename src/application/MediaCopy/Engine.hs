-- | Runs a plan: the copies, the checks, and the generations they produce.
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Exception (displayException, onException, throwIO, trySync)
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

-- | A whole pass: it can find the plan and the disk disagree, and it records who wrote the manifest.
type Pass es = (Copying es, Error PlanViolation :> es, Reader ToolInfo :> es)

data JobProgress = JobProgress
  { readSoFar :: Int64
  , lastEmit :: UTCTime
  }

-- | A pass's entries and failures are its own result, so no later pass can inherit them.
data PassTally = PassTally
  { entries :: Vector HashEntry
  , failures :: Int
  }

data FileStepResult = FileStepResult
  { outcome :: FileOutcome
  , manifestEntry :: Maybe HashEntry
  }

-- | What the source's own seal says about the bytes just read.
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

-- | A synchronous exception becomes 'JobFailed'. An asynchronous exception propagates.
executePlan
  :: (FileSystem :> es, Hasher :> es, Time :> es, Emit :> es)
  => ToolInfo
  -> JobPlan
  -> Eff es ()
executePlan cfg plan
  | planBlocked plan = emit (JobFailed (blockerText plan))
  | otherwise = do
      startTime <- currentTime
      -- 'runErrorNoCallStack' sits inside 'trySync' on purpose. The 'Error' effect travels as an
      -- exception, so a 'trySync' outside it can catch the violation and report it as an IO fault.
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

-- | Counts n more bytes. Emits 'Progress' at most every 100ms.
advanceProgress :: (Time :> es, Emit :> es, State JobProgress :> es) => Int64 -> Eff es ()
advanceProgress n = do
  modify (\st -> st {readSoFar = st.readSoFar + n})
  st <- getProgress
  t <- currentTime
  when (diffUTCTime t st.lastEmit >= 0.1) $ do
    modify (\s -> s {lastEmit = t})
    emit (Progress st.readSoFar)

-- | Emits 'Progress' whatever the throttle says, for the end of a file.
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
  -- The body reports a file's fault, never the engine's. 'Error' travels as an exception,
  -- so a 'PlanViolation' thrown here becomes this file's IO error. Throw above.
  attempt <- trySync body
  case attempt of
    Left e -> do
      let outcome = IoError (T.pack (displayException e))
      emit (FileStatusChanged step.path (Done outcome))
      -- 'State' survives 'trySync'. Charge the planned read from the value read before the body.
      modify (\st -> st {readSoFar = before.readSoFar + stepBytesToRead step})
      flushProgress
      pure FileStepResult {outcome, manifestEntry = Nothing}
    Right result -> do
      emit (FileStatusChanged step.path (Done result.outcome))
      pure result

-- | One read answers both the hash and the mtime, so a manifest entry describes the
-- bytes the hash covers.
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

-- | The hash and the mtime of one file on disk.
hashOf
  :: (FileSystem :> es, Hasher :> es, Reader JobFormat :> es)
  => ReadCache
  -> OsPath
  -> (ByteString -> Eff es ())
  -> Eff es (Hash, UTCTime)
hashOf mode path = hashVia (streamFile mode path)

-- | A source that no longer matches its seal records the sealed hash, never the bytes just read. A record of the actual hash blesses the corruption.
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
copyAndVerify source writes sealed rel size = do
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
    -- The source hash is known, so the engine records the failed file like a mismatch instead of dropping it.
    Left e -> do
      cleanupTemps writes
      JobInstant t <- ask @JobInstant
      let check = checkAgainstSeal sealed srcHash
          outcome = IoError (T.pack (displayException e))
      pure FileStepResult {outcome, manifestEntry = Just (fileEntry rel size mtime check.hash FailedAction t)}
    Right result -> pure result

-- | The source is read once at most. With nothing to write and a recorded hash, it is not read at all.
sourceHashFor
  :: (Hashing es)
  => OsPath
  -> Vector PlannedWrite
  -> Maybe Hash
  -> Eff es ()
  -- ^ Runs when the last chunk is written and the writers are about to synchronise, and not at all
  -- when the step writes nothing.
  -> Eff es (Hash, UTCTime)
sourceHashFor srcPath writing sealed onFlush
  | not (V.null writing) = hashVia (writeTemps srcPath writing onFlush) (\bs -> advanceProgress (fromIntegral (BS.length bs)))
  | Just h <- sealed = mtimeOf srcPath <&> \t -> (h, t)
  | otherwise = hashOf FromCache srcPath (\bs -> advanceProgress (fromIntegral (BS.length bs)))

-- | Hashes each reused destination. One that differs is copied again from the source and read back.
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
  -- A reuse check reads a destination back, which is what 'publishAndVerify' already reported
  -- 'Verifying' for. Only a rewrite moves the file to another state.
  checks <- V.mapM check reusing
  flushProgress
  let replaced = V.mapMaybe (\c -> either Just (const Nothing) c) checks
  pure $ case replaced V.!? 0 of
    Nothing -> written
    Just actual -> FileStepResult {outcome = Replaced (Mismatch srcHash actual), manifestEntry = written.manifestEntry}
  where
    check w = do
      (actual, _) <- hashOf FromDevice w.final (\bs -> advanceProgress (fromIntegral (BS.length bs)))
      -- A match keeps the final and removes the part file a stopped job left beside it.
      if actual == srcHash
        then discard (V.singleton w) >> pure (Right ())
        else do
          -- A rewrite is a whole second copy of the file, so it reports every state a first copy does.
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

-- | Publishes the temp files under their final names and reads every destination back.
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

-- | Publishes the part files under their final names, and reads every one back against the source
-- hash. This owns the two states a published copy passes through. A step with nothing to publish
-- names nothing, but still reads its reused destinations back, so it still verifies.
publishAndVerify :: (Hashing es) => RelPath -> Vector PlannedWrite -> UTCTime -> Hash -> Eff es FileOutcome
publishAndVerify rel writes mtime srcHash = do
  unless (V.null writes) $ do
    emit (FileStatusChanged rel Publishing)
    publish writes mtime
  emit (FileStatusChanged rel Verifying)
  verifyDestinations writes srcHash

-- | On mismatch the engine keeps the final file and flags it in the manifest. A read-back counts
-- against the progress like any other read, because the plan's total counts it.
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

-- | 'Discard' keeps the final of an overwrite and of a reuse, so a failed retry never destroys the copy it found.
cleanupTemps :: (FileSystem :> es) => Vector PlannedWrite -> Eff es ()
cleanupTemps writes = do
  _ <- trySync (discard writes)
  pure ()

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

-- | A race on the folder whose chain the plan rests on stops the job before it makes a
-- directory or reads a byte. 'writeGeneration' checks each folder again at the last moment.
requireGeneration :: (FileSystem :> es, Error PlanViolation :> es) => OsPath -> Int -> Eff es ()
requireGeneration folder number = do
  found <- loadChain folder >>= orThrow . first (HistoryFaultAt folder)
  orThrow (requireNextGeneration folder number (fromMaybe (Chain {entries = V.empty}) found))

-- | The seal writes its generation to the media source before the first copy. The engine checks the copies against the manifest the seal wrote.
runOffloadPlan
  :: (Pass es)
  => JobPlan
  -> CopyPass
  -> Eff es ()
runOffloadPlan plan copy = do
  sealed <- case plan.sealPass of
    Nothing -> pure (Just emptyTally)
    Just pass -> runSealPass plan.ignorePatterns copy.source pass
  forM_ sealed $ \sealTally -> do
    recorded <- originsForCopy plan copy
    copied <- runSteps copy.source recorded plan.steps
    -- The history goes over right before the generation that continues it, so a job that stopped
    -- earlier leaves no history that claims a complete copy.
    forM_ copy.generations $ \planned -> do
      -- Everything below this line writes to the destination and reads no byte of the copy. So the
      -- window names itself, rather than leave the last file's state standing.
      emit ManifestWriting
      -- The copy reproduces the media source's tree, empty folders included, so the destination is
      -- the data set its history describes.
      makeDirectories (V.map (\dir -> relToOsPath planned.folder dir) planned.directories)
      when (copy.carried > 0) $
        void (carryHistory copy.source planned.folder >>= orThrow . first (HistoryFaultAt copy.source))
      writeGeneration planned plan.ignorePatterns copied.entries
    emit (JobFinished (resultOf (sealTally.failures + copied.failures)))

-- | Only a seal pass writes hashes that the plan cannot name, so only that pass reads
-- the media source's history a second time.
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

-- | 'Nothing' is a seal that stopped the job, and it already said so. The copies never see this
-- pass's entries. This pass writes into the media source the job reads, which is the one time an
-- offload does.
runSealPass
  :: (Pass es)
  => Vector Text
  -> OsPath
  -> SealPass
  -> Eff es (Maybe PassTally)
runSealPass patterns source pass = do
  tally <- runSteps source Map.empty pass.steps
  -- The seal writes its manifest to the media source. On a card that is the slowest device the job
  -- touches, so this window is named like every other.
  emit ManifestWriting
  -- A seal records the media source where it stands, so its generation is in-place by definition, not by choice.
  writeGeneration pass.generation patterns tally.entries
  case pass.onFailure of
    CopyAnyway -> pure (Just tally)
    StopBeforeCopy
      | tally.failures == 0 -> pure (Just tally)
      | otherwise -> do
          emit (JobFailed (display SealStopped {failed = tally.failures, total = V.length pass.steps}))
          pure Nothing

-- | 'writeGeneration' makes @ascmhl\/@ and accepts an existing one. A locked folder with an earlier seal fails at the manifest write, not here.
runGenerationPlan
  :: (Pass es)
  => JobPlan
  -> RecordPass
  -> Eff es ()
runGenerationPlan plan record = do
  tally <- runSteps record.folder Map.empty plan.steps
  emit ManifestWriting
  writeGeneration record.generation plan.ignorePatterns tally.entries
  emit (JobFinished (resultOf tally.failures))

-- | Copies, verifies, hashes, or reports missing for one planned step.
runStep
  :: (Copying es)
  => OsPath
  -> Map RelPath Hash
  -> PlanStep
  -> Eff es FileStepResult
runStep root recorded step = case step.op of
  Copy expected -> do
    emit (FileStatusChanged step.path Copying)
    -- 'onException' attaches the cleanup, so it also runs on cancel.
    runFileStep step (copyAndVerify root step.writes (expectedHash recorded step.path expected) step.path step.size `onException` cleanupTemps step.writes)
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
    -- A path absent from the history is new to us but original on the wire.
    reportNew actual mtime t =
      FileStepResult {outcome = New, manifestEntry = Just (fileEntry step.path step.size mtime actual Original t)}
    reportVerified expected actual mtime t
      | actual == expected =
          FileStepResult {outcome = Ok, manifestEntry = Just (fileEntry step.path step.size mtime expected Verified t)}
      | otherwise =
          -- Record the expected hash, never the actual. A record of the actual hash blesses the corruption.
          FileStepResult
            { outcome = HashMismatch (Mismatch expected actual)
            , manifestEntry = Just (fileEntry step.path step.size mtime expected FailedAction t)
            }

-- | The steps run in plan order, so the entries a pass hands back are in that order too.
runSteps
  :: (Copying es)
  => OsPath
  -> Map RelPath Hash
  -> Vector PlanStep
  -> Eff es PassTally
runSteps root recorded steps =
  traverse (\step -> runStep root recorded step) steps <&> \results -> tallyOf results

tallyOf :: Vector FileStepResult -> PassTally
tallyOf results =
  PassTally
    { entries = V.mapMaybe (\result -> result.manifestEntry) results
    , failures = V.length (V.filter (\result -> outcomeFailed result.outcome) results)
    }

emptyTally :: PassTally
emptyTally = PassTally {entries = V.empty, failures = 0}

resultOf :: Int -> JobResult
resultOf failures = if failures == 0 then AllOk else WithFailures failures

-- | A plan cannot carry a hash that its own seal pass writes later, so that expectation resolves here instead.
expectedHash :: Map RelPath Hash -> RelPath -> Expected -> Maybe Hash
expectedHash recorded path expected = case expected of
  NoOriginal -> Nothing
  Recorded sealed -> Just sealed
  FromSealPass -> Map.lookup path recorded
