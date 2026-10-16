{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Domain.JobTest (tests) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.Job
import MediaCopy.Test.InMemoryFS (sampleHash, sampleSpec)

tests :: TestTree
tests =
  testGroup
    "Domain.Job"
    [ testProperty "progress never goes back" progressNeverGoesBack
    , testProperty "the liveness time is the time of the last movement" livenessTimeIsTheLastMovement
    , testProperty "a terminal event settles the job and clears what it had in hand" aTerminalEventSettlesTheJob
    , testProperty "the counts partition the finished files" countsPartitionTheFinishedFiles
    ]

progressNeverGoesBack :: Property
progressNeverGoesBack = property $ do
  timeline <- forAll genTimeline
  let dones = statesOf timeline & map (.bytesDone)
  zipWith (<=) dones (drop 1 dones) & and & assert

livenessTimeIsTheLastMovement :: Property
livenessTimeIsTheLastMovement = property $ do
  timeline <- forAll genTimeline
  let expected =
        timeline
          & filter (\step -> isMovement (snd step))
          & map fst
          & foldl (\_ at -> at) sampleSpec.createdAt
  (finalState timeline).lastMovedAt === expected

aTerminalEventSettlesTheJob :: Property
aTerminalEventSettlesTheJob = property $ do
  timeline <- forAll genTimeline
  terminal <- forAll genTerminal
  let st = finalState timeline & foldEvent sampleSpec.createdAt terminal
  assert (isTerminal st.phase)
  st.doing === Nothing

countsPartitionTheFinishedFiles :: Property
countsPartitionTheFinishedFiles = property $ do
  timeline <- forAll genTimeline
  let st = finalState timeline
      statuses = Map.elems st.files <&> (.status)
      counts = countOutcomes st
      countWhere keep = filter keep statuses & length
  counts.verified + counts.failed + counts.missing + counts.new + counts.replaced === countWhere isDone
  counts.failed + counts.missing === countWhere isFailure

statesOf :: List (UTCTime, JobEvent) -> List JobState
statesOf = scanl (\st step -> foldEvent (fst step) (snd step) st) (newJobState sampleSpec)

finalState :: List (UTCTime, JobEvent) -> JobState
finalState timeline = statesOf timeline & last

isMovement :: JobEvent -> Bool
isMovement = \case
  (Planned _; FileStatusChanged _ _; Progress _; ManifestWriting) -> True
  _ -> False

genTimeline :: Gen (List (UTCTime, JobEvent))
genTimeline = do
  steps <- Gen.list (Range.linear 0 40) ((,) <$> Gen.int (Range.linear 0 5) <*> genEvent)
  let times = steps & map fst & scanl1 (+) & map (\seconds -> addUTCTime (fromIntegral seconds) sampleSpec.createdAt)
  pure (zip times (map snd steps))

genEvent :: Gen JobEvent
genEvent =
  Gen.choice
    [ Gen.list (Range.linear 0 4) ((,) <$> genPath <*> Gen.int64 (Range.linear 0 100))
        <&> \planned -> Planned (PlannedWork (V.fromList planned) (planned & map snd & sum))
    , FileStatusChanged <$> genPath <*> genStatus
    , Progress <$> Gen.int64 (Range.linear 0 1000)
    , pure ManifestWriting
    , pure (MhlWritten [osp|/d/ascmhl/x.mhl|])
    , pure (OriginalsResolved "history" MD5)
    , pure (LogOpened [osp|/state/jobs/one.log|])
    , genTerminal
    ]

genTerminal :: Gen JobEvent
genTerminal =
  Gen.choice
    [ JobFinished <$> Gen.element [AllOk, WithFailures 1]
    , pure (JobFailed "boom")
    ]

genPath :: Gen RelPath
genPath = Gen.element [RelPath "a.mxf", RelPath "b.mxf", RelPath "C/c.mxf"]

genStatus :: Gen FileStatus
genStatus =
  Gen.element
    [ Pending
    , Hashing
    , Copying
    , Flushing
    , Publishing
    , Verifying
    , Done Ok
    , Done (HashMismatch mismatch)
    , Done Missing
    , Done New
    , Done (IoError "boom")
    , Done (Replaced mismatch)
    ]
  where
    mismatch = Mismatch sampleHash sampleHash
