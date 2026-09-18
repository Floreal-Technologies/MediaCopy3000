{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Domain.JobTest (tests) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Test.InMemoryFS qualified as InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Domain.Job"
    [ testProperty "the liveness time is the time of the last movement" livenessTimeIsTheLastMovement
    ]

livenessTimeIsTheLastMovement :: Property
livenessTimeIsTheLastMovement = property $ do
  timeline <- forAll genTimeline
  let expected =
        timeline
          & filter (\step -> isMovement (snd step))
          & map fst
          & foldl (\_ at -> at) InMemoryFS.sampleSpec.createdAt
  (finalState timeline).lastMovedAt === expected

statesOf :: List (UTCTime, JobEvent) -> List JobState
statesOf = scanl (\st step -> JobState.apply (fst step) (snd step) st) (JobState.new InMemoryFS.sampleSpec)

finalState :: List (UTCTime, JobEvent) -> JobState
finalState timeline = statesOf timeline & last

isMovement :: JobEvent -> Bool
isMovement = \case
  (JobEvent.Planned _; JobEvent.FileStatusChanged _ _; JobEvent.Progress _; JobEvent.ManifestWriting) -> True
  _ -> False

genTimeline :: Gen (List (UTCTime, JobEvent))
genTimeline = do
  steps <- Gen.list (Range.linear 0 40) ((,) <$> Gen.int (Range.linear 0 5) <*> genEvent)
  let times = steps & map fst & scanl1 (+) & map (\seconds -> addUTCTime (fromIntegral seconds) InMemoryFS.sampleSpec.createdAt)
  pure (zip times (map snd steps))

genEvent :: Gen JobEvent
genEvent =
  Gen.choice
    [ Gen.list (Range.linear 0 4) ((,) <$> genPath <*> Gen.int64 (Range.linear 0 100))
        <&> \planned -> JobEvent.Planned (JobEvent.PlannedWork (V.fromList planned) (planned & map snd & sum))
    , JobEvent.FileStatusChanged <$> genPath <*> genStatus
    , JobEvent.Progress <$> Gen.int64 (Range.linear 0 1000)
    , pure JobEvent.ManifestWriting
    , pure (JobEvent.MhlWritten [osp|/d/ascmhl/x.mhl|])
    , pure (JobEvent.OriginalsResolved "history" Algo.MD5)
    , pure (JobEvent.LogOpened [osp|/state/jobs/one.log|])
    , genTerminal
    ]

genTerminal :: Gen JobEvent
genTerminal =
  Gen.choice
    [ JobEvent.Finished <$> Gen.element [JobResult.AllOk, JobResult.WithFailures 1]
    , pure (JobEvent.Failed "boom")
    ]

genPath :: Gen RelPath
genPath = Gen.element [RelPath.RelPath "a.mxf", RelPath.RelPath "b.mxf", RelPath.RelPath "C/c.mxf"]

genStatus :: Gen FileStatus
genStatus =
  Gen.element
    [ FileStatus.Pending
    , FileStatus.Hashing
    , FileStatus.Copying
    , FileStatus.Flushing
    , FileStatus.Publishing
    , FileStatus.Verifying
    , FileStatus.Done FileOutcome.Ok
    , FileStatus.Done (FileOutcome.HashMismatch mismatch)
    , FileStatus.Done FileOutcome.Missing
    , FileStatus.Done FileOutcome.New
    , FileStatus.Done (FileOutcome.IoError "boom")
    , FileStatus.Done (FileOutcome.Replaced mismatch)
    ]
  where
    mismatch = FileOutcome.Mismatch InMemoryFS.sampleHash InMemoryFS.sampleHash
