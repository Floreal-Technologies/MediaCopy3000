{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Domain.JobTest (tests) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath ((</>))
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.Job

tests :: TestTree
tests =
  testGroup
    "Domain.Job"
    [ testCase "jobKind and jobLabel derive from the job payload" jobKindAndLabelDeriveFromJob
    , testGroup
        "foldEvent"
        [ testCase "records planned files as Pending and sums total bytes" recordsPlannedFilesAsPendingAndSumsTotalBytes
        , testCase "sets phase from JobFinished" setsPhaseFromJobFinished
        , testCase "sets phase from JobFailed" setsPhaseFromJobFailed
        , testCase "appends MhlWritten paths" appendsMhlWrittenPaths
        , testCase "records the origins used" foldEventRecordsTheOriginsUsed
        , testCase "keeps the log path" logOpenedIsKept
        , testProperty "never decreases bytesDone" bytesDoneNeverDecreases
        , testCase "a byte count stamps the liveness time" aByteCountStampsTheLivenessTime
        , testCase "a manifest leaves the liveness time alone" aManifestLeavesTheLivenessTimeAlone
        , testCase "the start of a manifest stamps the liveness time" aManifestStartStampsTheLivenessTime
        , testCase "an end clears what the job had in hand" anEndClearsWhatTheJobHadInHand
        ]
    , testGroup
        "countOutcomes"
        [ testCase "counts each category" countsEachCategory
        ]
    , testGroup
        "destinationPath"
        [ testCase "appends the source basename" appendsTheSourceBasename
        ]
    ]

jobKindAndLabelDeriveFromJob :: Assertion
jobKindAndLabelDeriveFromJob = do
  jobKind (Offload sampleOffload) @?= OffloadKind
  jobKind (VerifyFolder sampleVerify) @?= VerifyKind
  jobLabel (Offload sampleOffload) @?= "B003_C011"
  jobLabel (VerifyFolder sampleVerify) @?= "Day03"

recordsPlannedFilesAsPendingAndSumsTotalBytes :: Assertion
recordsPlannedFilesAsPendingAndSumsTotalBytes = do
  let st = fold (Planned (PlannedWork (V.fromList [(RelPath "a.mxf", 10), (RelPath "b.mxf", 5)]) 15)) (newJobState sampleSpec)
  Map.toList st.files @?= [(RelPath "a.mxf", FileEntry 10 Pending), (RelPath "b.mxf", FileEntry 5 Pending)]
  st.bytesTotal @?= 15

setsPhaseFromJobFinished :: Assertion
setsPhaseFromJobFinished = do
  let st = fold (JobFinished AllOk) (newJobState sampleSpec)
  st.phase @?= Finished AllOk

setsPhaseFromJobFailed :: Assertion
setsPhaseFromJobFailed = do
  let st = fold (JobFailed "boom") (newJobState sampleSpec)
  st.phase @?= Failed "boom"

appendsMhlWrittenPaths :: Assertion
appendsMhlWrittenPaths = do
  let st = fold (MhlWritten [osp|/d/ascmhl/x.mhl|]) (newJobState sampleSpec)
  st.mhlPaths @?= V.singleton [osp|/d/ascmhl/x.mhl|]

foldEventRecordsTheOriginsUsed :: Assertion
foldEventRecordsTheOriginsUsed = do
  let st = fold (OriginalsResolved "the media source's own history" MD5) (newJobState sampleSpec)
  st.originsUsed @?= Just "the media source's own history"

logOpenedIsKept :: Assertion
logOpenedIsKept = do
  let st = fold (LogOpened [osp|/state/jobs/one.log|]) (newJobState sampleSpec)
  st.logPath @?= Just [osp|/state/jobs/one.log|]

bytesDoneNeverDecreases :: Property
bytesDoneNeverDecreases = property $ do
  steps <- forAll $ Gen.list (Range.linear 0 50) (Gen.int64 (Range.linear 0 1000))
  let evs = map Progress steps
      states = scanl (flip fold) (newJobState sampleSpec) evs
      dones = map (\s -> s.bytesDone) states
  zipWith (<=) dones (drop 1 dones) & and & assert

countsEachCategory :: Assertion
countsEachCategory = do
  let st0 =
        ["a", "b", "c", "d", "e", "f"]
          & map (\p -> (RelPath p, 1))
          & V.fromList
          & (\fs -> Planned (PlannedWork fs 6))
          & flip fold (newJobState sampleSpec)
      st =
        foldl
          (flip fold)
          st0
          [ FileStatusChanged (RelPath "a") (Done Ok)
          , FileStatusChanged (RelPath "b") (Done (HashMismatch (Mismatch sampleHash sampleHash)))
          , FileStatusChanged (RelPath "c") (Done Missing)
          , FileStatusChanged (RelPath "d") (Done New)
          , FileStatusChanged (RelPath "e") Copying
          ]
  countOutcomes st @?= Counts {verified = 1, failed = 1, missing = 1, new = 1, replaced = 0}

appendsTheSourceBasename :: Assertion
appendsTheSourceBasename =
  destinationPath [osp|/mnt/ssd/Day03|] [osp|/media/CARD_B/B003_C011|] @?= [osp|/mnt/ssd/Day03|] </> [osp|B003_C011|]

aByteCountStampsTheLivenessTime :: Assertion
aByteCountStampsTheLivenessTime = do
  let later = addUTCTime 30 sampleSpec.createdAt
      st = foldEvent later (Progress 5) (newJobState sampleSpec)
  st.lastMovedAt @?= later

aManifestLeavesTheLivenessTimeAlone :: Assertion
aManifestLeavesTheLivenessTimeAlone = do
  let later = addUTCTime 30 sampleSpec.createdAt
      st = foldEvent later (MhlWritten [osp|/d/ascmhl/x.mhl|]) (newJobState sampleSpec)
  st.lastMovedAt @?= sampleSpec.createdAt

aManifestStartStampsTheLivenessTime :: Assertion
aManifestStartStampsTheLivenessTime = do
  let later = addUTCTime 30 sampleSpec.createdAt
      st = foldEvent later ManifestWriting (newJobState sampleSpec)
  st.lastMovedAt @?= later
  st.doing @?= Just WritingManifest

anEndClearsWhatTheJobHadInHand :: Assertion
anEndClearsWhatTheJobHadInHand = do
  let writing = fold ManifestWriting (newJobState sampleSpec)
  (fold (JobFinished AllOk) writing).doing @?= Nothing
  (fold (JobFailed "boom") writing).doing @?= Nothing

fold :: JobEvent -> JobState -> JobState
fold = foldEvent sampleSpec.createdAt

sampleSpec :: JobSpec
sampleSpec =
  JobSpec
    { jobId = JobId 1
    , job = Offload OffloadJob {source = [osp|/src|], destinations = [osp|/dst|] :| [], sealFirst = UseHistory, existingCopy = Nothing}
    , createdAt = UTCTime (fromGregorian 2026 9 9) 0
    }

sampleOffload :: OffloadJob
sampleOffload = OffloadJob {source = [osp|/media/CARD_B/B003_C011|], destinations = [osp|/d|] :| [], sealFirst = UseHistory, existingCopy = Nothing}

sampleVerify :: VerifyJob
sampleVerify = VerifyJob {folder = [osp|/vol/Day03|]}

sampleHash :: Hash
sampleHash = Hash XXH64 "ef46db3751d8e999"
