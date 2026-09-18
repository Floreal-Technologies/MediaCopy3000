module MediaCopy.Interface.WordingTest (tests) where

import Ascmhl.RelPath qualified as RelPath
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Interface.Translation (SupportedLanguage (..))
import MediaCopy.Interface.Translation.Embedded
import MediaCopy.Interface.Wording
import MediaCopy.Test.InMemoryFS qualified as InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Interface.Wording"
    [ testProperty "quietText speaks iff unfinished work is in hand and 2 s passed" quietTextSpeaksOnlyWhenStuck
    , testCase "humanBytes uses decimal units with one decimal from MB up" humanBytesUsesDecimalUnits
    , testCase "humanRate shows bytes per second and 0 B/s for non-positive rates" humanRateShowsBytesPerSecond
    , testCase "humanEta shows the two largest units and 0 s for negative times" humanEtaShowsTheTwoLargestUnits
    ]

humanBytesUsesDecimalUnits :: Assertion
humanBytesUsesDecimalUnits =
  map (\(bytes, _) -> humanBytes (embeddedWording English) bytes) table @?= map snd table
  where
    table :: List (Int64, Text)
    table =
      [ (0, "0 B")
      , (999, "999 B")
      , (1_000, "1 KB")
      , (12_288, "12 KB")
      , (999_499, "999 KB")
      , (999_500, "1.0 MB")
      , (95_600_000_000, "95.6 GB")
      , (2_500_000_000_000, "2.5 TB")
      ]

humanRateShowsBytesPerSecond :: Assertion
humanRateShowsBytesPerSecond =
  map (\(rate, _) -> humanRate (embeddedWording English) rate) table @?= map snd table
  where
    table :: List (Double, Text)
    table =
      [ (1.1e9, "1.1 GB/s")
      , (0, "0 B/s")
      , (0 / 0, "0 B/s")
      ]

humanEtaShowsTheTwoLargestUnits :: Assertion
humanEtaShowsTheTwoLargestUnits =
  map (\(seconds, _) -> humanEta (embeddedWording English) seconds) table @?= map snd table
  where
    table :: List (Double, Text)
    table =
      [ (34, "34 s")
      , (125, "2 min 05 s")
      , (3_720, "1 h 02 min")
      , (-1, "0 s")
      ]

quietTextSpeaksOnlyWhenStuck :: Property
quietTextSpeaksOnlyWhenStuck = property $ do
  language <- forAll Gen.enumBounded
  lastEvent <- forAll genLastEvent
  millis <- forAll (Gen.frequency [(1, Gen.element [1_999, 2_000]), (4, Gen.int (Range.linear 0 300_000))])
  let state = maybe planned (\event -> JobState.apply at event planned) lastEvent
      elapsed = fromIntegral millis / 1_000
      inHand = case state.doing of
        Just (JobState.OnFile status) -> not (FileStatus.isDone status)
        Just JobState.WritingManifest -> True
        Nothing -> False
  isJust (quietText (embeddedWording language) (addUTCTime elapsed at) state) === (inHand && elapsed >= 2)

genLastEvent :: Gen (Maybe JobEvent)
genLastEvent =
  Gen.maybe
    ( Gen.choice
        [ pure JobEvent.ManifestWriting
        , JobEvent.FileStatusChanged (RelPath.RelPath "A/1.mxf")
            <$> Gen.element [FileStatus.Pending, FileStatus.Hashing, FileStatus.Copying, FileStatus.Flushing, FileStatus.Publishing, FileStatus.Verifying, FileStatus.Done FileOutcome.Ok, FileStatus.Done FileOutcome.Missing, FileStatus.Done (FileOutcome.IoError "boom")]
        ]
    )

at :: UTCTime
at = UTCTime (fromGregorian 2026 9 15) 0

planned :: JobState
planned = JobState.apply at (JobEvent.Planned (JobEvent.PlannedWork (V.singleton (RelPath.RelPath "A/1.mxf", 100)) 300)) (JobState.new InMemoryFS.sampleSpec {JobSpec.createdAt = at})
