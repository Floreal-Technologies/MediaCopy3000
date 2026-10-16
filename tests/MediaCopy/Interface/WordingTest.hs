module MediaCopy.Interface.WordingTest (tests) where

import Ascmhl.Path (RelPath (..))
import Data.Maybe (isJust)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.Job
import MediaCopy.Interface.Translation.Embedded
import MediaCopy.Interface.Wording
import MediaCopy.Test.InMemoryFS (sampleSpec)

tests :: TestTree
tests =
  testGroup
    "Interface.Wording"
    [ testProperty "quietText speaks iff unfinished work is in hand and 2 s passed" quietTextSpeaksOnlyWhenStuck
    ]

quietTextSpeaksOnlyWhenStuck :: Property
quietTextSpeaksOnlyWhenStuck = property $ do
  language <- forAll Gen.enumBounded
  lastEvent <- forAll genLastEvent
  millis <- forAll (Gen.frequency [(1, Gen.element [1_999, 2_000]), (4, Gen.int (Range.linear 0 300_000))])
  let state = maybe planned (\event -> foldEvent at event planned) lastEvent
      elapsed = fromIntegral millis / 1_000
      inHand = case state.doing of
        Just (OnFile status) -> not (isDone status)
        Just WritingManifest -> True
        Nothing -> False
  isJust (quietText (embeddedWording language) (addUTCTime elapsed at) state) === (inHand && elapsed >= 2)

genLastEvent :: Gen (Maybe JobEvent)
genLastEvent =
  Gen.maybe
    ( Gen.choice
        [ pure ManifestWriting
        , FileStatusChanged (RelPath "A/1.mxf")
            <$> Gen.element [Pending, Hashing, Copying, Flushing, Publishing, Verifying, Done Ok, Done Missing, Done (IoError "boom")]
        ]
    )

at :: UTCTime
at = UTCTime (fromGregorian 2026 9 15) 0

planned :: JobState
planned = foldEvent at (Planned (PlannedWork (V.singleton (RelPath "A/1.mxf", 100)) 300)) (newJobState sampleSpec {createdAt = at})
