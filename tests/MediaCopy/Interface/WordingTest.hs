{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Interface.WordingTest (tests) where

import Ascmhl.Path (RelPath (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Vector qualified as V
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Job
import MediaCopy.Interface.Wording (quietText)

tests :: TestTree
tests =
  testGroup
    "Interface.Wording"
    [ testGroup
        "quietText"
        [ testCase "says nothing while the job moves" saysNothingWhileTheJobMoves
        , testCase "says nothing under the threshold and speaks at it" speaksAtTheThreshold
        , testCase "names the state of the file in hand" namesTheStateOfTheFileInHand
        , testCase "says nothing when the job holds no file" saysNothingWithNoFileInHand
        , testCase "names the manifest phase, which holds no file" namesTheManifestPhase
        ]
    ]

saysNothingWhileTheJobMoves :: Assertion
saysNothingWhileTheJobMoves =
  quietText (addUTCTime 0.4 at) (inState Flushing) @?= Nothing

speaksAtTheThreshold :: Assertion
speaksAtTheThreshold = do
  quietText (addUTCTime 1.9 at) (inState Flushing) @?= Nothing
  quietText (addUTCTime 2 at) (inState Flushing) @?= Just "Saving to disk · 2 s"

namesTheStateOfTheFileInHand :: Assertion
namesTheStateOfTheFileInHand = do
  quietText (addUTCTime 14 at) (inState Publishing) @?= Just "Naming the copy · 14 s"
  quietText (addUTCTime 245 at) (inState Copying) @?= Just "Copying · 4 min 05 s"

saysNothingWithNoFileInHand :: Assertion
saysNothingWithNoFileInHand = do
  quietText (addUTCTime 25 at) planned @?= Nothing
  quietText (addUTCTime 25 at) (inState (Done Ok)) @?= Nothing

namesTheManifestPhase :: Assertion
namesTheManifestPhase = do
  let writing = foldEvent at ManifestWriting (inState (Done Ok))
  quietText (addUTCTime 1.9 at) writing @?= Nothing
  quietText (addUTCTime 25 at) writing @?= Just "Writing the manifest · 25 s"

at :: UTCTime
at = UTCTime (fromGregorian 2026 9 15) 0

planned :: JobState
planned = foldEvent at (Planned (PlannedWork (V.singleton (RelPath "A/1.mxf", 100)) 300)) (newJobState spec)

inState :: FileStatus -> JobState
inState status = foldEvent at (FileStatusChanged (RelPath "A/1.mxf") status) planned

spec :: JobSpec
spec =
  JobSpec
    { jobId = JobId 1
    , job = Offload OffloadJob {source = [osp|/src|], destinations = [osp|/dst|] :| [], sealFirst = UseHistory, existingCopy = Nothing}
    , createdAt = at
    }
