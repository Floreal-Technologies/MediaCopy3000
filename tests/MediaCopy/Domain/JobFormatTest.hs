module MediaCopy.Domain.JobFormatTest (tests) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Display (display)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.JobFormat

tests :: TestTree
tests =
  testGroup
    "Domain.JobFormat"
    [ testCase "takes the format of the files the job will process" takesTheFormatOfTheFilesTheJobWillProcess
    , testCase "ignores a sealed path that is no longer present" ignoresASealedPathThatIsNoLongerPresent
    , testCase "falls back to the whole originals when none are present" fallsBackToTheWholeOriginalsWhenNoneArePresent
    , testCase "falls back to xxh64 when there are no originals" fallsBackToXxh64WhenThereAreNoOriginals
    , testCase "rejects a mix among the files it will process" rejectsAMixAmongTheFilesItWillProcess
    ]

takesTheFormatOfTheFilesTheJobWillProcess :: Assertion
takesTheFormatOfTheFilesTheJobWillProcess = do
  let expected = Map.fromList [(RelPath "a.mxf", Hash MD5 "5d41402abc4b2a76b9719d911017c592")]
  fmap formatAlgo (settleFormat expected (Set.fromList [RelPath "a.mxf"])) @?= Right MD5

ignoresASealedPathThatIsNoLongerPresent :: Assertion
ignoresASealedPathThatIsNoLongerPresent = do
  let expected =
        Map.fromList
          [ (RelPath "gone.mxf", Hash MD5 "5d41402abc4b2a76b9719d911017c592")
          , (RelPath "here.mxf", Hash XXH64 "26c7827d889f6da3")
          ]
  fmap formatAlgo (settleFormat expected (Set.fromList [RelPath "here.mxf"])) @?= Right XXH64

fallsBackToTheWholeOriginalsWhenNoneArePresent :: Assertion
fallsBackToTheWholeOriginalsWhenNoneArePresent = do
  let expected = Map.fromList [(RelPath "gone.mxf", Hash MD5 "5d41402abc4b2a76b9719d911017c592")]
  fmap formatAlgo (settleFormat expected (Set.fromList [RelPath "fresh.mxf"])) @?= Right MD5

fallsBackToXxh64WhenThereAreNoOriginals :: Assertion
fallsBackToXxh64WhenThereAreNoOriginals =
  fmap formatAlgo (settleFormat Map.empty (Set.fromList [RelPath "fresh.mxf"])) @?= Right XXH64

rejectsAMixAmongTheFilesItWillProcess :: Assertion
rejectsAMixAmongTheFilesItWillProcess = do
  let expected =
        Map.fromList
          [ (RelPath "a.mxf", Hash MD5 "5d41402abc4b2a76b9719d911017c592")
          , (RelPath "b.mxf", Hash XXH64 "26c7827d889f6da3")
          ]
      result = settleFormat expected (Set.fromList [RelPath "a.mxf", RelPath "b.mxf"])
  assertBool "a mixed set of originals was accepted" (isLeft result)
  either
    (\e -> assertBool "the message does not name both formats" (T.isInfixOf "xxh64" (display e) && T.isInfixOf "md5" (display e)))
    (\_ -> pure ())
    result
