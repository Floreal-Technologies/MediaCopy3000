module Ascmhl.PathTest (tests) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Ascmhl.Path (mkRelPath)

tests :: TestTree
tests =
  testGroup
    "Path"
    [ testCase "accepts a plain relative path" acceptsAPlainRelativePath
    , testCase "accepts a backslash inside a name" acceptsABackslashInsideAName
    , testCase "refuses an empty path" refusesAnEmptyPath
    , testCase "refuses a leading slash of either kind" refusesALeadingSlashOfEitherKind
    , testCase "refuses a drive letter" refusesADriveLetter
    , testCase "refuses a dot-dot between slashes of either kind" refusesADotDotBetweenSlashesOfEitherKind
    ]

accepted :: Text -> Assertion
accepted t = assertBool ("expected to accept " <> show t) (isJust (mkRelPath t))

refused :: Text -> Assertion
refused t = assertBool ("expected to refuse " <> show t) (isNothing (mkRelPath t))

acceptsAPlainRelativePath :: Assertion
acceptsAPlainRelativePath = do
  accepted "a.mxf"
  accepted "Clips/A001C005.mov"
  accepted "0001_CARD_A001_2026-09-12_140300.mhl"

acceptsABackslashInsideAName :: Assertion
acceptsABackslashInsideAName = do
  accepted "back\\slash.mxf"
  accepted "clip/back\\slash.mxf"

refusesAnEmptyPath :: Assertion
refusesAnEmptyPath = refused T.empty

refusesALeadingSlashOfEitherKind :: Assertion
refusesALeadingSlashOfEitherKind = do
  refused "/etc/passwd"
  refused "\\top"

refusesADriveLetter :: Assertion
refusesADriveLetter = do
  refused "C:2024"
  refused "c:/media"
  accepted "clips/C:2024"

refusesADotDotBetweenSlashesOfEitherKind :: Assertion
refusesADotDotBetweenSlashesOfEitherKind = do
  refused ".."
  refused "a/.."
  refused "../b"
  refused "a\\..\\b"
  refused "..\\b"
  accepted "a/..b"
  accepted "a/b.."
