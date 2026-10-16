module Ascmhl.PathTest (tests) where

import Data.Foldable (for_)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Ascmhl.Generators (genSegments)
import Ascmhl.Path (RelPath (..), mkRelPath)

tests :: TestTree
tests =
  testGroup
    "Ascmhl.Path"
    [ testProperty "mkRelPath accepts a relative path and refuses each way out of the folder" mkRelPathAcceptsRelativeAndRefusesEscapes
    ]

mkRelPathAcceptsRelativeAndRefusesEscapes :: Property
mkRelPathAcceptsRelativeAndRefusesEscapes = property $ do
  segments <- forAll genSegments
  separator <- forAll (Gen.element ["/", "\\"])
  letter <- forAll Gen.alpha
  at <- forAll (Gen.int (Range.linear 0 (length segments)))
  let joined parts = T.intercalate separator parts
      safe = joined segments
      drive = T.cons letter ":"
      driveInside = joined (segments <> [drive])
  mkRelPath safe === Just (RelPath safe)
  mkRelPath driveInside === Just (RelPath driveInside)
  for_
    ["", separator <> safe, drive <> safe, joined (take at segments <> [".."] <> drop at segments)]
    (\escape -> mkRelPath escape === Nothing)
