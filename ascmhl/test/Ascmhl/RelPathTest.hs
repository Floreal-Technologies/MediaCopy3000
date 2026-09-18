module Ascmhl.RelPathTest (tests) where

import Data.Foldable (for_)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Ascmhl.Generators qualified as Generators
import Ascmhl.RelPath qualified as RelPath

tests :: TestTree
tests =
  testGroup
    "RelPath"
    [ testProperty "parse accepts a relative path and refuses each way out of the folder" parseAcceptsRelativeAndRefusesEscapes
    ]

parseAcceptsRelativeAndRefusesEscapes :: Property
parseAcceptsRelativeAndRefusesEscapes = property $ do
  segments <- forAll Generators.genSegments
  separator <- forAll (Gen.element ["/", "\\"])
  letter <- forAll Gen.alpha
  at <- forAll (Gen.int (Range.linear 0 (length segments)))
  let joined parts = T.intercalate separator parts
      safe = joined segments
      drive = T.cons letter ":"
      driveInside = joined (segments <> [drive])
  RelPath.parse safe === Just (RelPath.RelPath safe)
  RelPath.parse driveInside === Just (RelPath.RelPath driveInside)
  for_
    ["", separator <> safe, drive <> safe, joined (take at segments <> [".."] <> drop at segments)]
    (\escape -> RelPath.parse escape === Nothing)
