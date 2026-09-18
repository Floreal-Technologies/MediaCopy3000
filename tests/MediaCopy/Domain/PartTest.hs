module MediaCopy.Domain.PartTest (tests) where

import Ascmhl.RelPath qualified as RelPath
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Part qualified as Part

tests :: TestTree
tests =
  testGroup
    "Domain.Part"
    [ testCase "a path that is not a part copy of a named file strips to nothing" notAPartCopyStripsToNothing
    ]

notAPartCopyStripsToNothing :: Assertion
notAPartCopyStripsToNothing =
  for_
    [RelPath.RelPath "card/a.mxf", RelPath.RelPath ".mc3k-part"]
    (\path -> Part.strip path @?= Nothing)
