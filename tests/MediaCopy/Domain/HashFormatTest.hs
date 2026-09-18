module MediaCopy.Domain.HashFormatTest (tests) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.HashFormat qualified as HashFormat

tests :: TestTree
tests =
  testGroup
    "Domain.HashFormat"
    [ testCase "the format comes from the present originals, else from all originals, and mixed formats are refused" formatComesFromTheOriginals
    ]

formatComesFromTheOriginals :: Assertion
formatComesFromTheOriginals =
  for_
    [ (Map.fromList [(RelPath.RelPath "a.mxf", Hash.Hash {algo = Algo.SHA1, value = "ab"})], Set.fromList [RelPath.RelPath "b.mxf"], Right Algo.SHA1)
    ,
      ( Map.fromList [(RelPath.RelPath "gone.mxf", Hash.Hash {algo = Algo.MD5, value = "0f"}), (RelPath.RelPath "here.mxf", Hash.Hash {algo = Algo.XXH64, value = "ab"})]
      , Set.fromList [RelPath.RelPath "here.mxf"]
      , Right Algo.XXH64
      )
    ,
      ( Map.fromList [(RelPath.RelPath "a.mxf", Hash.Hash {algo = Algo.MD5, value = "0f"}), (RelPath.RelPath "b.mxf", Hash.Hash {algo = Algo.SHA1, value = "ab"})]
      , Set.fromList [RelPath.RelPath "a.mxf", RelPath.RelPath "b.mxf"]
      , Left (HashFormat.MixedFormats [Algo.MD5, Algo.SHA1])
      )
    ]
    (\(expected, present, settled) -> HashFormat.settle expected present @?= settled)
