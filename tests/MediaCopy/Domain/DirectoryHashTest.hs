module MediaCopy.Domain.DirectoryHashTest (tests) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.DirectoryHash (DirectoryHashError)
import MediaCopy.Domain.DirectoryHash qualified as DirectoryHash
import MediaCopy.Effects.Hasher qualified as Hasher

tests :: TestTree
tests =
  testGroup
    "Domain.DirectoryHash"
    [ testCase "matches the reference on a nested tree" matchesTheReferenceOnANestedTree
    ]

matchesTheReferenceOnANestedTree :: Assertion
matchesTheReferenceOnANestedTree = do
  let tree =
        DirectoryHash.buildTree
          ( V.fromList
              [ (RelPath.RelPath "A/B/f2.txt", Hash.Hash Algo.XXH64 "b1f86c748074bc74")
              , (RelPath.RelPath "A/f1.txt", Hash.Hash Algo.XXH64 "5c80c09683041123")
              , (RelPath.RelPath "top.txt", Hash.Hash Algo.XXH64 "6d85d478e2fa354b")
              ]
          )
          (V.fromList [RelPath.RelPath "A", RelPath.RelPath "A/B", RelPath.RelPath "Empty"])
  runEff (Hasher.run (runErrorNoCallStack @DirectoryHashError (DirectoryHash.compute (Hasher.hashBytes Algo.XXH64) tree))) >>= \case
    Left e -> assertFailure (T.unpack (display e))
    Right (root, rows) ->
      Map.insert (RelPath.RelPath "") root (Map.fromList (V.toList rows))
        @?= Map.fromList
          [ (RelPath.RelPath "", DirectoryHash.DirectoryHash (Hash.Hash Algo.XXH64 "d39d6fe24c72be31") (Hash.Hash Algo.XXH64 "ae3733e7523bd835"))
          , (RelPath.RelPath "A", DirectoryHash.DirectoryHash (Hash.Hash Algo.XXH64 "e1d1abb1814a3c33") (Hash.Hash Algo.XXH64 "c36151e1b3c3a22d"))
          , (RelPath.RelPath "A/B", DirectoryHash.DirectoryHash (Hash.Hash Algo.XXH64 "d6ea6f397b524900") (Hash.Hash Algo.XXH64 "5374a30a8b609b13"))
          , (RelPath.RelPath "Empty", DirectoryHash.DirectoryHash (Hash.Hash Algo.XXH64 "ef46db3751d8e999") (Hash.Hash Algo.XXH64 "ef46db3751d8e999"))
          ]
