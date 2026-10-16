module MediaCopy.Domain.DirectoryHashTest (tests) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.DirectoryHash
import MediaCopy.Effects.Hasher

tests :: TestTree
tests =
  testGroup
    "Domain.DirectoryHash"
    [ testCase "matches the reference on a nested tree" matchesTheReferenceOnANestedTree
    ]

matchesTheReferenceOnANestedTree :: Assertion
matchesTheReferenceOnANestedTree = do
  let tree =
        buildTree
          ( V.fromList
              [ (RelPath "A/B/f2.txt", Hash XXH64 "b1f86c748074bc74")
              , (RelPath "A/f1.txt", Hash XXH64 "5c80c09683041123")
              , (RelPath "top.txt", Hash XXH64 "6d85d478e2fa354b")
              ]
          )
          (V.fromList [RelPath "A", RelPath "A/B", RelPath "Empty"])
  runEff (runHasher (runErrorNoCallStack @DirectoryHashError (directoryHashes (hashBytes XXH64) tree))) >>= \case
    Left e -> assertFailure (T.unpack (display e))
    Right (root, rows) ->
      Map.insert (RelPath "") root (Map.fromList (V.toList rows))
        @?= Map.fromList
          [ (RelPath "", DirHashes (Hash XXH64 "d39d6fe24c72be31") (Hash XXH64 "ae3733e7523bd835"))
          , (RelPath "A", DirHashes (Hash XXH64 "e1d1abb1814a3c33") (Hash XXH64 "c36151e1b3c3a22d"))
          , (RelPath "A/B", DirHashes (Hash XXH64 "d6ea6f397b524900") (Hash XXH64 "5374a30a8b609b13"))
          , (RelPath "Empty", DirHashes (Hash XXH64 "ef46db3751d8e999") (Hash XXH64 "ef46db3751d8e999"))
          ]
