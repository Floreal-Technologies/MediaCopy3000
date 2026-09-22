module MediaCopy.Domain.DirectoryHashTest (tests) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.DirectoryHash
import MediaCopy.Domain.JobFormat.Internal (JobFormat (..))
import MediaCopy.Effects.Hasher

tests :: TestTree
tests =
  testGroup
    "Domain.DirectoryHash"
    [ testCase "matches the reference on a nested tree" matchesTheReferenceOnANestedTree
    , testCase "hashes an empty directory as the digest of nothing" hashesAnEmptyDirectoryAsTheDigestOfNothing
    ]

sampleTree :: DirNode
sampleTree =
  buildTree
    ( V.fromList
        [ (RelPath "A/B/f2.txt", Hash XXH64 "b1f86c748074bc74")
        , (RelPath "A/f1.txt", Hash XXH64 "5c80c09683041123")
        , (RelPath "top.txt", Hash XXH64 "6d85d478e2fa354b")
        ]
    )
    (V.fromList [RelPath "A", RelPath "A/B", RelPath "Empty"])

runHashes :: JobFormat -> DirNode -> IO (DirHashes, Vector (RelPath, DirHashes))
runHashes fmt node =
  runEff (runHasherIO (runErrorNoCallStack @DirectoryHashError (directoryHashes (hashBytes fmt) node))) >>= \case
    Left e -> assertFailure (T.unpack (display e))
    Right pairs -> pure pairs

matchesTheReferenceOnANestedTree :: Assertion
matchesTheReferenceOnANestedTree = do
  (rootPair, rows) <- runHashes (JobFormat XXH64) sampleTree
  V.map (\row -> fst row) rows @?= V.fromList [RelPath "A/B", RelPath "A", RelPath "Empty"]
  V.map (\row -> (snd row).content) rows
    @?= V.fromList [Hash XXH64 "d6ea6f397b524900", Hash XXH64 "e1d1abb1814a3c33", Hash XXH64 "ef46db3751d8e999"]
  V.map (\row -> (snd row).structure) rows
    @?= V.fromList [Hash XXH64 "5374a30a8b609b13", Hash XXH64 "c36151e1b3c3a22d", Hash XXH64 "ef46db3751d8e999"]
  rootPair.content @?= Hash XXH64 "d39d6fe24c72be31"
  rootPair.structure @?= Hash XXH64 "ae3733e7523bd835"

hashesAnEmptyDirectoryAsTheDigestOfNothing :: Assertion
hashesAnEmptyDirectoryAsTheDigestOfNothing = do
  (rootPair, _rows) <- runHashes (JobFormat XXH64) (buildTree V.empty V.empty)
  rootPair.content @?= Hash XXH64 "ef46db3751d8e999"
  rootPair.structure @?= Hash XXH64 "ef46db3751d8e999"
