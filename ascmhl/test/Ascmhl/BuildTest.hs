module Ascmhl.BuildTest (tests) where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (List, sort, sortOn, tails)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import Ascmhl.Build
import Ascmhl.Generators
import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Ascmhl.Types

tests :: TestTree
tests =
  testGroup
    "Ascmhl.Build"
    [ testProperty "orderedEntries lists each entry once, each directory after its contents" directoriesComeAfterTheirContents
    , testProperty "appendGeneration keeps the old entries in order and adds the new one last" appendKeepsTheOldEntries
    , testCase "latestHashes prefers the later generation, then xxh64" latestHashesPrefersLaterThenXxh64
    ]

instant :: UTCTime
instant = UTCTime (fromGregorian 2026 9 14) 32400

directoriesComeAfterTheirContents :: Property
directoriesComeAfterTheirContents = property $ do
  paths <- forAll (Gen.set (Range.linear 1 12) (Gen.list (Range.linear 1 3) (Gen.element ["a", "b", "a.x", "a-1"]) <&> T.intercalate "/"))
  let files = Set.filter (\p -> not (any (\q -> (p <> "/") `T.isPrefixOf` q) paths)) paths
      dirs = foldMap ancestors files
      fileOf p = fileEntry (RelPath p) 0 instant (Hash XXH64 "ef46db3751d8e999") Original instant
      dirOf p = directoryEntry (RelPath p) instant (dirHash instant (Hash XXH64 "8d02114c32e28cbe") (Hash XXH64 "f557f8ca8e5a88ef"))
      ordered = V.toList (orderedEntries (V.fromList (map fileOf (Set.toList files))) (V.fromList (map dirOf (Set.toList dirs))))
  sort (map entryPath ordered) === sort (Set.toList files <> Set.toList dirs)
  for_
    (tails ordered)
    ( \case
        ManifestDir d : later -> assert (not (any (\e -> (display d.path <> "/") `T.isPrefixOf` entryPath e) later))
        _ -> success
    )

ancestors :: Text -> Set.Set Text
ancestors p =
  let segments = T.splitOn "/" p
  in Set.fromList (map (\n -> T.intercalate "/" (take n segments)) [1 .. length segments - 1])

entryPath :: ManifestEntry -> Text
entryPath = \case
  ManifestFile e -> display e.path
  ManifestDir d -> display d.path

appendKeepsTheOldEntries :: Property
appendKeepsTheOldEntries = property $ do
  numbers <- forAll (Gen.set (Range.linear 0 6) (Gen.int (Range.linear 1 100)))
  given <- forAll (traverse (\n -> chainEntry n <$> genRelPath <*> Gen.maybe (genHashOf C4)) (Set.toList numbers) >>= Gen.shuffle)
  name <- forAll genRelPath
  c4 <- forAll (genHashOf C4)
  let next = highestGeneration (Chain (V.fromList given)) + 1
      appended = appendGeneration (Chain (V.fromList given)) next name c4
  V.toList appended.entries === sortOn (.sequenceNr) given <> [chainEntry next name (Just c4)]

latestHashesPrefersLaterThenXxh64 :: Assertion
latestHashesPrefersLaterThenXxh64 = do
  let a = RelPath "a.mxf"
      b = RelPath "b.mxf"
      c = RelPath "c.mxf"
      md5A = Hash MD5 "d41d8cd98f00b204e9800998ecf8427e"
      xxhA = Hash XXH64 "ef46db3751d8e999"
      xxhB = Hash XXH64 "1111111111111111"
      md5B = Hash MD5 "0123456789abcdef0123456789abcdef"
      sha1C = Hash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      earlier = manifestOf [entryWith a [md5A, xxhA], entryWith b [xxhB], entryWith c [sha1C]]
      later = manifestOf [entryWith b [md5B]]
  latestHashes (V.fromList [earlier, later]) @?= Map.fromList [(a, xxhA), (b, md5B), (c, sha1C)]

entryWith :: RelPath -> List Hash -> HashEntry
entryWith path hashes =
  HashEntry
    { path
    , size = 0
    , lastModified = instant
    , hashes = V.fromList (map (\hash -> ManifestHash {hash, action = Original, hashDate = Nothing, extraAttrs = Map.empty}) hashes)
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

manifestOf :: List HashEntry -> Manifest
manifestOf entries =
  newManifest
    (creatorInfo instant "studio-01" "mediacopy3000" "0.1.0.0")
    ProcessTransfer
    (dirHash instant (Hash XXH64 "8d02114c32e28cbe") (Hash XXH64 "f557f8ca8e5a88ef"))
    V.empty
    (V.fromList (map ManifestFile entries))
