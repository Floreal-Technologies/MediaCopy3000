module Ascmhl.ManifestTest (tests) where

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

import Ascmhl.Algo qualified as Algo
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.Creator qualified as Creator
import Ascmhl.DirHash qualified as DirHash
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry (FileEntry)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Generators qualified as Generators
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry (ManifestEntry)
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.Process qualified as Process
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath

tests :: TestTree
tests =
  testGroup
    "Manifest"
    [ testProperty "ManifestEntry.ordered lists each entry once, each directory after its contents" directoriesComeAfterTheirContents
    , testProperty "Chain.append keeps the old entries in order and adds the new one last" appendKeepsTheOldEntries
    , testCase "latestHashes prefers the later generation, then xxh64" latestHashesPrefersLaterThenXxh64
    ]

instant :: UTCTime
instant = UTCTime (fromGregorian 2026 9 14) 32400

directoriesComeAfterTheirContents :: Property
directoriesComeAfterTheirContents = property $ do
  paths <- forAll (Gen.set (Range.linear 1 12) (Gen.list (Range.linear 1 3) (Gen.element ["a", "b", "a.x", "a-1"]) <&> T.intercalate "/"))
  let files = Set.filter (\p -> not (any (\q -> (p <> "/") `T.isPrefixOf` q) paths)) paths
      dirs = foldMap ancestors files
      fileOf p = FileEntry.new (RelPath.RelPath p) 0 instant (Hash.Hash Algo.XXH64 "ef46db3751d8e999") FileEntry.Original instant
      dirOf p = DirectoryEntry.new (RelPath.RelPath p) instant (DirHash.new instant (Hash.Hash Algo.XXH64 "8d02114c32e28cbe") (Hash.Hash Algo.XXH64 "f557f8ca8e5a88ef"))
      ordered = V.toList (ManifestEntry.ordered (V.fromList (map fileOf (Set.toList files))) (V.fromList (map dirOf (Set.toList dirs))))
  sort (map entryPath ordered) === sort (Set.toList files <> Set.toList dirs)
  for_
    (tails ordered)
    ( \case
        ManifestEntry.Directory d : later -> assert (not (any (\e -> (display d.path <> "/") `T.isPrefixOf` entryPath e) later))
        _ -> success
    )

ancestors :: Text -> Set.Set Text
ancestors p =
  let segments = T.splitOn "/" p
  in Set.fromList (map (\n -> T.intercalate "/" (take n segments)) [1 .. length segments - 1])

entryPath :: ManifestEntry -> Text
entryPath = \case
  ManifestEntry.File e -> display e.path
  ManifestEntry.Directory d -> display d.path

appendKeepsTheOldEntries :: Property
appendKeepsTheOldEntries = property $ do
  numbers <- forAll (Gen.set (Range.linear 0 6) (Gen.int (Range.linear 1 100)))
  given <- forAll (traverse (\n -> ChainEntry.new n <$> Generators.genRelPath <*> Gen.maybe (Generators.genHashOf Algo.C4)) (Set.toList numbers) >>= Gen.shuffle)
  name <- forAll Generators.genRelPath
  c4 <- forAll (Generators.genHashOf Algo.C4)
  let next = Chain.highestGeneration (Chain.Chain (V.fromList given)) + 1
      appended = Chain.append (Chain.Chain (V.fromList given)) next name c4
  V.toList appended.entries === sortOn (.sequenceNr) given <> [ChainEntry.new next name (Just c4)]

latestHashesPrefersLaterThenXxh64 :: Assertion
latestHashesPrefersLaterThenXxh64 = do
  let a = RelPath.RelPath "a.mxf"
      b = RelPath.RelPath "b.mxf"
      c = RelPath.RelPath "c.mxf"
      md5A = Hash.Hash Algo.MD5 "d41d8cd98f00b204e9800998ecf8427e"
      xxhA = Hash.Hash Algo.XXH64 "ef46db3751d8e999"
      xxhB = Hash.Hash Algo.XXH64 "1111111111111111"
      md5B = Hash.Hash Algo.MD5 "0123456789abcdef0123456789abcdef"
      sha1C = Hash.Hash Algo.SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      earlier = manifestOf [entryWith a [md5A, xxhA], entryWith b [xxhB], entryWith c [sha1C]]
      later = manifestOf [entryWith b [md5B]]
  Manifest.latestHashes (V.fromList [earlier, later]) @?= Map.fromList [(a, xxhA), (b, md5B), (c, sha1C)]

entryWith :: RelPath -> List Hash -> FileEntry
entryWith path hashes =
  FileEntry.FileEntry
    { path
    , size = 0
    , lastModified = instant
    , hashes = V.fromList (map (\hash -> FileEntry.HashRecord {hash, action = FileEntry.Original, hashDate = Nothing, extraAttrs = Map.empty}) hashes)
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

manifestOf :: List FileEntry -> Manifest
manifestOf entries =
  Manifest.new
    (Creator.new instant "studio-01" "mediacopy3000" "0.1.0.0")
    Process.Transfer
    (DirHash.new instant (Hash.Hash Algo.XXH64 "8d02114c32e28cbe") (Hash.Hash Algo.XXH64 "f557f8ca8e5a88ef"))
    V.empty
    (V.fromList (map ManifestEntry.File entries))
