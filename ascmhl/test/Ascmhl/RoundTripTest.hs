module Ascmhl.RoundTripTest (tests) where

import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), minutesToTimeZone, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)
import Text.XML (Element (..), Name (..), Node (..))

import Ascmhl.Algo qualified as Algo
import Ascmhl.C4 qualified as C4
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.Creator qualified as Creator
import Ascmhl.DirHash (DirHash)
import Ascmhl.DirHash qualified as DirHash
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Generators qualified as Generators
import Ascmhl.Hash qualified as Hash
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.Process qualified as Process
import Ascmhl.RelPath qualified as RelPath
import Ascmhl.Time qualified as Time

tests :: TestTree
tests =
  testGroup
    "Ascmhl.RoundTrip"
    [ testProperty "an MHL time reads back as the same instant from any offset" timesRoundTrip
    , testProperty "a built manifest reads back equal" builtManifestsRoundTrip
    , testCase "reads the reference manifest" readsTheReferenceManifest
    , testCase "refuses a manifest whose path leaves the folder" refusesAnEscapingPath
    , testCase "chains read back equal and keep foreign elements and the c4 case" chainsRoundTrip
    ]

fixturePath :: FilePath
fixturePath = "test/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"

orFail :: (Show e) => Either e a -> IO a
orFail e = either (assertFailure . show) pure e

timesRoundTrip :: Property
timesRoundTrip = property $ do
  instant <- forAll Generators.genInstant
  offset <- forAll (Gen.int (Range.linear (-720) 840))
  let local = utcToZonedTime (minutesToTimeZone offset) instant & iso8601Show & T.pack
  Time.parse (Time.render instant) === Just instant
  Time.parse local === Just instant
  Time.parse (T.dropEnd 6 (Time.render instant) <> "Z") === Just instant

builtManifestsRoundTrip :: Property
builtManifestsRoundTrip = property $ do
  manifest <- forAll genManifest
  Manifest.parse (Manifest.render manifest) === Right manifest

genManifest :: Gen Manifest
genManifest = do
  creator <- Creator.new <$> Generators.genInstant <*> Generators.genWords <*> Generators.genWords <*> Generators.genWords
  process <- Gen.enumBounded
  root <- genDirHash
  ignorePatterns <- Gen.list (Range.linear 0 3) (Gen.filter (not . T.null) Generators.genWords)
  files <- Gen.list (Range.linear 0 6) (FileEntry.new <$> Generators.genRelPath <*> Gen.int64 (Range.linear 0 (2 ^ (40 :: Int))) <*> Generators.genInstant <*> Generators.genHash <*> Gen.element [FileEntry.Original, FileEntry.Verified, FileEntry.Failed] <*> Generators.genInstant)
  dirs <- Gen.list (Range.linear 0 3) (DirectoryEntry.new <$> Generators.genRelPath <*> Generators.genInstant <*> genDirHash)
  entries <- Gen.shuffle (map ManifestEntry.File files <> map ManifestEntry.Directory dirs)
  pure (Manifest.new creator process root (V.fromList ignorePatterns) (V.fromList entries))

genDirHash :: Gen DirHash
genDirHash = do
  algo <- Gen.enumBounded
  DirHash.new <$> Generators.genInstant <*> Generators.genHashOf algo <*> Generators.genHashOf algo

readsTheReferenceManifest :: Assertion
readsTheReferenceManifest = do
  m <- TIO.readFile fixturePath >>= orFail . Manifest.parse
  let notes = RelPath.RelPath "Sidecar/notes.txt"
      clip = RelPath.RelPath "A002C001.MXF"
      byPath = ManifestEntry.files m.entries & V.toList & map (\e -> (e.path, e)) & Map.fromList
      dirs = V.mapMaybe (\case ManifestEntry.Directory d -> Just d; ManifestEntry.File _ -> Nothing) m.entries
  m.process @?= Process.Transfer
  fmap (\e -> e.size) byPath @?= Map.fromList [(clip, 4096), (notes, 0)]
  fmap (\e -> e.hashes & V.toList & map (\h -> h.hash.algo) & Set.fromList) byPath
    @?= Map.fromList [(clip, Set.singleton Algo.XXH64), (notes, Set.fromList [Algo.MD5, Algo.XXH64])]
  assertBool "every hash is an original" (all (\e -> V.all (\h -> h.action == FileEntry.Original) e.hashes) byPath)
  assertBool
    "the sub-second part of a modification time is kept"
    (any (\e -> e.lastModified.utctDayTime /= fromInteger (truncate e.lastModified.utctDayTime)) (Map.lookup notes byPath))
  V.map (\d -> (d.path, V.map (\h -> h.content.algo) d.hashes)) dirs @?= V.singleton (RelPath.RelPath "Sidecar", V.singleton Algo.XXH64)
  V.map (\h -> h.content.algo) m.rootHash @?= V.singleton Algo.XXH64
  assertBool "the ascmhl folder is ignored" (V.elem "ascmhl" m.ignorePatterns)
  Manifest.parse (Manifest.render m) @?= Right m

refusesAnEscapingPath :: Assertion
refusesAnEscapingPath = do
  txt <- TIO.readFile fixturePath
  for_
    [ (">A002C001.MXF<", ">/etc/passwd<")
    , (">A002C001.MXF<", ">../A002C001.MXF<")
    , (">Sidecar<", ">../Sidecar<")
    ]
    ( \(needle, escape) ->
        assertBool (T.unpack escape <> " is accepted") (isLeft (Manifest.parse (T.replace needle escape txt)))
    )

chainsRoundTrip :: Assertion
chainsRoundTrip = do
  plain <- readChain "ascmhl_chain.xml"
  withC4 <- readChain "ascmhl_chain_c4.xml"
  withForeign <- readChain "ascmhl_chain_foreign.xml"
  for_ [plain, withC4, withForeign] (\c -> Chain.parse (Chain.render c) @?= Right c)
  assertBool "an unknown hash element is dropped" (V.all (\e -> not (V.null e.unknown)) plain.entries)
  assertBool "a foreign element is dropped" (V.any (\e -> V.any inForeignNamespace e.unknown) withForeign.entries)
  assertBool
    "a c4 hash no longer decodes"
    (V.all (\e -> any (\h -> isJust (C4.toBytes h.value)) e.c4) (withC4.entries <> withForeign.entries))
  where
    readChain name = TIO.readFile ("test/fixtures/ascmhl/" <> name) >>= orFail . Chain.parse
    inForeignNamespace = \case
      NodeElement e -> nameNamespace e.elementName == Just "urn:example:other:v1"
      _ -> False
