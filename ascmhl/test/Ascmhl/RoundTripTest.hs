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

import Ascmhl.Build
import Ascmhl.Generators
import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read
import Ascmhl.Types
import Ascmhl.Write

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
  instant <- forAll genInstant
  offset <- forAll (Gen.int (Range.linear (-720) 840))
  let local = utcToZonedTime (minutesToTimeZone offset) instant & iso8601Show & T.pack
  parseMhlTime (formatMhlTime instant) === Just instant
  parseMhlTime local === Just instant
  parseMhlTime (T.dropEnd 6 (formatMhlTime instant) <> "Z") === Just instant

builtManifestsRoundTrip :: Property
builtManifestsRoundTrip = property $ do
  manifest <- forAll genManifest
  parseManifest (renderManifest manifest) === Right manifest

genManifest :: Gen Manifest
genManifest = do
  creator <- creatorInfo <$> genInstant <*> genWords <*> genWords <*> genWords
  process <- Gen.enumBounded
  root <- genDirHash
  ignorePatterns <- Gen.list (Range.linear 0 3) (Gen.filter (not . T.null) genWords)
  files <- Gen.list (Range.linear 0 6) (fileEntry <$> genRelPath <*> Gen.int64 (Range.linear 0 (2 ^ (40 :: Int))) <*> genInstant <*> genHash <*> Gen.element [Original, Verified, FailedAction] <*> genInstant)
  dirs <- Gen.list (Range.linear 0 3) (directoryEntry <$> genRelPath <*> genInstant <*> genDirHash)
  entries <- Gen.shuffle (map ManifestFile files <> map ManifestDir dirs)
  pure (newManifest creator process root (V.fromList ignorePatterns) (V.fromList entries))

genDirHash :: Gen DirHash
genDirHash = do
  algo <- Gen.enumBounded
  dirHash <$> genInstant <*> genHashOf algo <*> genHashOf algo

readsTheReferenceManifest :: Assertion
readsTheReferenceManifest = do
  m <- TIO.readFile fixturePath >>= orFail . parseManifest
  let notes = RelPath "Sidecar/notes.txt"
      clip = RelPath "A002C001.MXF"
      byPath = fileEntries m.entries & V.toList & map (\e -> (e.path, e)) & Map.fromList
      dirs = V.mapMaybe (\case ManifestDir d -> Just d; ManifestFile _ -> Nothing) m.entries
  m.process @?= ProcessTransfer
  fmap (\e -> e.size) byPath @?= Map.fromList [(clip, 4096), (notes, 0)]
  fmap (\e -> e.hashes & V.toList & map (\h -> h.hash.algo) & Set.fromList) byPath
    @?= Map.fromList [(clip, Set.singleton XXH64), (notes, Set.fromList [MD5, XXH64])]
  assertBool "every hash is an original" (all (\e -> V.all (\h -> h.action == Original) e.hashes) byPath)
  assertBool
    "the sub-second part of a modification time is kept"
    (any (\e -> e.lastModified.utctDayTime /= fromInteger (truncate e.lastModified.utctDayTime)) (Map.lookup notes byPath))
  V.map (\d -> (d.path, V.map (\h -> h.content.algo) d.hashes)) dirs @?= V.singleton (RelPath "Sidecar", V.singleton XXH64)
  V.map (\h -> h.content.algo) m.rootHash @?= V.singleton XXH64
  assertBool "the ascmhl folder is ignored" (V.elem "ascmhl" m.ignorePatterns)
  parseManifest (renderManifest m) @?= Right m

refusesAnEscapingPath :: Assertion
refusesAnEscapingPath = do
  txt <- TIO.readFile fixturePath
  for_
    [ (">A002C001.MXF<", ">/etc/passwd<")
    , (">A002C001.MXF<", ">../A002C001.MXF<")
    , (">Sidecar<", ">../Sidecar<")
    ]
    ( \(needle, escape) ->
        assertBool (T.unpack escape <> " is accepted") (isLeft (parseManifest (T.replace needle escape txt)))
    )

chainsRoundTrip :: Assertion
chainsRoundTrip = do
  plain <- readChain "ascmhl_chain.xml"
  withC4 <- readChain "ascmhl_chain_c4.xml"
  withForeign <- readChain "ascmhl_chain_foreign.xml"
  for_ [plain, withC4, withForeign] (\c -> parseChain (renderChain c) @?= Right c)
  assertBool "an unknown hash element is dropped" (V.all (\e -> not (V.null e.unknown)) plain.entries)
  assertBool "a foreign element is dropped" (V.any (\e -> V.any inForeignNamespace e.unknown) withForeign.entries)
  assertBool
    "a c4 hash no longer decodes"
    (V.all (\e -> any (\h -> isJust (c4ToBytes h.value)) e.c4) (withC4.entries <> withForeign.entries))
  where
    readChain name = TIO.readFile ("test/fixtures/ascmhl/" <> name) >>= orFail . parseChain
    inForeignNamespace = \case
      NodeElement e -> nameNamespace e.elementName == Just "urn:example:other:v1"
      _ -> False
