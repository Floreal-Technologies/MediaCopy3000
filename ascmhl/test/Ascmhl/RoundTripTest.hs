{-# LANGUAGE NumericUnderscores #-}

module Ascmhl.RoundTripTest (tests) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit
import Text.XML (Element (..), Name (..), Node (..))

import Ascmhl.Build
import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read
import Ascmhl.Types
import Ascmhl.Write

-- | Unwraps a parse result. A Left fails the test.
orFail :: (Show e) => Either e a -> IO a
orFail e = either (\err -> assertFailure (show err)) pure e

fixturePath :: FilePath
fixturePath = "test/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"

tests :: TestTree
tests =
  testGroup
    "Domain.Mhl"
    [ testGroup
        "time format"
        [ testCase "formats and parses" formatsAndParses
        , testCase "parses a Z suffix and a non-zero offset" parsesAZSuffixAndANonZeroOffset
        ]
    , testGroup
        "manifestFileName"
        [ testCase "follows NNNN_folder_date_time.mhl" followsNNNNFolderDateTimeMhl
        ]
    , testGroup
        "manifest"
        [ testCase "parses the fixture" parsesTheFixture
        , testCase "round-trips through render" roundTripsThroughRender
        , testCase "rejects garbage" rejectsGarbage
        , testCase "rejects a path that escapes the folder" rejectsEscapingPath
        , testCase "parses a zero-byte file with no size attribute" parsesAZeroByteFileWithNoSizeAttribute
        , testCase "parses the root hash and ignore patterns" parsesTheRootHashAndIgnorePatterns
        , testCase "parses a directory hash entry" parsesADirectoryHashEntry
        ]
    , testGroup
        "chain"
        [ testCase "parses and round-trips the fixture" chainParsesAndRoundTripsTheFixture
        , testCase "keeps the case of a c4 chain hash" keepsTheCaseOfAC4ChainHash
        , testCase "gives a foreign namespace back unchanged" givesAForeignNamespaceBackUnchanged
        ]
    , testGroup
        "latestHashes"
        [ testCase "prefers xxh64 and later generations" prefersXxh64AndLaterGenerations
        ]
    , testGroup
        "historyOf"
        [ testCase "builds one generation per manifest" buildsOneGenerationPerManifest
        ]
    , testGroup
        "build"
        [ testCase "orders a three-level tree by §6.5" ordersAThreeLevelTreeBy65
        , testCase "round-trips a built manifest" roundTripsABuiltManifest
        , testCase "appendGeneration keeps ordering and sets c4" appendGenerationKeepsOrderingAndSetsC4
        ]
    ]

formatsAndParses :: Assertion
formatsAndParses = do
  let t = UTCTime (fromGregorian 2026 5 9) (secondsToDiffTime (17 * 3600 + 120))
  formatMhlTime t @?= "2026-05-09T17:02:00+00:00"
  parseMhlTime "2026-05-09T17:02:00+00:00" @?= Just t

parsesAZSuffixAndANonZeroOffset :: Assertion
parsesAZSuffixAndANonZeroOffset = do
  parseMhlTime "2026-05-09T17:02:00Z" @?= parseMhlTime "2026-05-09T17:02:00+00:00"
  parseMhlTime "2026-05-09T19:02:00+02:00" @?= parseMhlTime "2026-05-09T17:02:00+00:00"

followsNNNNFolderDateTimeMhl :: Assertion
followsNNNNFolderDateTimeMhl = do
  let t = UTCTime (fromGregorian 2026 5 9) (secondsToDiffTime (17 * 3600 + 120))
  manifestFileName 1 "A002R2EC" t @?= "0001_A002R2EC_2026-05-09_170200.mhl"

-- | The fixture's hash date. Every hash in the fixture carries it.
fixtureHashDate :: UTCTime
fixtureHashDate = UTCTime (fromGregorian 2026 5 9) (secondsToDiffTime (17 * 3600 + 120))

parsesTheFixture :: Assertion
parsesTheFixture = do
  txt <- TIO.readFile fixturePath
  case parseManifest txt of
    Left err -> assertFailure (show err)
    Right m -> do
      m.process @?= ProcessTransfer
      m.creator.hostname @?= "dit-laptop"
      let files = fileEntries m.entries
      map (\e -> e.path) (V.toList files) @?= [RelPath "Sidecar/notes.txt", RelPath "A002C001.MXF"]
      map (\e -> e.size) (V.toList files) @?= [0, 4096]
      (files V.! 0).hashes
        @?= V.fromList
          [ ManifestHash {hash = Hash MD5 "d41d8cd98f00b204e9800998ecf8427e", action = Original, hashDate = Just fixtureHashDate, extraAttrs = Map.empty}
          , ManifestHash {hash = Hash XXH64 "ef46db3751d8e999", action = Original, hashDate = Just fixtureHashDate, extraAttrs = Map.empty}
          ]
      (files V.! 0).lastModified @?= UTCTime (fromGregorian 2026 5 9) (picosecondsToDiffTime ((16 * 3600 + 58 * 60 + 12) * 1_000_000_000_000 + 123_456_000_000))

roundTripsThroughRender :: Assertion
roundTripsThroughRender = do
  txt <- TIO.readFile fixturePath
  m <- orFail (parseManifest txt)
  parseManifest (renderManifest m) @?= Right m

rejectsGarbage :: Assertion
rejectsGarbage =
  assertBool "parseManifest \"<nope/>\" should be Left" (either (const True) (const False) (parseManifest "<nope/>"))

parsesAZeroByteFileWithNoSizeAttribute :: Assertion
parsesAZeroByteFileWithNoSizeAttribute = do
  txt <- TIO.readFile fixturePath
  m <- orFail (parseManifest txt)
  let files = fileEntries m.entries
  map (\e -> e.size) (V.toList files) @?= [0, 4096]

parsesTheRootHashAndIgnorePatterns :: Assertion
parsesTheRootHashAndIgnorePatterns = do
  txt <- TIO.readFile fixturePath
  m <- orFail (parseManifest txt)
  V.map (\d -> d.content) m.rootHash @?= V.singleton (Hash XXH64 "8d02114c32e28cbe")
  V.map (\d -> d.structure) m.rootHash @?= V.singleton (Hash XXH64 "f557f8ca8e5a88ef")
  m.ignorePatterns @?= V.fromList [".DS_Store", "ascmhl"]

parsesADirectoryHashEntry :: Assertion
parsesADirectoryHashEntry = do
  txt <- TIO.readFile fixturePath
  m <- orFail (parseManifest txt)
  let dirs =
        V.mapMaybe
          ( \case
              ManifestDir d -> Just d
              ManifestFile _ -> Nothing
          )
          m.entries
  V.map (\d -> d.path) dirs @?= V.singleton (RelPath "Sidecar")
  V.map (\d -> d.content) (V.concatMap (\d -> d.hashes) dirs) @?= V.singleton (Hash XXH64 "f1d7771e64cb3720")

chainParsesAndRoundTripsTheFixture :: Assertion
chainParsesAndRoundTripsTheFixture = do
  txt <- TIO.readFile "test/fixtures/ascmhl/ascmhl_chain.xml"
  c <- orFail (parseChain txt)
  c.entries
    @?= V.singleton
      ChainEntry
        { sequenceNr = 1
        , path = RelPath "0001_A002R2EC_2026-05-09_170200.mhl"
        , -- The fixture is a legacy chain. Its hash is xxh64, so it has no c4 and the parser keeps the entry whole.
          c4 = Nothing
        , -- The entry is kept as the very node it was read as, namespace included.
          unknown =
            V.singleton
              ( NodeElement
                  ( Element
                      (Name "xxh64" (Just "urn:ASC:MHL:DIRECTORY:v2.0") Nothing)
                      Map.empty
                      [NodeContent "0123456789abcdef"]
                  )
              )
        }
  parseChain (renderChain c) @?= Right c

-- | An element this project does not model comes back as it was read, its own namespace
-- included. A rewrite that re-namespaces it edits another tool's record of its own work.
givesAForeignNamespaceBackUnchanged :: Assertion
givesAForeignNamespaceBackUnchanged = do
  txt <- TIO.readFile "test/fixtures/ascmhl/ascmhl_chain_foreign.xml"
  c <- orFail (parseChain txt)
  let rendered = renderChain c
  assertBool
    ("the foreign namespace is gone from the rewrite:\n" <> T.unpack rendered)
    ("urn:example:other:v1" `T.isInfixOf` rendered)
  parseChain rendered @?= Right c

keepsTheCaseOfAC4ChainHash :: Assertion
keepsTheCaseOfAC4ChainHash = do
  txt <- TIO.readFile "test/fixtures/ascmhl/ascmhl_chain_c4.xml"
  c <- orFail (parseChain txt)
  V.mapMaybe (\e -> fmap (\h -> h.value) e.c4) c.entries
    @?= V.singleton "c447Fm3BJZQ62765jMZJH4m28hrDM7Szbj9CUmj4F4gnvyDYXYz4WfnK2nYRhFvRgYEectEXYBYWLDpLo6XGNAfKdt"
  parseChain (renderChain c) @?= Right c

prefersXxh64AndLaterGenerations :: Assertion
prefersXxh64AndLaterGenerations = do
  txt <- TIO.readFile fixturePath
  m1 <- orFail (parseManifest txt)
  let m2 =
        Manifest
          { creator = m1.creator
          , process = m1.process
          , rootHash = V.empty
          , ignorePatterns = V.empty
          , unknown = V.empty
          , entries =
              V.map
                ( \e ->
                    ManifestFile
                      HashEntry
                        { path = e.path
                        , size = e.size
                        , lastModified = e.lastModified
                        , hashes = V.singleton ManifestHash {hash = Hash XXH64 "1111111111111111", action = Verified, hashDate = Nothing, extraAttrs = Map.empty}
                        , pathAttrs = Map.empty
                        , unknown = V.empty
                        }
                )
                (V.filter (\e -> e.path == RelPath "A002C001.MXF") (fileEntries m1.entries))
          }
      hs = latestHashes (V.fromList [m1, m2])
  Map.lookup (RelPath "A002C001.MXF") hs @?= Just (Hash XXH64 "1111111111111111")
  Map.lookup (RelPath "Sidecar/notes.txt") hs @?= Just (Hash XXH64 "ef46db3751d8e999")

buildsOneGenerationPerManifest :: Assertion
buildsOneGenerationPerManifest = do
  txt <- TIO.readFile fixturePath
  m <- orFail (parseManifest txt)
  let MhlHistory gens = historyOf (V.singleton (1, m))
      gensList = V.toList gens
  map (\g -> g.number) gensList @?= [1]
  -- A set of formats has no document order. It reads back in 'Ord HashAlgo', preferred format first.
  map (\g -> Set.toList g.algos) gensList @?= [[XXH64, MD5]]
  map (\g -> g.failures) gensList @?= [0]

rejectsEscapingPath :: Assertion
rejectsEscapingPath = do
  txt <- TIO.readFile fixturePath
  let absolute = T.replace ">A002C001.MXF<" ">/etc/passwd<" txt
      dotdot = T.replace ">A002C001.MXF<" ">../A002C001.MXF<" txt
  assertBool "absolute path accepted" (isLeft (parseManifest absolute))
  assertBool ".. path accepted" (isLeft (parseManifest dotdot))

-- | The instant every built fixture carries.
buildInstant :: UTCTime
buildInstant = UTCTime (fromGregorian 2026 9 14) (secondsToDiffTime (9 * 3600))

-- | A row as one line, so a failure reads as an order and not as two records.
entryLabels :: Vector ManifestEntry -> [Text]
entryLabels entries =
  map
    ( \case
        ManifestFile e -> "file " <> display e.path
        ManifestDir d -> "dir " <> display d.path
    )
    (V.toList entries)

builtFile :: Text -> HashEntry
builtFile path =
  fileEntry (RelPath path) 4096 buildInstant (Hash XXH64 "ef46db3751d8e999") Original buildInstant

builtDir :: Text -> DirectoryEntry
builtDir path =
  directoryEntry
    (RelPath path)
    buildInstant
    (dirHash buildInstant (Hash XXH64 "8d02114c32e28cbe") (Hash XXH64 "f557f8ca8e5a88ef"))

ordersAThreeLevelTreeBy65 :: Assertion
ordersAThreeLevelTreeBy65 = do
  let files = V.fromList (map builtFile ["top.txt", "A/a1.mxf", "A/B/b1.mxf", "C/c1.mxf"])
      -- Out of order on purpose: the order comes from the paths, not from the vector.
      dirs = V.fromList (map builtDir ["C", "A/B", "A"])
  entryLabels (orderedEntries files dirs)
    @?= [ "file A/B/b1.mxf"
        , "dir A/B"
        , "file A/a1.mxf"
        , "dir A"
        , "file C/c1.mxf"
        , "dir C"
        , "file top.txt"
        ]

roundTripsABuiltManifest :: Assertion
roundTripsABuiltManifest = do
  let files = V.fromList (map builtFile ["A/a1.mxf", "top.txt"])
      dirs = V.singleton (builtDir "A")
      m =
        newManifest
          (creatorInfo buildInstant "studio-01" "mediacopy3000" "0.1.0.0")
          ProcessTransfer
          (dirHash buildInstant (Hash XXH64 "8d02114c32e28cbe") (Hash XXH64 "f557f8ca8e5a88ef"))
          (V.fromList [".DS_Store", "ascmhl"])
          (orderedEntries files dirs)
  parseManifest (renderManifest m) @?= Right m

appendGenerationKeepsOrderingAndSetsC4 :: Assertion
appendGenerationKeepsOrderingAndSetsC4 = do
  let earlier = Hash C4 "c447Fm3BJZQ62765jMZJH4m28hrDM7Szbj9CUmj4F4gnvyDYXYz4WfnK2nYRhFvRgYEectEXYBYWLDpLo6XGNAfKdt"
      -- The given chain sits in the wrong order, and generation 1 is a legacy entry with no c4.
      given =
        Chain
          ( V.fromList
              [ ChainEntry {sequenceNr = 2, path = RelPath "0002_card_2026-09-13_091500.mhl", c4 = Just earlier, unknown = V.empty}
              , ChainEntry {sequenceNr = 1, path = RelPath "0001_card_2026-09-12_140300.mhl", c4 = Nothing, unknown = V.empty}
              ]
          )
      minted = Hash C4 "c43GfLJbkPgkFvEMXfEzMg1V5GkYJQFQKSZQHsAzvuJ4WJ4FRQqYfvA9JTBpFTVFpJcZHvJYfFvJJFQFTvJpFvJqFf"
      appended = appendGeneration given 3 (RelPath "0003_card_2026-09-14_090000.mhl") minted
  V.map (\e -> e.sequenceNr) appended.entries @?= V.fromList [1, 2, 3]
  V.map (\e -> e.c4) appended.entries @?= V.fromList [Nothing, Just earlier, Just minted]
  V.map (\e -> e.path) appended.entries
    @?= V.fromList
      [ RelPath "0001_card_2026-09-12_140300.mhl"
      , RelPath "0002_card_2026-09-13_091500.mhl"
      , RelPath "0003_card_2026-09-14_090000.mhl"
      ]
