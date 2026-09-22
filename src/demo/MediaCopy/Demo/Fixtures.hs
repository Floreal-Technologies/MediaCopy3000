module MediaCopy.Demo.Fixtures
  ( at
  , mediaSource
  , shuttle
  , archive
  , mediaSourceFiles
  , mediaSourceDirs
  , mediaSourceTree
  , totalBytes
  , offloadJob
  , offloadFrom
  , verifyJob
  , sealJob
  , specFor
  , offloadFactsReady
  , offloadFactsBlocked
  , verifyFacts
  , sealFacts
  , readyPlan
  , blockedPlan
  , partialPlan
  , verifyPlan
  , sealPlan
  , history
  , paletteListing
  , hashOf
  , osp
  , rel
  , child
  ) where

import Ascmhl.Build (chainFromListing, creatorInfo, dirHash, directoryEntry, fileEntry, newManifest, orderedEntries)
import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Path (RelPath, mkRelPath)
import Ascmhl.Types (Chain (..), DirHash, HashAction (..), Manifest, ManifestEntry, MhlHistory, ProcessKind (..), historyOf)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

import MediaCopy.Domain.FileSystem (Tree (..), ignorePatterns, partSuffix)
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan)
import MediaCopy.Domain.Preflight (GenerationFacts (..), HistoryRule (..), OffloadFacts (..), TargetFacts (..), decideGeneration, decideOffload)
import MediaCopy.Interface.Theme (FamilyInfo (..), FamilyListing (..), PaletteMode (..), ThemeListing (..))

at :: UTCTime
at = UTCTime {utctDay = fromGregorian 2026 9 12, utctDayTime = secondsToDiffTime (14 * 3600 + 3 * 60)}

mediaSource :: OsPath
mediaSource = osp "/media/CARD_A001"

shuttle :: OsPath
shuttle = osp "/Volumes/Shuttle-01/2026-09-12"

archive :: OsPath
archive = osp "/Volumes/Archive-A/2026-09-12"

mediaSourceFiles :: Vector (RelPath, FileSize)
mediaSourceFiles =
  V.fromList
    [ (rel "A001C001_260912_R1AB.mov", 4_812_345_678)
    , (rel "A001C002_260912_R1AB.mov", 3_204_112_900)
    , (rel "A001C003_260912_R1AB.mov", 5_991_004_112)
    , (rel "A001C004_260912_R1AB.mov", 1_204_990_000)
    , (rel "Clips/A001C005_260912_R1AB.mov", 2_004_112_233)
    , (rel "Sidecar/A001C001.wav", 44_120_400)
    , (rel "A001M01.XML", 12_004)
    ]

mediaSourceDirs :: Vector RelPath
mediaSourceDirs = V.fromList [rel "Clips", rel "Sidecar"]

mediaSourceTree :: Tree
mediaSourceTree = Tree {files = V.fromList (sortOn fst (V.toList mediaSourceFiles)), dirs = mediaSourceDirs}

totalBytes :: FileSize
totalBytes = V.sum (V.map snd mediaSourceFiles)

offloadJob :: SealFirst -> Job
offloadJob = offloadFrom mediaSource

offloadFrom :: OsPath -> SealFirst -> Job
offloadFrom source sealFirst =
  Offload
    OffloadJob
      { source
      , destinations = shuttle :| [archive]
      , sealFirst
      , existingCopy = Nothing
      }

verifyJob :: OsPath -> Job
verifyJob folder = VerifyFolder VerifyJob {folder}

sealJob :: OsPath -> Job
sealJob folder = SealMediaSource SealJob {folder}

specFor :: JobId -> Job -> JobSpec
specFor jid job = JobSpec {jobId = jid, job, createdAt = at}

recordedHashes :: Map RelPath Hash
recordedHashes = Map.fromList (V.toList (V.map (\pair -> (fst pair, hashOf "4f9a1c3b2d7e8051")) mediaSourceFiles))

sourceChain :: Chain
sourceChain =
  chainFromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        ]
    )

verifyChain :: Chain
verifyChain =
  chainFromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        , osp "0003_CARD_A001_2026-09-12_140300.mhl"
        ]
    )

destinationFacts :: Int64 -> Maybe Int64
destinationFacts free = Just free

targetFacts :: OsPath -> Maybe Int64 -> Maybe Tree -> TargetFacts
targetFacts root freeBytes existing = TargetFacts {root, freeBytes, history = Right Nothing, existing}

offloadFactsReady :: OffloadFacts
offloadFactsReady =
  OffloadFacts
    { sourceTree = Just mediaSourceTree
    , targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just Tree {files = V.empty, dirs = V.empty})
          , targetFacts archive (destinationFacts 3_010_000_000_000) Nothing
          ]
    , originals = Right (Just recordedHashes)
    , sourceHistory = Right (Just (sourceChain, recordedHashes))
    }

offloadFactsBlocked :: OffloadFacts
offloadFactsBlocked =
  offloadFactsReady
    { targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just Tree {files = V.singleton (rel "DCIM/other.mov", 1_200_000), dirs = V.singleton (rel "DCIM")})
          , targetFacts archive (destinationFacts 12_000_000_000) (Just Tree {files = V.empty, dirs = V.empty})
          ]
    }

verifyFacts :: GenerationFacts
verifyFacts =
  GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 812_004_000_000
    , history = Right (Just (verifyChain, recordedHashes))
    }

sealFacts :: GenerationFacts
sealFacts =
  GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 24_000_000_000
    , history = Right Nothing
    }

offloadFactsPartial :: OffloadFacts
offloadFactsPartial =
  offloadFactsReady
    { targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just partialTree)
          , targetFacts archive (destinationFacts 3_010_000_000_000) Nothing
          ]
    }
  where
    held = V.take 4 mediaSourceFiles
    partName = V.map (\pair -> fst pair) mediaSourceFiles V.!? 4
    partialTree =
      Tree
        { files = held <> maybe V.empty (\p -> V.singleton (rel (display p <> partSuffix), 400_000_000)) partName
        , dirs = mediaSourceDirs
        }

readyPlan :: JobSpec -> JobPlan
readyPlan spec = decideOffload spec (offloadOf spec.job) offloadFactsReady

blockedPlan :: JobSpec -> JobPlan
blockedPlan spec = decideOffload spec (offloadOf spec.job) offloadFactsBlocked

partialPlan :: JobSpec -> JobPlan
partialPlan spec = decideOffload spec (offloadOf spec.job) offloadFactsPartial

verifyPlan :: JobSpec -> JobPlan
verifyPlan spec = decideGeneration spec RequireHistory verifyFacts

sealPlan :: JobSpec -> JobPlan
sealPlan spec = decideGeneration spec AllowFresh sealFacts

offloadOf :: Job -> OffloadJob
offloadOf = \case
  Offload oj -> oj
  other -> error ("demo fixture asked for an offload plan of a " <> show (jobKind other) <> " job")

history :: MhlHistory
history =
  historyOf
    ( V.fromList
        [ (1, manifestAt ProcessInPlace (-7200))
        , (2, manifestAt ProcessTransfer (-3600))
        , (3, manifestAt ProcessInPlace 0)
        ]
    )

manifestAt :: ProcessKind -> Int -> Manifest
manifestAt process offsetSeconds =
  newManifest
    (creatorInfo t "studio-01" "mediacopy3000" "0.1.0.0")
    process
    (treeHash t)
    ignorePatterns
    (entriesAt t)
  where
    t = addUTCTime (fromIntegral offsetSeconds) at

entriesAt :: UTCTime -> Vector ManifestEntry
entriesAt t =
  orderedEntries
    (V.map (\pair -> fileEntry (fst pair) (snd pair) t (hashOf "4f9a1c3b2d7e8051") Original t) mediaSourceTree.files)
    (V.map (\path -> directoryEntry path t (treeHash t)) mediaSourceTree.dirs)

treeHash :: UTCTime -> DirHash
treeHash t = dirHash t (hashOf "1b7f0c9d2a4e6358") (hashOf "9e3d5a7c1f8b0246")

paletteListing :: ThemeListing
paletteListing =
  ThemeListing
    { families =
        V.fromList
          [ FamilyListing
              { directory = "catppuccin"
              , info = FamilyInfo {name = Just "Catppuccin", homepage = Just "https://catppuccin.com"}
              , modes =
                  V.fromList
                    [ (DarkPalette, V.fromList ["frappé.css", "macchiato.css", "mocha.css"])
                    , (LightPalette, V.singleton "latte.css")
                    ]
              }
          , FamilyListing
              { directory = "dracula"
              , info = FamilyInfo {name = Just "Dracula", homepage = Just "https://draculatheme.com"}
              , modes =
                  V.fromList
                    [ (DarkPalette, V.singleton "dracula.css")
                    , (LightPalette, V.singleton "alucard.css")
                    ]
              }
          , FamilyListing
              { directory = "everforest"
              , info = FamilyInfo {name = Just "Everforest", homepage = Just "https://everforest.vercel.app"}
              , modes =
                  V.fromList
                    [ (DarkPalette, V.fromList ["hard.css", "medium.css", "soft.css"])
                    , (LightPalette, V.fromList ["hard.css", "medium.css", "soft.css"])
                    ]
              }
          , FamilyListing
              { directory = "kanagawa"
              , info = FamilyInfo {name = Just "Kanagawa", homepage = Just "https://github.com/rebelot/kanagawa.nvim"}
              , modes =
                  V.fromList
                    [ (DarkPalette, V.fromList ["dragon.css", "wave.css"])
                    , (LightPalette, V.singleton "lotus.css")
                    ]
              }
          ]
    }

hashOf :: Text -> Hash
hashOf value = Hash {algo = XXH64, value}

osp :: String -> OsPath
osp = unsafeEncodeUtf

rel :: Text -> RelPath
rel raw = case mkRelPath raw of
  Just path -> path
  Nothing -> error ("demo fixture holds a path that is not relative: " <> show raw)

child :: OsPath -> String -> OsPath
child parent name = parent </> osp name
