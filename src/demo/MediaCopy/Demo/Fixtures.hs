-- | The fixture world the screenshots and the model tests share.
--
-- One media source, two destinations, one instant, one history. Nothing here touches a disk: every
-- path is a literal and every fact is written by hand. Every plan comes from the planner: a fixture
-- states the facts a gathering would have read, and 'decideOffload' or 'decideGeneration' settles
-- the plan, so no fixture can show a plan the engine would refuse to run.
module MediaCopy.Demo.Fixtures
  ( -- * The instant, the paths and the media source
    at
  , mediaSource
  , shuttle
  , archive
  , mediaSourceFiles
  , mediaSourceDirs
  , mediaSourceTree
  , totalBytes

    -- * The jobs
  , offloadJob
  , offloadFrom
  , verifyJob
  , sealJob
  , specFor

    -- * What the planner reads
  , offloadFactsReady
  , offloadFactsBlocked
  , verifyFacts
  , sealFacts

    -- * What the planner settles
  , readyPlan
  , blockedPlan
  , partialPlan
  , verifyPlan
  , sealPlan

    -- * The history a finished job shows
  , history

    -- * The palettes the asset tree holds
  , paletteListing

    -- * Small helpers
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

-- * The instant, the paths and the media source

-- | This instant stamps every fixture, so two runs of the screenshot script make the same picture.
at :: UTCTime
at = UTCTime {utctDay = fromGregorian 2026 9 12, utctDayTime = secondsToDiffTime (14 * 3600 + 3 * 60)}

mediaSource :: OsPath
mediaSource = osp "/media/CARD_A001"

shuttle :: OsPath
shuttle = osp "/Volumes/Shuttle-01/2026-09-12"

archive :: OsPath
archive = osp "/Volumes/Archive-A/2026-09-12"

-- | The files of the fixture media source. Every job reads the same one, so one set serves them all.
-- A scene indexes into this vector, so its order is the order the file rows appear in.
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

-- | The folders the fixture media source holds.
mediaSourceDirs :: Vector RelPath
mediaSourceDirs = V.fromList [rel "Clips", rel "Sidecar"]

-- | The walk a gathering would have made. A 'Tree' holds its files in path order, which the scene's
-- own order is not, so the sort happens here and nowhere else.
mediaSourceTree :: Tree
mediaSourceTree = Tree {files = V.fromList (sortOn fst (V.toList mediaSourceFiles)), dirs = mediaSourceDirs}

totalBytes :: FileSize
totalBytes = V.sum (V.map snd mediaSourceFiles)

-- * The jobs

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

-- * What the planner reads

-- | The hashes the media source's own history holds: one for each file, all of one format. One
-- format settles the job's, and the copies then have something to check against.
recordedHashes :: Map RelPath Hash
recordedHashes = Map.fromList (V.toList (V.map (\pair -> (fst pair, hashOf "4f9a1c3b2d7e8051")) mediaSourceFiles))

-- | The media source already holds two generations, so the sheet can say what a third would add.
sourceChain :: Chain
sourceChain =
  chainFromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        ]
    )

-- | The folder a verify job reads holds the three generations that 'history' shows.
verifyChain :: Chain
verifyChain =
  chainFromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        , osp "0003_CARD_A001_2026-09-12_140300.mhl"
        ]
    )

-- | One destination as the plan reads it: the free bytes of the parent's device.
destinationFacts :: Int64 -> Maybe Int64
destinationFacts free = Just free

-- | One destination of the fixture offload. It has no history, so each records generation 1. The
-- root is the folder the operator picked, which a gathering appends the media source's name to
-- ('destinationPath'). The manual's pictures and sample report show the picked folder, so the
-- fixture keeps it.
targetFacts :: OsPath -> Maybe Int64 -> Maybe Tree -> TargetFacts
targetFacts root freeBytes existing = TargetFacts {root, freeBytes, history = Right Nothing, existing}

-- | A shuttle that is empty and an archive that the job will create. Nothing blocks this plan.
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

-- | The same job against a shuttle that holds a folder and a file the card does not, and an archive that is too small. Two blockers: 'DestinationForeign' and 'InsufficientSpace'.
offloadFactsBlocked :: OffloadFacts
offloadFactsBlocked =
  offloadFactsReady
    { targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just Tree {files = V.singleton (rel "DCIM/other.mov", 1_200_000), dirs = V.singleton (rel "DCIM")})
          , targetFacts archive (destinationFacts 12_000_000_000) (Just Tree {files = V.empty, dirs = V.empty})
          ]
    }

-- | A folder that holds three generations, so a verify has a history to check against.
verifyFacts :: GenerationFacts
verifyFacts =
  GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 812_004_000_000
    , history = Right (Just (verifyChain, recordedHashes))
    }

-- | A media source straight out of a camera: files, and no history at all.
sealFacts :: GenerationFacts
sealFacts =
  GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 24_000_000_000
    , history = Right Nothing
    }

-- | The shuttle holds the first four files of the card and a part file of the fifth. Nothing else.
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

-- * What the planner settles

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

-- | The offload a plan fixture plans for. The plan follows its own spec, so the queue's other two
-- media sources get plans that name themselves. A spec of another kind is a fixture mistake, and
-- stops the process before a picture comes out.
offloadOf :: Job -> OffloadJob
offloadOf = \case
  Offload oj -> oj
  other -> error ("demo fixture asked for an offload plan of a " <> show (jobKind other) <> " job")

-- * The history a finished job shows

-- | Three generations: a seal, the transfer that copied it, and a verify. The reader derives each
-- row from the manifest, so the picture shows what a real read of a real history would show.
history :: MhlHistory
history =
  historyOf
    ( V.fromList
        [ (1, manifestAt ProcessInPlace (-7200))
        , (2, manifestAt ProcessTransfer (-3600))
        , (3, manifestAt ProcessInPlace 0)
        ]
    )

-- | One generation's manifest, offset from 'at' by whole seconds.
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

-- | Every file under the one recorded hash, and every folder under one directory hash. The reader
-- takes the generation's formats and its failure count from these rows.
entriesAt :: UTCTime -> Vector ManifestEntry
entriesAt t =
  orderedEntries
    (V.map (\pair -> fileEntry (fst pair) (snd pair) t (hashOf "4f9a1c3b2d7e8051") Original t) mediaSourceTree.files)
    (V.map (\path -> directoryEntry path t (treeHash t)) mediaSourceTree.dirs)

treeHash :: UTCTime -> DirHash
treeHash t = dirHash t (hashOf "1b7f0c9d2a4e6358") (hashOf "9e3d5a7c1f8b0246")

-- * The palettes the asset tree holds

-- | Every palette under @assets\/themes@, as the tree presents them. The application reads the tree
-- at run time and a fixture cannot, so the listing is repeated here. It must keep the tree's shape,
-- @\<family\>\/\<light|dark\>\/\<name\>.css@, with the family's own file beside them.
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

-- * Small helpers

hashOf :: Text -> Hash
hashOf value = Hash {algo = XXH64, value}

-- | The paths are fixtures, so an unencodable path or an impossible relative path is a mistake
-- here and nowhere else. Both stop the process at once and never get to a screenshot.
osp :: String -> OsPath
osp = unsafeEncodeUtf

rel :: Text -> RelPath
rel raw = case mkRelPath raw of
  Just path -> path
  Nothing -> error ("demo fixture holds a path that is not relative: " <> show raw)

child :: OsPath -> String -> OsPath
child parent name = parent </> osp name
