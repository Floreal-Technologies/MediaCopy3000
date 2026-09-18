module MediaCopy.Demo.Fixtures
  ( at
  , mediaSource
  , shuttle
  , archive
  , mediaSourceFiles
  , totalBytes
  , offloadJob
  , offloadFrom
  , verifyJob
  , sealJob
  , specFor
  , readyPlan
  , blockedPlan
  , partialPlan
  , verifyPlan
  , sealPlan
  , pluginCatalog
  , pluginSpec
  , pluginPlan
  , pluginReports
  , history
  , hashOf
  , osp
  , rel
  , child
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Author qualified as Author
import Ascmhl.Chain (Chain)
import Ascmhl.Chain qualified as Chain
import Ascmhl.Creator qualified as Creator
import Ascmhl.DirHash (DirHash)
import Ascmhl.DirHash qualified as DirHash
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.History (History)
import Ascmhl.History qualified as History
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry (ManifestEntry)
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.Process (Process)
import Ascmhl.Process qualified as Process
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Field qualified as Field
import System.OsPath (unsafeEncodeUtf, (</>))

import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.Destination (Destination)
import MediaCopy.Domain.Destination qualified as Destination
import MediaCopy.Domain.FieldView qualified as FieldView
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload (Offload)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Part qualified as Part
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PluginCatalog (PluginCatalog)
import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginReport (PluginReport)
import MediaCopy.Domain.PluginReport qualified as PluginReport
import MediaCopy.Domain.Preflight qualified as Preflight
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Tree (Tree)
import MediaCopy.Domain.Tree qualified as Tree

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
mediaSourceTree = Tree.Tree {files = V.fromList (sortOn fst (V.toList mediaSourceFiles)), dirs = mediaSourceDirs}

totalBytes :: FileSize
totalBytes = V.sum (V.map snd mediaSourceFiles)

offloadJob :: Offload.SealFirst -> Job
offloadJob = offloadFrom mediaSource

offloadFrom :: OsPath -> Offload.SealFirst -> Job
offloadFrom source sealFirst =
  Job.Offload
    Offload.Offload
      { source
      , destinations = shuttle :| [archive]
      , sealFirst
      , existingCopy = Nothing
      }

verifyJob :: OsPath -> Job
verifyJob folder = Job.VerifyFolder Job.Verify {folder}

sealJob :: OsPath -> Job
sealJob folder = Job.SealMediaSource Job.Seal {folder}

specFor :: JobId -> Job -> JobSpec
specFor jid job = JobSpec.JobSpec {id = jid, job, createdAt = at, pluginFields = Map.empty}

recordedHashes :: Map RelPath Hash
recordedHashes = Map.fromList (V.toList (V.map (\pair -> (fst pair, hashOf "4f9a1c3b2d7e8051")) mediaSourceFiles))

sourceChain :: Chain
sourceChain =
  Chain.fromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        ]
    )

verifyChain :: Chain
verifyChain =
  Chain.fromListing
    ( V.fromList
        [ osp "0001_CARD_A001_2026-09-12_120000.mhl"
        , osp "0002_CARD_A001_2026-09-12_130000.mhl"
        , osp "0003_CARD_A001_2026-09-12_140300.mhl"
        ]
    )

destinationFacts :: Int64 -> Maybe Int64
destinationFacts free = Just free

targetFacts :: OsPath -> Maybe Int64 -> Maybe Tree -> Destination
targetFacts root freeBytes existing = Destination.Destination {root, freeBytes, history = Right Nothing, existing}

offloadFactsReady :: Preflight.OffloadFacts
offloadFactsReady =
  Preflight.OffloadFacts
    { sourceTree = Just mediaSourceTree
    , targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just Tree.Tree {files = V.empty, dirs = V.empty})
          , targetFacts archive (destinationFacts 3_010_000_000_000) Nothing
          ]
    , originals = Right (Just recordedHashes)
    , sourceHistory = Right (Just (sourceChain, recordedHashes))
    }

offloadFactsBlocked :: Preflight.OffloadFacts
offloadFactsBlocked =
  offloadFactsReady
    { Preflight.targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just Tree.Tree {files = V.singleton (rel "DCIM/other.mov", 1_200_000), dirs = V.singleton (rel "DCIM")})
          , targetFacts archive (destinationFacts 12_000_000_000) (Just Tree.Tree {files = V.empty, dirs = V.empty})
          ]
    }

verifyFacts :: Preflight.GenerationFacts
verifyFacts =
  Preflight.GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 812_004_000_000
    , history = Right (Just (verifyChain, recordedHashes))
    }

sealFacts :: Preflight.GenerationFacts
sealFacts =
  Preflight.GenerationFacts
    { tree = Just mediaSourceTree
    , freeBytes = Just 24_000_000_000
    , history = Right Nothing
    }

offloadFactsPartial :: Preflight.OffloadFacts
offloadFactsPartial =
  offloadFactsReady
    { Preflight.targets =
        V.fromList
          [ targetFacts shuttle (destinationFacts 812_004_000_000) (Just partialTree)
          , targetFacts archive (destinationFacts 3_010_000_000_000) Nothing
          ]
    }
  where
    held = V.take 4 mediaSourceFiles
    partName = V.map fst mediaSourceFiles V.!? 4
    partialTree =
      Tree.Tree
        { files = held <> maybe V.empty (\p -> V.singleton (rel (display p <> Part.suffix), 400_000_000)) partName
        , dirs = mediaSourceDirs
        }

readyPlan :: JobSpec -> Plan
readyPlan spec = Preflight.offload spec (offloadOf spec.job) offloadFactsReady

blockedPlan :: JobSpec -> Plan
blockedPlan spec = Preflight.offload spec (offloadOf spec.job) offloadFactsBlocked

partialPlan :: JobSpec -> Plan
partialPlan spec = Preflight.offload spec (offloadOf spec.job) offloadFactsPartial

verifyPlan :: JobSpec -> Plan
verifyPlan spec = Preflight.generation spec Preflight.RequireHistory verifyFacts

sealPlan :: JobSpec -> Plan
sealPlan spec = Preflight.generation spec Preflight.AllowFresh sealFacts

credits :: PluginRef
credits = PluginRef.PluginRef {id = "tech.floreal.credits", name = "Credits"}

demoSlots :: Vector Plugin.AuthorSlot
demoSlots = V.fromList [AuthorSlot.AuthorSlot "DIT" "Jane Doe" "jane@example.com" "", AuthorSlot.AuthorSlot "Camera operator" "" "" ""]

pluginCatalog :: PluginCatalog
pluginCatalog =
  PluginCatalog.PluginCatalog
    { entries =
        V.fromList
          [ PluginCatalog.Entry
              { plugin = credits
              , version = "1.0.0"
              , description = "Puts the names of the crew into each manifest that a job writes."
              , folder = "/home/you/.local/share/mediacopy3000/plugins/tech.floreal.credits"
              , enabled = True
              , trace = False
              , capabilities = V.singleton (PluginCatalog.CapabilityView Capability.ManifestWrite PluginCatalog.Granted)
              , settings = V.singleton (FieldView.FieldView "authors" "Authors" Field.AuthorsField True (FieldView.AuthorList demoSlots))
              , jobFields = FieldView.authorFields "authors" demoSlots
              , active = True
              , problem = Nothing
              }
          ]
    , rejected = V.singleton ("/home/you/.local/share/mediacopy3000/plugins/old-tool", "plugin.json needs plug-in API 2, and this MediaCopy 3000 speaks API 1")
    , problem = Nothing
    }

pluginSpec :: JobId -> Job -> JobSpec
pluginSpec jid job = JobSpec.JobSpec {id = jid, job, createdAt = at, pluginFields = Map.singleton credits.id (Map.singleton "authors.1.name" "Sam Roe")}

pluginPlan :: JobSpec -> Plan
pluginPlan spec =
  Plan.withPlugins
    PluginPlan.PluginPlan
      { active = V.singleton credits
      , findings = V.empty
      , contributions =
          Contributions.Contributions
            { authors = V.fromList [Author.Author "Jane Doe" (Just "jane@example.com") Nothing (Just "DIT"), Author.Author "Sam Roe" Nothing Nothing (Just "Camera operator")]
            , fileMetadata = Map.empty
            , manifestMetadata = Nothing
            , contributors = V.singleton credits
            }
      }
    (readyPlan spec)

pluginReports :: List PluginReport
pluginReports =
  [PluginReport.Warned PluginFinding.PluginFinding {plugin = credits, severity = Severity.Warning, about = PluginFinding.Faulted (PluginFinding.Unavailable "contribute timed out after 30 s")}]

offloadOf :: Job -> Offload
offloadOf = \case
  Job.Offload oj -> oj
  other -> error ("demo fixture asked for an offload plan of a " <> show (Job.kind other) <> " job")

history :: History
history =
  History.fromManifests
    ( V.fromList
        [ (1, manifestAt Process.InPlace (-7200))
        , (2, manifestAt Process.Transfer (-3600))
        , (3, manifestAt Process.InPlace 0)
        ]
    )

manifestAt :: Process -> Int -> Manifest
manifestAt process offsetSeconds =
  Manifest.new
    (Creator.new t "studio-01" "mediacopy3000" "0.1.0.0")
    process
    (treeHash t)
    Tree.ignorePatterns
    (entriesAt t)
  where
    t = addUTCTime (fromIntegral offsetSeconds) at

entriesAt :: UTCTime -> Vector ManifestEntry
entriesAt t =
  ManifestEntry.ordered
    (V.map (\pair -> FileEntry.new (fst pair) (snd pair) t (hashOf "4f9a1c3b2d7e8051") FileEntry.Original t) mediaSourceTree.files)
    (V.map (\path -> DirectoryEntry.new path t (treeHash t)) mediaSourceTree.dirs)

treeHash :: UTCTime -> DirHash
treeHash t = DirHash.new t (hashOf "1b7f0c9d2a4e6358") (hashOf "9e3d5a7c1f8b0246")

hashOf :: Text -> Hash
hashOf value = Hash.Hash {algo = Algo.XXH64, value}

osp :: String -> OsPath
osp = unsafeEncodeUtf

rel :: Text -> RelPath
rel raw = case RelPath.parse raw of
  Just path -> path
  Nothing -> error ("demo fixture holds a path that is not relative: " <> show raw)

child :: OsPath -> String -> OsPath
child parent name = parent </> osp name
