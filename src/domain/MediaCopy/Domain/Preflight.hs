module MediaCopy.Domain.Preflight
  ( HistoryRule (..)
  , OffloadFacts (..)
  , GenerationFacts (..)
  , offload
  , generation
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Chain (Chain)
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry (ChainEntry)
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Layout qualified as Layout
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.Process (Process)
import Ascmhl.Process qualified as Process
import Ascmhl.RelPath (RelPath)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import System.OsPath (takeFileName, unsafeEncodeUtf, (</>))

import MediaCopy.Domain.Destination (Destination)
import MediaCopy.Domain.Destination qualified as Destination
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.Finding (Finding)
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.HashFormat (FormatError)
import MediaCopy.Domain.HashFormat qualified as HashFormat
import MediaCopy.Domain.HistoryError (HistoryError)
import MediaCopy.Domain.HistoryError qualified as HistoryError
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload (Offload)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PlannedGeneration (PlannedGeneration)
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Domain.Tree (Tree)
import MediaCopy.Domain.Tree qualified as Tree

data HistoryRule = RequireHistory | AllowFresh

data OffloadFacts = OffloadFacts
  { sourceTree :: Maybe Tree
  , targets :: Vector Destination
  , originals :: Either HistoryError (Maybe (Map RelPath Hash))
  , sourceHistory :: Either HistoryError (Maybe (Chain, Map RelPath Hash))
  }

data GenerationFacts = GenerationFacts
  { tree :: Maybe Tree
  , freeBytes :: Maybe Int64
  , history :: Either HistoryError (Maybe (Chain, Map RelPath Hash))
  }

data SourceRead = SourceRead
  { exists :: Bool
  , files :: Vector (RelPath, FileSize)
  , dirs :: Vector RelPath
  , fileSet :: Set RelPath
  , dirSet :: Set RelPath
  , chain :: Chain
  , recorded :: Map RelPath Hash
  , byPath :: Map RelPath ChainEntry
  , expected :: Map RelPath Hash
  , totalBytes :: Int64
  }

readSource :: OffloadFacts -> SourceRead
readSource facts =
  SourceRead
    { exists = isJust facts.sourceTree
    , files
    , dirs
    , fileSet = Set.fromList (V.toList (V.map fst files))
    , dirSet = Set.fromList (V.toList dirs)
    , chain
    , recorded
    , byPath = chain.entries & V.toList & map (\entry -> (entry.path, entry)) & Map.fromList
    , expected = HistoryError.readOr Map.empty facts.originals
    , totalBytes = sum (V.map snd files)
    }
  where
    files = maybe V.empty (.files) facts.sourceTree
    dirs = maybe V.empty (.dirs) facts.sourceTree
    (chain, recorded) = HistoryError.readOr (Chain.Chain {entries = V.empty}, Map.empty) facts.sourceHistory

offload :: JobSpec -> Offload -> OffloadFacts -> Plan
offload spec job facts =
  Plan.Plan
    { spec
    , execution = Plan.CopyInto Plan.CopyPass {source, generations = plannedDests, carried}
    , targets
    , format = settledOf formatResult
    , originsUsed = originsUsedText job.sealFirst src.expected
    , steps
    , totalBytes = src.totalBytes
    , bytesToRead = V.sum (V.map PlanStep.bytesToRead steps) + maybe 0 (.bytes) sealing
    , creates = createdFolders source dests sealing
    , ignorePatterns = Tree.ignorePatterns
    , findings
    , sealPass = sealing
    , generations = Chain.highestGeneration src.chain
    , plugins = PluginPlan.empty
    }
  where
    source = job.source
    src = readSource facts
    sealing = planSealPass spec.createdAt source src.dirs src.chain job.sealFirst src.recorded src.files
    dests = V.map (.root) facts.targets
    classified = V.map (Destination.classify job.existingCopy src.fileSet src.dirSet src.byPath) facts.targets
    targets = V.map (.target) classified
    destsHeld = V.zip dests (V.map (.finals) classified)
    carried = carriedGenerations src.chain sealing
    destGenerations = destGenerationsOf spec.createdAt src.dirs carried facts.targets
    plannedDests = V.mapMaybe (either (const Nothing) Just) destGenerations
    formatResult = HashFormat.settle src.expected src.fileSet
    steps = copySteps job destsHeld sealing src
    findings =
      [ Finding.onlyIf (not src.exists) Finding.Finding {severity = Severity.Blocker, code = Finding.SourceMissing, detail = OsPath.toText source}
      , Finding.onlyIf (src.exists && V.null src.files) Finding.Finding {severity = Severity.Warning, code = Finding.SourceEmpty, detail = OsPath.toText source}
      , Finding.blockerFor Finding.historyCode facts.sourceHistory
      , Finding.blockerFor Finding.historyCode facts.originals
      , Finding.blockerFor Finding.formatCode formatResult
      , sealOverHistoryWarning sealing src.chain
      , targetFindings (V.length dests) steps classified
      , classified & V.toList & concatMap (.findings)
      , destHistoryBlocker destGenerations
      ]
        & concat
        & nubOrd
        & V.fromList

originsUsedText :: Offload.SealFirst -> Map RelPath Hash -> Text
originsUsedText sealFirst expected = case sealFirst of
  Offload.SealBeforeCopy _ -> "the seal this job takes first"
  Offload.UseHistory -> Plan.originsDescription expected

createdFolders :: OsPath -> Vector OsPath -> Maybe Plan.SealPass -> Vector OsPath
createdFolders source dests sealing =
  dests & V.toList & (<> maybe [] (const [Layout.dir source]) sealing) & V.fromList

carriedGenerations :: Chain -> Maybe Plan.SealPass -> Int
carriedGenerations chain sealing = Chain.highestGeneration chain + (if isJust sealing then 1 else 0)

destGenerationsOf
  :: UTCTime
  -> Vector RelPath
  -> Int
  -> Vector Destination
  -> Vector (Either HistoryError PlannedGeneration)
destGenerationsOf createdAt destDirs carried targets =
  V.map
    (\target -> target.history <&> \_ -> generationNumbered createdAt Process.Transfer target.root destDirs (carried + 1))
    targets

copySteps :: Offload -> Vector (OsPath, Map RelPath FileSize) -> Maybe Plan.SealPass -> SourceRead -> Vector PlanStep
copySteps job destsHeld sealing src =
  stepsInPathOrder
    src.files
    ( \path size ->
        PlanStep.PlanStep
          { path
          , size
          , op = PlanStep.Copy (expectationOf sealing src.expected path)
          , writes = Destination.writes job.existingCopy destsHeld (path, size)
          }
    )

expectationOf :: Maybe Plan.SealPass -> Map RelPath Hash -> RelPath -> PlanStep.Expected
expectationOf sealing expected path = case sealing of
  Just _ -> PlanStep.FromSealPass
  Nothing -> maybe PlanStep.NoOriginal PlanStep.Recorded (Map.lookup path expected)

sealOverHistoryWarning :: Maybe Plan.SealPass -> Chain -> List Finding
sealOverHistoryWarning sealing chain = case sealing of
  Just _ | not (V.null chain.entries) -> [sealedWarning chain]
  _ -> []

targetFindings :: Int -> Vector PlanStep -> Vector Destination.Classified -> List Finding
targetFindings count steps classified =
  V.zip (Destination.bytesNeeded count steps) classified
    & V.toList
    & mapMaybe (\pair -> Destination.spaceFinding (fst pair) (snd pair).target)

destHistoryBlocker :: Vector (Either HistoryError a) -> List Finding
destHistoryBlocker results
  | V.any isLeft results =
      [ Finding.Finding
          { severity = Severity.Blocker
          , code = Finding.DestinationHistoryUnreadable
          , detail = "a destination's history cannot be read"
          }
      ]
  | otherwise = []

generation :: JobSpec -> HistoryRule -> GenerationFacts -> Plan
generation spec rule facts =
  Plan.Plan
    { spec
    , execution = Plan.RecordAt Plan.RecordPass {folder, generation = generationAt spec.createdAt folder dirs chain}
    , targets = V.singleton Target.Target {root = folder, freeBytes = facts.freeBytes, state = Target.Fresh}
    , format = settledOf formatResult
    , originsUsed = if sealed then "the folder's own history" else "none"
    , steps
    , totalBytes = sum (V.map snd disk)
    , bytesToRead = sum (V.map snd disk)
    , creates = V.singleton (Layout.dir folder)
    , ignorePatterns = Tree.ignorePatterns
    , findings = generationFindings rule folder facts chain formatResult
    , sealPass = Nothing
    , generations = Chain.highestGeneration chain
    , plugins = PluginPlan.empty
    }
  where
    folder = Job.root spec.job
    disk = maybe V.empty (.files) facts.tree
    dirs = maybe V.empty (.dirs) facts.tree
    (chain, expected) = HistoryError.readOr (Chain.Chain {entries = V.empty}, Map.empty) facts.history
    formatResult = HashFormat.settle expected (Set.fromList (V.toList (V.map fst disk)))
    steps = generationSteps expected disk
    sealed = not (V.null chain.entries)

generationFindings :: HistoryRule -> OsPath -> GenerationFacts -> Chain -> Either FormatError Algo -> Vector Finding
generationFindings rule folder facts chain formatResult =
  [ Finding.onlyIf (not exists) Finding.Finding {severity = Severity.Blocker, code = Finding.SourceMissing, detail = OsPath.toText folder}
  , case rule of
      RequireHistory
        | exists && not sealed && isRight facts.history ->
            [Finding.Finding {severity = Severity.Blocker, code = Finding.NoSeal, detail = OsPath.toText folder}]
      AllowFresh
        | sealed -> [sealedWarning chain]
      _ -> []
  , Finding.blockerFor Finding.historyCode facts.history
  , Finding.blockerFor Finding.formatCode formatResult
  ]
    & concat
    & nubOrd
    & V.fromList
  where
    exists = isJust facts.tree
    sealed = not (V.null chain.entries)

planSealPass :: UTCTime -> OsPath -> Vector RelPath -> Chain -> Offload.SealFirst -> Map RelPath Hash -> Vector (RelPath, FileSize) -> Maybe Plan.SealPass
planSealPass t source dirs chain sealFirst recorded files = case sealFirst of
  Offload.UseHistory -> Nothing
  Offload.SealBeforeCopy policy ->
    let steps = generationSteps recorded files
    in Just
         Plan.SealPass
           { steps
           , bytes = sum (V.map (.size) steps)
           , onFailure = policy
           , generation = generationAt t source dirs chain
           }

sealedWarning :: Chain -> Finding
sealedWarning chain =
  Finding.Finding {severity = Severity.Warning, code = Finding.AlreadySealed, detail = "generation " <> T.show (Chain.highestGeneration chain)}

stepsInPathOrder :: Vector (RelPath, a) -> (RelPath -> a -> PlanStep) -> Vector PlanStep
stepsInPathOrder rows build =
  rows
    & V.toList
    & sortOn fst
    & map (\pair -> build (fst pair) (snd pair))
    & V.fromList

generationSteps :: Map RelPath Hash -> Vector (RelPath, FileSize) -> Vector PlanStep
generationSteps expected disk =
  stepsInPathOrder
    (PlanStep.verifyOps expected (V.map fst disk))
    (\path op -> PlanStep.PlanStep {path, size = Map.findWithDefault 0 path sizes, op, writes = V.empty})
  where
    sizes = Map.fromList (V.toList disk)

settledOf :: Either FormatError Algo -> Maybe Algo
settledOf result = case result of
  Left _ -> Nothing
  Right fmt -> Just fmt

generationAt :: UTCTime -> OsPath -> Vector RelPath -> Chain -> PlannedGeneration
generationAt t folder dirs chain = generationNumbered t Process.InPlace folder dirs (1 + Chain.highestGeneration chain)

generationNumbered :: UTCTime -> Process -> OsPath -> Vector RelPath -> Int -> PlannedGeneration
generationNumbered t process folder dirs number =
  PlannedGeneration.PlannedGeneration
    { folder
    , number
    , manifest = Layout.dir folder </> unsafeEncodeUtf (T.unpack name)
    , process
    , directories = dirs
    }
  where
    name = Manifest.fileName number (OsPath.toText (takeFileName folder)) t
