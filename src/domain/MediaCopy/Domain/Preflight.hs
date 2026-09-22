module MediaCopy.Domain.Preflight
  ( HistoryRule (..)
  , TargetFacts (..)
  , OffloadFacts (..)
  , GenerationFacts (..)
  , decideOffload
  , decideGeneration
  ) where

import Ascmhl.Hash
import Ascmhl.Layout (ascmhlDir)
import Ascmhl.Path (RelPath (..), pathText)
import Ascmhl.Types
import Ascmhl.Write (manifestFileName)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (isLeft, isRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (List, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath, takeFileName, unsafeEncodeUtf, (</>))

import MediaCopy.Domain.Destination
import MediaCopy.Domain.FileSystem (Tree (..), ignorePatterns)
import MediaCopy.Domain.History
import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (FormatError (..), JobFormat, settleFormat)
import MediaCopy.Domain.Plan

data HistoryRule = RequireHistory | AllowFresh

data OffloadFacts = OffloadFacts
  { sourceTree :: Maybe Tree
  , targets :: Vector TargetFacts
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
    , expected = readOr Map.empty facts.originals
    , totalBytes = sum (V.map snd files)
    }
  where
    files = maybe V.empty (\tree -> tree.files) facts.sourceTree
    dirs = maybe V.empty (\tree -> tree.dirs) facts.sourceTree
    (chain, recorded) = readOr (Chain {entries = V.empty}, Map.empty) facts.sourceHistory

decideOffload :: JobSpec -> OffloadJob -> OffloadFacts -> JobPlan
decideOffload spec job facts =
  JobPlan
    { spec
    , execution = CopyInto CopyPass {source, process = ProcessTransfer, generations = plannedDests, carried}
    , targets
    , format = settledOf formatResult
    , originsUsed = originsUsedText job.sealFirst src.expected
    , steps
    , totalBytes = src.totalBytes
    , bytesToRead = V.sum (V.map stepBytesToRead steps) + maybe 0 (\pass -> pass.bytes) sealing
    , creates = createdFolders source dests sealing
    , ignorePatterns
    , findings
    , sealPass = sealing
    , generations = highestGeneration src.chain
    }
  where
    source = job.source
    src = readSource facts
    sealing = planSealPass spec.createdAt source src.dirs src.chain job.sealFirst src.recorded src.files
    dests = V.map (\target -> target.root) facts.targets
    classified = V.map (classify job.existingCopy src.fileSet src.dirSet src.byPath) facts.targets
    targets = V.map (\c -> c.target) classified
    destsHeld = V.zip dests (V.map (\c -> c.finals) classified)
    carried = carriedGenerations src.chain sealing
    destGenerations = destGenerationsOf spec.createdAt src.dirs carried facts.targets
    plannedDests = V.mapMaybe (\result -> either (const Nothing) Just result) destGenerations
    formatResult = settleFormat src.expected src.fileSet
    steps = copySteps job destsHeld sealing src
    findings =
      [ findingIf (not src.exists) Finding {severity = Blocker, code = SourceMissing, detail = pathText source}
      , findingIf (src.exists && V.null src.files) Finding {severity = Warning, code = SourceEmpty, detail = pathText source}
      , blockerFor historyCode facts.sourceHistory
      , blockerFor historyCode facts.originals
      , blockerFor formatCode formatResult
      , sealOverHistoryWarning sealing src.chain
      , targetFindings (V.length dests) steps classified
      , classified & V.toList & concatMap (\c -> c.findings)
      , destHistoryBlocker destGenerations
      ]
        & concat
        & nubOrd
        & V.fromList

originsUsedText :: SealFirst -> Map RelPath Hash -> Text
originsUsedText sealFirst expected = case sealFirst of
  SealBeforeCopy _ -> "the seal this job takes first"
  UseHistory -> originsDescription expected

createdFolders :: OsPath -> Vector OsPath -> Maybe SealPass -> Vector OsPath
createdFolders source dests sealing =
  dests & V.toList & (<> maybe [] (const [ascmhlDir source]) sealing) & V.fromList

carriedGenerations :: Chain -> Maybe SealPass -> Int
carriedGenerations chain sealing = highestGeneration chain + (if isJust sealing then 1 else 0)

destGenerationsOf
  :: UTCTime
  -> Vector RelPath
  -> Int
  -> Vector TargetFacts
  -> Vector (Either HistoryError PlannedGeneration)
destGenerationsOf createdAt destDirs carried targets =
  V.map
    (\target -> target.history <&> \_ -> generationNumbered createdAt ProcessTransfer target.root destDirs (carried + 1))
    targets

copySteps :: OffloadJob -> Vector (OsPath, Map RelPath FileSize) -> Maybe SealPass -> SourceRead -> Vector PlanStep
copySteps job destsHeld sealing src =
  stepsInPathOrder
    src.files
    ( \path size ->
        PlanStep
          { path
          , size
          , op = Copy (expectationOf sealing src.expected path)
          , writes = writesFor job.existingCopy destsHeld (path, size)
          }
    )

expectationOf :: Maybe SealPass -> Map RelPath Hash -> RelPath -> Expected
expectationOf sealing expected path = case sealing of
  Just _ -> FromSealPass
  Nothing -> maybe NoOriginal (\sealed -> Recorded sealed) (Map.lookup path expected)

sealOverHistoryWarning :: Maybe SealPass -> Chain -> List Finding
sealOverHistoryWarning sealing chain = case sealing of
  Just _ | not (V.null chain.entries) -> [sealedWarning chain]
  _ -> []

targetFindings :: Int -> Vector PlanStep -> Vector Classified -> List Finding
targetFindings count steps classified =
  V.zip (neededPerDest count steps) classified
    & V.toList
    & mapMaybe (\pair -> targetFinding (fst pair) (snd pair).target)

destHistoryBlocker :: Vector (Either HistoryError a) -> List Finding
destHistoryBlocker results
  | V.any isLeft results =
      [ Finding
          { severity = Blocker
          , code = DestinationHistoryUnreadable
          , detail = "a destination's history cannot be read"
          }
      ]
  | otherwise = []

decideGeneration :: JobSpec -> HistoryRule -> GenerationFacts -> JobPlan
decideGeneration spec rule facts =
  JobPlan
    { spec
    , execution = RecordAt RecordPass {folder, process = ProcessInPlace, generation = generationAt spec.createdAt ProcessInPlace folder dirs chain}
    , targets = V.singleton Target {root = folder, freeBytes = facts.freeBytes, state = Fresh}
    , format = settledOf formatResult
    , originsUsed = if sealed then "the folder's own history" else "none"
    , steps
    , totalBytes = sum (V.map snd disk)
    , bytesToRead = sum (V.map snd disk)
    , creates = V.singleton (ascmhlDir folder)
    , ignorePatterns
    , findings = generationFindings rule folder facts chain formatResult
    , sealPass = Nothing
    , generations = highestGeneration chain
    }
  where
    folder = jobRoot spec.job
    disk = maybe V.empty (\tree -> tree.files) facts.tree
    dirs = maybe V.empty (\tree -> tree.dirs) facts.tree
    (chain, expected) = readOr (Chain {entries = V.empty}, Map.empty) facts.history
    formatResult = settleFormat expected (Set.fromList (V.toList (V.map fst disk)))
    steps = generationSteps expected disk
    sealed = not (V.null chain.entries)

generationFindings :: HistoryRule -> OsPath -> GenerationFacts -> Chain -> Either FormatError JobFormat -> Vector Finding
generationFindings rule folder facts chain formatResult =
  [ findingIf (not exists) Finding {severity = Blocker, code = SourceMissing, detail = pathText folder}
  , case rule of
      RequireHistory
        | exists && not sealed && isRight facts.history ->
            [Finding {severity = Blocker, code = NoSeal, detail = pathText folder}]
      AllowFresh
        | sealed -> [sealedWarning chain]
      _ -> []
  , blockerFor historyCode facts.history
  , blockerFor formatCode formatResult
  ]
    & concat
    & nubOrd
    & V.fromList
  where
    exists = isJust facts.tree
    sealed = not (V.null chain.entries)

planSealPass :: UTCTime -> OsPath -> Vector RelPath -> Chain -> SealFirst -> Map RelPath Hash -> Vector (RelPath, FileSize) -> Maybe SealPass
planSealPass t source dirs chain sealFirst recorded files = case sealFirst of
  UseHistory -> Nothing
  SealBeforeCopy policy ->
    let steps = generationSteps recorded files
    in Just
         SealPass
           { steps
           , bytes = sum (V.map (\step -> step.size) steps)
           , onFailure = policy
           , generation = generationAt t ProcessInPlace source dirs chain
           }

sealedWarning :: Chain -> Finding
sealedWarning chain =
  Finding {severity = Warning, code = AlreadySealed, detail = "generation " <> T.pack (show (highestGeneration chain))}

stepsInPathOrder :: Vector (RelPath, a) -> (RelPath -> a -> PlanStep) -> Vector PlanStep
stepsInPathOrder rows build =
  rows
    & V.toList
    & sortOn (\pair -> fst pair)
    & map (\pair -> build (fst pair) (snd pair))
    & V.fromList

generationSteps :: Map RelPath Hash -> Vector (RelPath, FileSize) -> Vector PlanStep
generationSteps expected disk =
  stepsInPathOrder
    (planVerify expected (V.map (\pair -> fst pair) disk))
    (\path op -> PlanStep {path, size = Map.findWithDefault 0 path sizes, op, writes = V.empty})
  where
    sizes = Map.fromList (V.toList disk)

settledOf :: Either FormatError JobFormat -> Maybe JobFormat
settledOf result = case result of
  Left _ -> Nothing
  Right fmt -> Just fmt

generationAt :: UTCTime -> ProcessKind -> OsPath -> Vector RelPath -> Chain -> PlannedGeneration
generationAt t process folder dirs chain = generationNumbered t process folder dirs (1 + highestGeneration chain)

generationNumbered :: UTCTime -> ProcessKind -> OsPath -> Vector RelPath -> Int -> PlannedGeneration
generationNumbered t process folder dirs number =
  PlannedGeneration
    { folder
    , number
    , manifest = ascmhlDir folder </> unsafeEncodeUtf (T.unpack name)
    , process
    , directories = dirs
    }
  where
    name = manifestFileName number (pathText (takeFileName folder)) t
