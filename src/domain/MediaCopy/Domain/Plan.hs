module MediaCopy.Domain.Plan
  ( Expected (..)
  , WriteMode (..)
  , PlannedWrite (..)
  , PlannedGeneration (..)
  , FileOp (..)
  , planVerify
  , PlanStep (..)
  , stepBytesToRead
  , TargetState (..)
  , Target (..)
  , Severity (..)
  , FindingCode (..)
  , Finding (..)
  , findingIf
  , blockerFor
  , historyCode
  , formatCode
  , SealPass (..)
  , CopyPass (..)
  , RecordPass (..)
  , PlanExecution (..)
  , JobPlan (..)
  , planBlocked
  , blockers
  , planEquivalent
  , plannedGenerations
  , planRaceChecks
  ) where

import Ascmhl.Hash (Hash)
import Ascmhl.Path (RelPath)
import Ascmhl.Types (ProcessKind)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath)

import MediaCopy.Domain.History (HistoryError (..))
import MediaCopy.Domain.Job (FileSize, JobSpec, OnSealFailure)
import MediaCopy.Domain.JobFormat (FormatError (..), JobFormat)

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

data Expected
  = NoOriginal
  | Recorded Hash
  | FromSealPass
  deriving stock (Eq, Show)

data WriteMode
  = WriteNew
  | Overwrite
  | Reuse
  deriving stock (Eq, Show)

-- |
-- >>> map display [WriteNew, Overwrite, Reuse]
-- ["copy","overwrite","reuse"]
instance Display WriteMode where
  displayBuilder = \case
    WriteNew -> "copy"
    Overwrite -> "overwrite"
    Reuse -> "reuse"

data PlannedWrite = PlannedWrite
  { final :: OsPath
  , temp :: OsPath
  , mode :: WriteMode
  }
  deriving stock (Eq, Show)

data FileOp
  = Copy Expected
  | VerifyAgainst Hash
  | ReportMissing
  | ReportNew
  deriving stock (Eq, Show)

planVerify :: Map RelPath Hash -> Vector RelPath -> Vector (RelPath, FileOp)
planVerify manifest disk = onDisk <> missing
  where
    diskSet = Set.fromList (V.toList disk)
    onDisk = V.map (\p -> (p, maybe ReportNew VerifyAgainst (Map.lookup p manifest))) disk
    missing =
      Map.keys manifest
        & filter (\p -> not (Set.member p diskSet))
        & map (\p -> (p, ReportMissing))
        & V.fromList

data PlannedGeneration = PlannedGeneration
  { folder :: OsPath
  , number :: Int
  , manifest :: OsPath
  , process :: ProcessKind
  , directories :: Vector RelPath
  }
  deriving stock (Eq, Show)

data PlanStep = PlanStep
  { path :: RelPath
  , size :: FileSize
  , op :: FileOp
  , writes :: Vector PlannedWrite
  }
  deriving stock (Eq, Show)

stepBytesToRead :: PlanStep -> Int64
stepBytesToRead step =
  let reuses = V.length (V.filter (\w -> w.mode == Reuse) step.writes)
      allReuse = reuses == V.length step.writes
      hasExpected = case step.op of
        (Copy (Recorded _); Copy FromSealPass) -> True
        _ -> False
      sourceBytes = if allReuse && hasExpected then 0 else step.size
  in sourceBytes + fromIntegral (V.length step.writes) * step.size

data TargetState = Fresh | NotEmpty | Absent | Partial
  deriving stock (Eq, Show)

-- |
-- >>> map display [Fresh, NotEmpty, Absent, Partial]
-- ["empty","not empty","will be created","partial copy"]
instance Display TargetState where
  displayBuilder = \case
    Fresh -> "empty"
    NotEmpty -> "not empty"
    Absent -> "will be created"
    Partial -> "partial copy"

data Target = Target
  { root :: OsPath
  , freeBytes :: Maybe Int64
  , state :: TargetState
  }
  deriving stock (Eq, Show)

data Severity = Blocker | Warning
  deriving stock (Eq, Ord, Show)

-- | >>> map display [Blocker, Warning]
-- ["blocker","warning"]
instance Display Severity where
  displayBuilder = \case
    Blocker -> "blocker"
    Warning -> "warning"

data FindingCode
  = SourceMissing
  | SourceEmpty
  | DestinationPartial
  | DestinationForeign
  | DestinationOtherSource
  | DestinationUnavailable
  | DestinationHistoryUnreadable
  | InsufficientSpace
  | FormatUnsettled
  | ChainNamesNoManifest
  | ChainUnreadable
  | ManifestUnreadable
  | NoSeal
  | AlreadySealed
  deriving stock (Eq, Ord, Show)

-- |
-- >>> display DestinationForeign
-- "destination holds files that are not on the media source"
instance Display FindingCode where
  displayBuilder = \case
    SourceMissing -> "source not found"
    SourceEmpty -> "source is empty"
    DestinationPartial -> "destination holds a partial copy"
    DestinationForeign -> "destination holds files that are not on the media source"
    DestinationOtherSource -> "destination was copied from another media source"
    DestinationUnavailable -> "destination cannot be read"
    DestinationHistoryUnreadable -> "destination history cannot be read"
    InsufficientSpace -> "not enough space"
    FormatUnsettled -> "hash format cannot be settled"
    ChainNamesNoManifest -> "chain names no manifest"
    ChainUnreadable -> "chain cannot be read"
    ManifestUnreadable -> "a manifest the chain names cannot be read"
    NoSeal -> "folder has no history"
    AlreadySealed -> "folder is already sealed"

data Finding = Finding
  { severity :: Severity
  , code :: FindingCode
  , detail :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | >>> findingIf False Finding {severity = Warning, code = NoSeal, detail = "card"}
-- []
-- >>> findingIf True Finding {severity = Warning, code = NoSeal, detail = "card"}
-- [Finding {severity = Warning, code = NoSeal, detail = "card"}]
findingIf :: Bool -> Finding -> List Finding
findingIf holds finding
  | holds = [finding]
  | otherwise = []

blockerFor :: (Display e) => (e -> FindingCode) -> Either e a -> List Finding
blockerFor codeOf = \case
  Left e -> [Finding {severity = Blocker, code = codeOf e, detail = display e}]
  Right _ -> []

-- | >>> historyCode (ChainEmpty (unsafeEncodeUtf "ascmhl_chain.xml"))
-- ChainNamesNoManifest
-- >>> historyCode (ChainNotParsed (unsafeEncodeUtf "ascmhl_chain.xml") "not well formed")
-- ChainUnreadable
-- >>> historyCode (ManifestNotFound (unsafeEncodeUtf "0001_a.mhl"))
-- ManifestUnreadable
historyCode :: HistoryError -> FindingCode
historyCode = \case
  ChainNotParsed _ _ -> ChainUnreadable
  ChainEmpty _ -> ChainNamesNoManifest
  (ManifestNotFound _; ManifestNotParsed _ _; HistoryUnreadable _ _) -> ManifestUnreadable

-- | >>> formatCode (MixedFormats [])
-- FormatUnsettled
formatCode :: FormatError -> FindingCode
formatCode = \case
  MixedFormats _ -> FormatUnsettled

data CopyPass = CopyPass
  { source :: OsPath
  , process :: ProcessKind
  , generations :: Vector PlannedGeneration
  , carried :: Int
  }
  deriving stock (Eq, Show)

data RecordPass = RecordPass
  { folder :: OsPath
  , process :: ProcessKind
  , generation :: PlannedGeneration
  }
  deriving stock (Eq, Show)

data PlanExecution
  = CopyInto CopyPass
  | RecordAt RecordPass
  deriving stock (Eq, Show)

data SealPass = SealPass
  { steps :: Vector PlanStep
  , bytes :: Int64
  , onFailure :: OnSealFailure
  , generation :: PlannedGeneration
  }
  deriving stock (Eq, Show)

data JobPlan = JobPlan
  { spec :: JobSpec
  , execution :: PlanExecution
  , targets :: Vector Target
  , format :: Maybe JobFormat
  , originsUsed :: Text
  , steps :: Vector PlanStep
  , totalBytes :: Int64
  , bytesToRead :: Int64
  , creates :: Vector OsPath
  , ignorePatterns :: Vector Text
  , findings :: Vector Finding
  , sealPass :: Maybe SealPass
  , generations :: Int
  }
  deriving stock (Eq, Show)

blockers :: JobPlan -> Vector Finding
blockers plan = V.filter (\finding -> finding.severity == Blocker) plan.findings

planBlocked :: JobPlan -> Bool
planBlocked plan = not (V.null (blockers plan))

planEquivalent :: JobPlan -> JobPlan -> Bool
planEquivalent a b = blankFree a == blankFree b
  where
    blankFree plan = plan {targets = V.map (\target -> target {freeBytes = Nothing}) plan.targets}

planRaceChecks :: JobPlan -> Vector (OsPath, Int)
planRaceChecks plan = case plan.execution of
  RecordAt record -> V.singleton (record.folder, record.generation.number)
  CopyInto copy -> V.singleton (copy.source, maybe (copy.carried + 1) (\pass -> pass.generation.number) plan.sealPass)

plannedGenerations :: JobPlan -> Vector PlannedGeneration
plannedGenerations plan = sealed <> executed
  where
    sealed = maybe V.empty (\pass -> V.singleton pass.generation) plan.sealPass
    executed = case plan.execution of
      CopyInto copy -> copy.generations
      RecordAt record -> V.singleton record.generation
