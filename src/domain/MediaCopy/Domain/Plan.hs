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

-- | Where a copy's expected hash comes from.
data Expected
  = NoOriginal
  | Recorded Hash
  | FromSealPass
  deriving stock (Eq, Show)

-- | What the engine does at one destination for one file.
data WriteMode
  = WriteNew
  | -- | The final exists. The part file is renamed over it.
    Overwrite
  | -- | The final exists with the source's size. The engine hashes it and writes only on a mismatch.
    Reuse
  deriving stock (Eq, Show)

-- | The word the plan sheet puts on a write.
--
-- >>> map display [WriteNew, Overwrite, Reuse]
-- ["copy","overwrite","reuse"]
instance Display WriteMode where
  displayBuilder = \case
    WriteNew -> "copy"
    Overwrite -> "overwrite"
    Reuse -> "reuse"

-- | The plan names both files a copy touches, so the engine never builds a path of its own.
data PlannedWrite = PlannedWrite
  { final :: OsPath
  , temp :: OsPath
  , mode :: WriteMode
  }
  deriving stock (Eq, Show)

data FileOp
  = -- | The payload names where the expectation comes from, never a hash that the plan cannot know.
    Copy Expected
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

-- | The plan names the generation number and the file, so the engine writes the manifest
-- the operator approved.
data PlannedGeneration = PlannedGeneration
  { folder :: OsPath
  , number :: Int
  , manifest :: OsPath
  , process :: ProcessKind
  , directories :: Vector RelPath
  -- ^ The directories of 'folder' alone. A manifest describes one tree, so no other
  -- generation can share this set.
  }
  deriving stock (Eq, Show)

data PlanStep = PlanStep
  { path :: RelPath
  , size :: FileSize
  , op :: FileOp
  , writes :: Vector PlannedWrite
  -- ^ One for each target of a 'Copy', and none for any other operation.
  }
  deriving stock (Eq, Show)

-- | The bytes the engine reads for one step: the plan's account, and the charge on a failed step.
-- A step every destination reuses reads the source only when the plan names no hash for it.
--
-- Every destination is read once, whatever its mode: a reused one to check what is there, a written
-- one to check the copy back. A mismatch makes the engine write and read a destination twice, which
-- no plan can predict, so a step that ends 'Replaced' reads more than its account. 'FromSealPass'
-- counts as a recorded hash, because the seal pass records every path the copy pass plans, so the
-- lookup never misses.
stepBytesToRead :: PlanStep -> Int64
stepBytesToRead step =
  let reuses = V.length (V.filter (\w -> w.mode == Reuse) step.writes)
      allReuse = reuses == V.length step.writes
      hasExpected = case step.op of
        (Copy (Recorded _); Copy FromSealPass) -> True
        _ -> False
      sourceBytes = if allReuse && hasExpected then 0 else step.size
  in sourceBytes + fromIntegral (V.length step.writes) * step.size

-- | 'Absent' is a folder the job will create, not a fault. 'Partial' passed the strict check; 'NotEmpty' failed it.
data TargetState = Fresh | NotEmpty | Absent | Partial
  deriving stock (Eq, Show)

-- | The word the plan sheet puts on a destination.
--
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
  -- ^ 'Nothing' when the free-space call failed, or when no call was made because the folder is not there.
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

-- | The wording every finding reaches the operator under.
--
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

-- | What the plan learned that the operator must know.
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
  -- No plan reaches the last of these three. 'carryHistory' and 'readHistory' raise
  -- 'HistoryUnreadable' while a job runs, and a running job's fault is a 'PlanViolation', not a
  -- finding. The alternative exists to stay total.
  (ManifestNotFound _; ManifestNotParsed _ _; HistoryUnreadable _ _) -> ManifestUnreadable

-- | >>> formatCode (MixedFormats [])
-- FormatUnsettled
formatCode :: FormatError -> FindingCode
formatCode = \case
  MixedFormats _ -> FormatUnsettled

-- | The engine reads from 'source', writes every step into each of the plan's targets, and records a
-- generation in each of them.
data CopyPass = CopyPass
  { source :: OsPath
  , process :: ProcessKind
  , generations :: Vector PlannedGeneration
  -- ^ One for each target, in the order of 'targets'.
  , carried :: Int
  -- ^ The generations of the media source's history that every destination receives before its own, the seal pass's included. Each planned generation is numbered @carried + 1@.
  }
  deriving stock (Eq, Show)

-- | The engine hashes the steps where they already are, and records a generation in that same folder.
data RecordPass = RecordPass
  { folder :: OsPath
  , process :: ProcessKind
  , generation :: PlannedGeneration
  }
  deriving stock (Eq, Show)

-- | What the engine does with 'steps'. The plan says it, so execution never reads the job
-- a second time to find its shape. The two can never disagree.
data PlanExecution
  = CopyInto CopyPass
  | RecordAt RecordPass
  deriving stock (Eq, Show)

-- | 'bytes' is what the seal reads. The plan never charges it against a destination's free space.
data SealPass = SealPass
  { steps :: Vector PlanStep
  , bytes :: Int64
  , onFailure :: OnSealFailure
  , generation :: PlannedGeneration
  -- ^ A seal records the media source where it stands, so its process is in-place by definition.
  }
  deriving stock (Eq, Show)

-- | 'format' is 'Nothing' only when 'FormatUnsettled' blocks the plan, so no execution reads it.
data JobPlan = JobPlan
  { spec :: JobSpec
  , execution :: PlanExecution
  , targets :: Vector Target
  , format :: Maybe JobFormat
  , originsUsed :: Text
  , steps :: Vector PlanStep
  , totalBytes :: Int64
  , bytesToRead :: Int64
  -- ^ Every byte the job reads, both passes, so a seal job's progress does not finish at its halfway point.
  , creates :: Vector OsPath
  -- ^ The folders the job brings into being: each destination's root, and the media source's @ascmhl\/@ under a seal pass. The writers make what they need; this is the operator's account.
  , ignorePatterns :: Vector Text
  , findings :: Vector Finding
  , sealPass :: Maybe SealPass
  , generations :: Int
  -- ^ What the job's own folder already holds, so the sheet can say so before the operator chooses.
  }
  deriving stock (Eq, Show)

blockers :: JobPlan -> Vector Finding
blockers plan = V.filter (\finding -> finding.severity == Blocker) plan.findings

planBlocked :: JobPlan -> Bool
planBlocked plan = not (V.null (blockers plan))

-- | The comparison ignores free space, because a disk that gained a log line did not change
-- the job. A change that matters reaches 'findings' as 'InsufficientSpace'.
planEquivalent :: JobPlan -> JobPlan -> Bool
planEquivalent a b = blankFree a == blankFree b
  where
    blankFree plan = plan {targets = V.map (\target -> target {freeBytes = Nothing}) plan.targets}

-- | The folder whose chain decides whether the plan is still current, and the number that folder
-- must be about to write. A destination carries the media source's history, so the media source's
-- chain answers for every destination. A seal pass answers for them all.
planRaceChecks :: JobPlan -> Vector (OsPath, Int)
planRaceChecks plan = case plan.execution of
  RecordAt record -> V.singleton (record.folder, record.generation.number)
  CopyInto copy -> V.singleton (copy.source, maybe (copy.carried + 1) (\pass -> pass.generation.number) plan.sealPass)

-- | Every generation a plan will write, the seal pass's first. A race on the media source
-- is then caught before a byte of it is read.
plannedGenerations :: JobPlan -> Vector PlannedGeneration
plannedGenerations plan = sealed <> executed
  where
    sealed = maybe V.empty (\pass -> V.singleton pass.generation) plan.sealPass
    executed = case plan.execution of
      CopyInto copy -> copy.generations
      RecordAt record -> V.singleton record.generation
