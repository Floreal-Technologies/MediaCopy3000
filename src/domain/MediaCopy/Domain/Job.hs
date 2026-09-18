module MediaCopy.Domain.Job
  ( JobId (..)
  , FileSize
  , JobKind (..)
  , SealFirst (..)
  , OnSealFailure (..)
  , ExistingCopy (..)
  , OffloadJob (..)
  , VerifyJob (..)
  , SealJob (..)
  , Job (..)
  , Mismatch (..)
  , JobSpec (..)
  , FileOutcome (..)
  , FileStatus (..)
  , Doing (..)
  , FileEntry (..)
  , PlannedWork (..)
  , JobEvent (..)
  , SealStopped (..)
  , JobResult (..)
  , JobPhase (..)
  , JobState (..)
  , Counts (..)
  , jobKind
  , jobRoot
  , historyFolder
  , jobLabel
  , allOkText
  , originsDescription
  , newJobState
  , foldEvent
  , outcomeFailed
  , isFailure
  , isDone
  , isTerminalEvent
  , isTerminal
  , countOutcomes
  , destinationPath
  , fractionOf
  ) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath, pathText)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import System.OsPath (OsPath, takeFileName, (</>))

newtype JobId = JobId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

type FileSize = Int64

data JobKind = OffloadKind | VerifyKind | SealKind
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Display JobKind where
  displayBuilder = \case
    OffloadKind -> "offload"
    VerifyKind -> "verify"
    SealKind -> "seal"

-- | What an offload does about the media source's originals before it copies.
data SealFirst
  = UseHistory
  | SealBeforeCopy OnSealFailure
  deriving stock (Eq, Show)

-- | What a job does when its seal pass records a failure.
data OnSealFailure = StopBeforeCopy | CopyAnyway
  deriving stock (Eq, Show)

instance Display OnSealFailure where
  displayBuilder = \case
    StopBeforeCopy -> "stop before copying"
    CopyAnyway -> "copy anyway"

-- | What an offload does with a destination that already holds part of the copy.
data ExistingCopy = Resume | Replace
  deriving stock (Eq, Show)

instance Display ExistingCopy where
  displayBuilder = \case
    Resume -> "resume"
    Replace -> "replace"

data OffloadJob = OffloadJob
  { source :: OsPath
  , destinations :: NonEmpty OsPath
  , sealFirst :: SealFirst
  , existingCopy :: Maybe ExistingCopy
  -- ^ 'Nothing' while the operator has not chosen. A partial destination blocks the plan until then.
  }
  deriving stock (Eq, Show)

data VerifyJob = VerifyJob
  { folder :: OsPath
  }
  deriving stock (Eq, Show)

data SealJob = SealJob
  { folder :: OsPath
  }
  deriving stock (Eq, Show)

data Job
  = Offload OffloadJob
  | VerifyFolder VerifyJob
  | SealMediaSource SealJob
  deriving stock (Eq, Show)

-- | The three operations, under the names the manual's glossary gives them.
jobKind :: Job -> JobKind
jobKind = \case
  Offload _ -> OffloadKind
  VerifyFolder _ -> VerifyKind
  SealMediaSource _ -> SealKind

-- | The one folder a job reads. An offload reads its media source. Nothing else has a second root.
jobRoot :: Job -> OsPath
jobRoot = \case
  Offload oj -> oj.source
  VerifyFolder vj -> vj.folder
  SealMediaSource sj -> sj.folder

-- | The folder whose own history the job appends to. An offload writes to its destinations, not to a folder it already knows.
historyFolder :: Job -> Maybe OsPath
historyFolder = \case
  Offload _ -> Nothing
  VerifyFolder vj -> Just vj.folder
  SealMediaSource sj -> Just sj.folder

jobLabel :: Job -> Text
jobLabel job = pathText (takeFileName (jobRoot job))

-- | A seal's common case records a first hash rather than checking one, so it must not claim "verified".
allOkText :: JobKind -> Text
allOkText = \case
  (OffloadKind; VerifyKind) -> "finished, all files verified"
  SealKind -> "finished, all files sealed"

-- | This text is the only thing that tells the user which originals the job consulted.
originsDescription :: Map RelPath Hash -> Text
originsDescription resolved
  | Map.null resolved = "none found – every file recorded as original"
  | otherwise = "the media source's own history, " <> T.pack (show (Map.size resolved)) <> " files"

data JobSpec = JobSpec
  { jobId :: JobId
  , job :: Job
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show)

data Mismatch = Mismatch
  { expected :: Hash
  , actual :: Hash
  }
  deriving stock (Eq, Show)

data FileOutcome
  = Ok
  | HashMismatch Mismatch
  | Missing
  | New
  | IoError Text
  | -- | A reused destination file did not match and was copied again. Not a failure.
    Replaced Mismatch
  deriving stock (Eq, Show)

-- | 'Flushing' and 'Publishing' are the two windows that move no data. The first waits for the
-- disk to take what the copy wrote. The second renames the copy and commits the directory entry.
-- Neither can report progress, so each reports itself instead.
data FileStatus
  = Pending
  | Hashing
  | Copying
  | Flushing
  | Publishing
  | Verifying
  | Done FileOutcome
  deriving stock (Eq, Show)

-- | The last thing a job reported about itself. The second kind exists because a job ends by
-- writing its manifest and synchronising it, which moves no byte of the copy and belongs to no
-- file.
data Doing
  = OnFile FileStatus
  | WritingManifest
  deriving stock (Eq, Show)

-- | What a job's line says it is doing when it cannot say a rate.
instance Display Doing where
  displayBuilder = \case
    OnFile status -> displayBuilder status
    WritingManifest -> "Writing the manifest"

data JobResult = AllOk | WithFailures Int
  deriving stock (Eq, Show)

-- | What a job's plan came to. Its own type, so both fields belong to every value that has them.
-- A record on one alternative of 'JobEvent' would make each field a partial one.
data PlannedWork = PlannedWork
  { files :: Vector (RelPath, FileSize)
  -- ^ Every file the plan covers, with its size.
  , bytesToRead :: Int64
  -- ^ The bytes the job reads. A reused file is planned but not read.
  }
  deriving stock (Eq, Show)

data JobEvent
  = Planned PlannedWork
  | FileStatusChanged RelPath FileStatus
  | Progress Int64
  | -- | The start of the phase 'MhlWritten' ends: the manifest is built, written and synchronised.
    ManifestWriting
  | MhlWritten OsPath
  | OriginalsResolved Text HashAlgo
  | -- | Posted by the runtime before the engine starts, so the report can name the file.
    LogOpened OsPath
  | JobFinished JobResult
  | JobFailed Text
  deriving stock (Eq, Show)

-- | What a file's row and the event log say about a file.
instance Display FileStatus where
  displayBuilder = \case
    Pending -> "Pending"
    Hashing -> "Hashing"
    Copying -> "Copying"
    Flushing -> "Saving to disk"
    Publishing -> "Naming the copy"
    Verifying -> "Verifying"
    Done Ok -> "Verified"
    Done (HashMismatch _) -> "Hash mismatch"
    Done Missing -> "Missing"
    Done New -> "New (not in manifest)"
    Done (IoError message) -> "I/O error: " <> displayBuilder message
    Done (Replaced _) -> "Replaced after mismatch"

-- | One line of the event log per event.
instance Display JobEvent where
  displayBuilder = \case
    Planned (PlannedWork fs toRead) ->
      "planned "
        <> displayBuilder (T.pack (show (V.length fs)))
        <> " files, "
        <> displayBuilder (T.pack (show toRead))
        <> " bytes to read"
    FileStatusChanged p s -> displayBuilder p <> " " <> displayBuilder s
    Progress n -> "progress " <> displayBuilder (T.pack (show n))
    ManifestWriting -> "writing the manifest"
    MhlWritten p -> "manifest " <> displayBuilder (pathText p)
    OriginalsResolved origin algo -> "originals " <> displayBuilder origin <> " · " <> displayBuilder algo
    LogOpened p -> "log " <> displayBuilder (pathText p)
    JobFinished AllOk -> "finished, all ok"
    JobFinished (WithFailures n) -> "finished, " <> displayBuilder (T.pack (show n)) <> " failures"
    JobFailed message -> "failed – " <> displayBuilder message

-- | The seal found faults and the operator asked the job to stop. This is an outcome of the media
-- source, not a fault of this code, so it is no 'PlanViolation'.
data SealStopped = SealStopped
  { failed :: Int
  , total :: Int
  }
  deriving stock (Eq, Show)

-- | The one sentence the operator reads for a seal stop.
instance Display SealStopped where
  displayBuilder stopped =
    "the seal failed for "
      <> displayBuilder (T.pack (show stopped.failed))
      <> " of "
      <> displayBuilder (T.pack (show stopped.total))
      <> " files; nothing was copied"

data JobPhase = Queued | Running | NeedsReview | Finished JobResult | Failed Text | Cancelled
  deriving stock (Eq, Show)

data FileEntry = FileEntry
  { size :: FileSize
  , status :: FileStatus
  }
  deriving stock (Eq, Show)

data JobState = JobState
  { spec :: JobSpec
  , phase :: JobPhase
  , files :: Map RelPath FileEntry
  , bytesDone :: Int64
  , bytesTotal :: Int64
  , mhlPaths :: Vector OsPath
  , revision :: Int
  -- ^ Counts the events that change 'files', so a renderer can skip an unchanged file list.
  , originsUsed :: Maybe Text
  , originsAlgo :: Maybe HashAlgo
  -- ^ The format the job hashed in. Only a job that resolves originals sets it.
  , logPath :: Maybe OsPath
  -- ^ Where the runtime writes this job's event log. Nothing when no log could open.
  , lastMovedAt :: UTCTime
  -- ^ When the job last gave any sign of movement. The interface ages this to tell a slow phase
  -- from a stopped one, because the two phases that block on the disk report no byte. 'foldEvent'
  -- decides what counts as movement.
  , doing :: Maybe Doing
  -- ^ The last thing the job reported about itself. Nothing before the first report, and Nothing
  -- again once the job ends. So a job that stopped can never read as one still at work.
  }
  deriving stock (Eq, Generic, Show)

newJobState :: JobSpec -> JobState
newJobState spec =
  JobState
    { spec
    , phase = Queued
    , files = Map.empty
    , bytesDone = 0
    , bytesTotal = 0
    , mhlPaths = V.empty
    , revision = 0
    , originsUsed = Nothing
    , originsAlgo = Nothing
    , logPath = Nothing
    , -- A queued job has moved nothing yet, so it is as old as its own spec.
      lastMovedAt = spec.createdAt
    , doing = Nothing
    }

-- | A job without a plan has no total, so it reads as no progress and not as complete. A fraction
-- never passes 1, but a mismatch makes the engine read bytes the plan could not predict, so
-- 'bytesDone' can pass 'bytesTotal'. The bar and the percentage then hold at the top while the
-- rewrite finishes, and the byte pair beside them keeps the true figure.
fractionOf :: JobState -> Double
fractionOf state
  | state.bytesTotal <= 0 = 0
  | otherwise = min 1 (fromIntegral state.bytesDone / fromIntegral state.bytesTotal)

-- | The time is the moment the interface received the event. Four events carry it into
-- 'lastMovedAt': the plan that starts the job, a file state, a byte count, and the start of the
-- manifest write. Other events prove movement too, such as a manifest reaching the disk, but each
-- lands at the end of a phase, so stamping it would reset the age after the wait it should have
-- measured.
foldEvent :: UTCTime -> JobEvent -> JobState -> JobState
foldEvent at ev st = case ev of
  Planned (PlannedWork fs toRead) ->
    st
      { files = fs & V.map (\(p, s) -> (p, FileEntry s Pending)) & V.toList & Map.fromList
      , bytesTotal = toRead
      , phase = Running
      , revision = st.revision + 1
      , -- The job becomes 'Running' here, so the age counts from this moment and not from the spec,
        -- which a job that waited its turn in the queue made long before.
        lastMovedAt = at
      }
  FileStatusChanged p s ->
    st
      { -- A status for a path the plan never named keeps a zero size, because no plan ever gave it one.
        files = Map.insertWith (\_fresh existing -> existing {status = s}) p (FileEntry 0 s) st.files
      , revision = st.revision + 1
      , lastMovedAt = at
      , doing = Just (OnFile s)
      }
  Progress d -> st {bytesDone = max st.bytesDone d, lastMovedAt = at}
  -- This is the one phase that names no file. It says so, rather than leave the last file's
  -- state standing while the disk works on something else.
  ManifestWriting -> st {doing = Just WritingManifest, lastMovedAt = at}
  MhlWritten p -> st {mhlPaths = V.snoc st.mhlPaths p}
  OriginalsResolved origin algo -> st {originsUsed = Just origin, originsAlgo = Just algo}
  LogOpened p -> st {logPath = Just p}
  -- An end clears what the job had in hand. Without this the last phase stands for ever
  -- and any reader that forgets to check 'phase' ages a job that stopped.
  JobFinished r -> st {phase = Finished r, doing = Nothing}
  JobFailed msg -> st {phase = Failed msg, doing = Nothing}

-- | A missing file is a failure of the job, even though the job read nothing to fail.
outcomeFailed :: FileOutcome -> Bool
outcomeFailed = \case
  (Ok; New; Replaced _) -> False
  (HashMismatch _; IoError _; Missing) -> True

isFailure :: FileStatus -> Bool
isFailure = \case
  Done outcome -> outcomeFailed outcome
  _ -> False

isDone :: FileStatus -> Bool
isDone = \case
  Done _ -> True
  _ -> False

-- | Whether a job has stopped for good, whatever its outcome.
isTerminal :: JobPhase -> Bool
isTerminal = \case
  (Finished _; Failed _; Cancelled) -> True
  _ -> False

-- | The last event of a job, whatever its outcome. Nothing follows it.
isTerminalEvent :: JobEvent -> Bool
isTerminalEvent = \case
  (JobFinished _; JobFailed _) -> True
  _ -> False

data Counts = Counts
  { verified :: Int
  , failed :: Int
  , missing :: Int
  , new :: Int
  , replaced :: Int
  }
  deriving stock (Eq, Show)

countOutcomes :: JobState -> Counts
countOutcomes st = foldr step (Counts 0 0 0 0 0) (Map.elems st.files)
  where
    step :: FileEntry -> Counts -> Counts
    -- A file still on its way counts in no column.
    step fe c = case fe.status of
      (Pending; Hashing; Copying; Flushing; Publishing; Verifying) -> c
      Done Ok -> c {verified = c.verified + 1}
      -- 'SealStopped' carries a 'failed' too, so an update of this one field alone is ambiguous.
      (Done (HashMismatch _); Done (IoError _)) -> Counts {verified = c.verified, failed = c.failed + 1, missing = c.missing, new = c.new, replaced = c.replaced}
      Done Missing -> c {missing = c.missing + 1}
      Done New -> c {new = c.new + 1}
      Done (Replaced _) -> c {replaced = c.replaced + 1}

destinationPath :: OsPath -> OsPath -> OsPath
destinationPath parent source = parent </> takeFileName source
