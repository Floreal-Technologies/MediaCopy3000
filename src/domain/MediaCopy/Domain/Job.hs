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
  , Throughput (..)
  , rateOf
  , plural
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
import Data.Time (UTCTime, diffUTCTime)
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

data SealFirst
  = UseHistory
  | SealBeforeCopy OnSealFailure
  deriving stock (Eq, Show)

data OnSealFailure = StopBeforeCopy | CopyAnyway
  deriving stock (Eq, Show)

instance Display OnSealFailure where
  displayBuilder = \case
    StopBeforeCopy -> "stop before copying"
    CopyAnyway -> "copy anyway"

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

jobKind :: Job -> JobKind
jobKind = \case
  Offload _ -> OffloadKind
  VerifyFolder _ -> VerifyKind
  SealMediaSource _ -> SealKind

jobRoot :: Job -> OsPath
jobRoot = \case
  Offload oj -> oj.source
  VerifyFolder vj -> vj.folder
  SealMediaSource sj -> sj.folder

historyFolder :: Job -> Maybe OsPath
historyFolder = \case
  Offload _ -> Nothing
  VerifyFolder vj -> Just vj.folder
  SealMediaSource sj -> Just sj.folder

jobLabel :: Job -> Text
jobLabel job = pathText (takeFileName (jobRoot job))

originsDescription :: Map RelPath Hash -> Text
originsDescription resolved
  | Map.null resolved = "none found – every file recorded as original"
  | otherwise = "the media source's own history, " <> plural "file" (Map.size resolved)

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
  | Replaced Mismatch
  deriving stock (Eq, Show)

data FileStatus
  = Pending
  | Hashing
  | Copying
  | Flushing
  | Publishing
  | Verifying
  | Done FileOutcome
  deriving stock (Eq, Show)

data Doing
  = OnFile FileStatus
  | WritingManifest
  deriving stock (Eq, Show)

instance Display Doing where
  displayBuilder = \case
    OnFile status -> displayBuilder status
    WritingManifest -> "Writing the manifest"

data JobResult = AllOk | WithFailures Int
  deriving stock (Eq, Show)

data PlannedWork = PlannedWork
  { files :: Vector (RelPath, FileSize)
  , bytesToRead :: Int64
  }
  deriving stock (Eq, Show)

data JobEvent
  = Planned PlannedWork
  | FileStatusChanged RelPath FileStatus
  | Progress Int64
  | ManifestWriting
  | MhlWritten OsPath
  | OriginalsResolved Text HashAlgo
  | LogOpened OsPath
  | JobFinished JobResult
  | JobFailed Text
  deriving stock (Eq, Show)

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

instance Display JobEvent where
  displayBuilder = \case
    Planned (PlannedWork fs toRead) ->
      "planned "
        <> displayBuilder (plural "file" (V.length fs))
        <> ", "
        <> displayBuilder (T.pack (show toRead))
        <> " bytes to read"
    FileStatusChanged p s -> displayBuilder p <> " " <> displayBuilder s
    Progress n -> "progress " <> displayBuilder (T.pack (show n))
    ManifestWriting -> "writing the manifest"
    MhlWritten p -> "manifest " <> displayBuilder (pathText p)
    OriginalsResolved origin algo -> "originals " <> displayBuilder origin <> " · " <> displayBuilder algo
    LogOpened p -> "log " <> displayBuilder (pathText p)
    JobFinished AllOk -> "finished, all ok"
    JobFinished (WithFailures n) -> "finished, " <> displayBuilder (plural "failure" n)
    JobFailed message -> "failed – " <> displayBuilder message

data SealStopped = SealStopped
  { failed :: Int
  , total :: Int
  }
  deriving stock (Eq, Show)

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
  , originsUsed :: Maybe Text
  , originsAlgo :: Maybe HashAlgo
  , logPath :: Maybe OsPath
  , lastMovedAt :: UTCTime
  , doing :: Maybe Doing
  , throughput :: Maybe Throughput
  }
  deriving stock (Eq, Generic, Show)

data Throughput = Throughput
  { at :: UTCTime
  , bytes :: Int64
  , rate :: Double
  }
  deriving stock (Eq, Show)

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
    , lastMovedAt = spec.createdAt
    , doing = Nothing
    , throughput = Nothing
    }

rateOf :: JobState -> Double
rateOf state = case (state.phase, state.throughput) of
  (Running, Just sample) -> sample.rate
  _ -> 0

-- |
-- >>> plural "failure" 1
-- "1 failure"
-- >>> plural "file" 3
-- "3 files"
plural :: Text -> Int -> Text
plural noun 1 = "1 " <> noun
plural noun n = T.pack (show n) <> " " <> noun <> "s"

fractionOf :: JobState -> Double
fractionOf state
  | state.bytesTotal <= 0 = 0
  | otherwise = min 1 (fromIntegral state.bytesDone / fromIntegral state.bytesTotal)

foldEvent :: UTCTime -> JobEvent -> JobState -> JobState
foldEvent at ev st = case ev of
  Planned (PlannedWork fs toRead) ->
    st
      { files = fs & V.map (\(p, s) -> (p, FileEntry s Pending)) & V.toList & Map.fromList
      , bytesTotal = toRead
      , phase = Running
      , revision = st.revision + 1
      , lastMovedAt = at
      }
  FileStatusChanged p s ->
    st
      { files = Map.insertWith (\_fresh existing -> existing {status = s}) p (FileEntry 0 s) st.files
      , revision = st.revision + 1
      , lastMovedAt = at
      , doing = Just (OnFile s)
      }
  Progress d ->
    let bytes = max st.bytesDone d
        fresh = Throughput {at, bytes, rate = 0}
        sampled = case st.throughput of
          Nothing -> fresh
          Just previous
            | elapsed < 0.5 -> previous
            | otherwise -> fresh {rate = fromIntegral (bytes - previous.bytes) / elapsed}
            where
              elapsed = realToFrac (diffUTCTime at previous.at) :: Double
    in st {bytesDone = bytes, lastMovedAt = at, throughput = Just sampled}
  ManifestWriting -> st {doing = Just WritingManifest, lastMovedAt = at}
  MhlWritten p -> st {mhlPaths = V.snoc st.mhlPaths p}
  OriginalsResolved origin algo -> st {originsUsed = Just origin, originsAlgo = Just algo}
  LogOpened p -> st {logPath = Just p}
  JobFinished r -> st {phase = Finished r, doing = Nothing}
  JobFailed msg -> st {phase = Failed msg, doing = Nothing}

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

isTerminal :: JobPhase -> Bool
isTerminal = \case
  (Finished _; Failed _; Cancelled) -> True
  _ -> False

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
    step fe c = case fe.status of
      (Pending; Hashing; Copying; Flushing; Publishing; Verifying) -> c
      Done Ok -> c {verified = c.verified + 1}
      (Done (HashMismatch _); Done (IoError _)) -> Counts {verified = c.verified, failed = c.failed + 1, missing = c.missing, new = c.new, replaced = c.replaced}
      Done Missing -> c {missing = c.missing + 1}
      Done New -> c {new = c.new + 1}
      Done (Replaced _) -> c {replaced = c.replaced + 1}

destinationPath :: OsPath -> OsPath -> OsPath
destinationPath parent source = parent </> takeFileName source
