module MediaCopy.Domain.JobState
  ( JobState (..)
  , Phase (..)
  , FileEntry (..)
  , Counts (..)
  , Doing (..)
  , new
  , apply
  , isTerminal
  , counts
  , fraction
  , rate
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.RelPath (RelPath)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobResult (JobResult)
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.PluginState (PluginState)
import MediaCopy.Domain.PluginState qualified as PluginState

data Doing
  = OnFile FileStatus
  | WritingManifest
  deriving stock (Eq, Show)

instance Display Doing where
  displayBuilder = \case
    OnFile status -> displayBuilder status
    WritingManifest -> "Writing the manifest"

data Phase = Queued | Running | NeedsReview | Finished JobResult | Failed Text | Cancelled
  deriving stock (Eq, Show)

data FileEntry = FileEntry
  { size :: FileSize
  , status :: FileStatus
  }
  deriving stock (Eq, Show)

data JobState = JobState
  { spec :: JobSpec
  , phase :: Phase
  , files :: Map RelPath FileEntry
  , bytesDone :: Int64
  , bytesTotal :: Int64
  , mhlPaths :: Vector OsPath
  , revision :: Int
  , originsUsed :: Maybe Text
  , originsAlgo :: Maybe Algo
  , logPath :: Maybe OsPath
  , lastMovedAt :: UTCTime
  , doing :: Maybe Doing
  , throughput :: Maybe Throughput
  , plugins :: PluginState
  }
  deriving stock (Eq, Generic, Show)

data Throughput = Throughput
  { at :: UTCTime
  , bytes :: Int64
  , rate :: Double
  }
  deriving stock (Eq, Show)

new :: JobSpec -> JobState
new spec =
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
    , plugins = PluginState.empty
    }

rate :: JobState -> Double
rate state = case (state.phase, state.throughput) of
  (Running, Just sample) -> sample.rate
  _ -> 0

fraction :: JobState -> Double
fraction state
  | state.bytesTotal <= 0 = 0
  | otherwise = min 1 (fromIntegral state.bytesDone / fromIntegral state.bytesTotal)

apply :: UTCTime -> JobEvent -> JobState -> JobState
apply at ev st = case ev of
  JobEvent.Planned (JobEvent.PlannedWork fs toRead) ->
    st
      { files = fs & V.map (\(p, s) -> (p, FileEntry s FileStatus.Pending)) & V.toList & Map.fromList
      , bytesTotal = toRead
      , phase = Running
      , revision = st.revision + 1
      , lastMovedAt = at
      }
  JobEvent.FileStatusChanged p s ->
    st
      { files = Map.insertWith (\_fresh existing -> existing {status = s}) p (FileEntry 0 s) st.files
      , revision = st.revision + 1
      , lastMovedAt = at
      , doing = Just (OnFile s)
      }
  JobEvent.Progress d ->
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
  JobEvent.ManifestWriting -> st {doing = Just WritingManifest, lastMovedAt = at}
  JobEvent.MhlWritten p -> st {mhlPaths = V.snoc st.mhlPaths p}
  JobEvent.OriginalsResolved origin algo -> st {originsUsed = Just origin, originsAlgo = Just algo}
  JobEvent.LogOpened p -> st {logPath = Just p}
  JobEvent.Finished r -> st {phase = Finished r, doing = Nothing}
  JobEvent.Failed msg -> st {phase = Failed msg, doing = Nothing}
  JobEvent.PluginReported report -> st {plugins = PluginState.apply report st.plugins}

isTerminal :: Phase -> Bool
isTerminal = \case
  (Finished _; Failed _; Cancelled) -> True
  _ -> False

data Counts = Counts
  { verified :: Int
  , failed :: Int
  , missing :: Int
  , new :: Int
  , replaced :: Int
  }
  deriving stock (Eq, Show)

counts :: JobState -> Counts
counts st = foldr step (Counts 0 0 0 0 0) (Map.elems st.files)
  where
    step :: FileEntry -> Counts -> Counts
    step fe c = case fe.status of
      (FileStatus.Pending; FileStatus.Hashing; FileStatus.Copying; FileStatus.Flushing; FileStatus.Publishing; FileStatus.Verifying) -> c
      FileStatus.Done FileOutcome.Ok -> c {verified = c.verified + 1}
      (FileStatus.Done (FileOutcome.HashMismatch _); FileStatus.Done (FileOutcome.IoError _)) -> Counts {verified = c.verified, failed = c.failed + 1, missing = c.missing, new = c.new, replaced = c.replaced}
      FileStatus.Done FileOutcome.Missing -> c {missing = c.missing + 1}
      FileStatus.Done FileOutcome.New -> c {new = c.new + 1}
      FileStatus.Done (FileOutcome.Replaced _) -> c {replaced = c.replaced + 1}
