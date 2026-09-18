module MediaCopy.Domain.JobEvent
  ( JobEvent (..)
  , PlannedWork (..)
  , isTerminal
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Data.Text qualified as T
import Data.Vector qualified as V

import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.JobResult (JobResult)
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.PluginReport (PluginReport)
import MediaCopy.Domain.Prose qualified as Prose

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
  | OriginalsResolved Text Algo
  | LogOpened OsPath
  | Finished JobResult
  | Failed Text
  | PluginReported PluginReport
  deriving stock (Eq, Show)

instance Display JobEvent where
  displayBuilder = \case
    Planned (PlannedWork fs toRead) ->
      "planned "
        <> displayBuilder (Prose.plural "file" (V.length fs))
        <> ", "
        <> displayBuilder (T.show toRead)
        <> " bytes to read"
    FileStatusChanged p s -> displayBuilder p <> " " <> displayBuilder s
    Progress n -> "progress " <> displayBuilder (T.show n)
    ManifestWriting -> "writing the manifest"
    MhlWritten p -> "manifest " <> displayBuilder (OsPath.toText p)
    OriginalsResolved origin algo -> "originals " <> displayBuilder origin <> " · " <> displayBuilder algo
    LogOpened p -> "log " <> displayBuilder (OsPath.toText p)
    Finished JobResult.AllOk -> "finished, all ok"
    Finished (JobResult.WithFailures n) -> "finished, " <> displayBuilder (Prose.plural "failure" n)
    Failed message -> "failed – " <> displayBuilder message
    PluginReported report -> displayBuilder report

isTerminal :: JobEvent -> Bool
isTerminal = \case
  (Finished _; Failed _) -> True
  _ -> False
