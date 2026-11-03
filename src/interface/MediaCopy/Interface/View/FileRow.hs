module MediaCopy.Interface.View.FileRow
  ( FileRowView (..)
  , fileRowView
  ) where

import Ascmhl.RelPath (RelPath)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording (fileStatusText, humanBytes)

-- $setup
-- >>> import Ascmhl.RelPath (RelPath (..))
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)

data FileRowView = FileRowView
  { name :: Text
  , size :: Text
  , status :: Text
  , icon :: Text
  , failed :: Bool
  , verified :: Bool
  }
  deriving stock (Eq, Show)

-- |
-- >>> fileRowView (embeddedWording English) (RelPath "A001/clip.mov") 1500000 (FileStatus.Done FileOutcome.Ok)
-- FileRowView {name = "A001/clip.mov", size = "1.5 MB", status = "Verified", icon = "emblem-ok-symbolic", failed = False, verified = True}
fileRowView :: Wording -> RelPath -> FileSize -> FileStatus -> FileRowView
fileRowView wording path fileSize fileStatus =
  FileRowView
    { name = display path
    , size = humanBytes wording fileSize
    , status = fileStatusText wording fileStatus
    , icon = statusIcon fileStatus
    , failed = FileStatus.isFailure fileStatus
    , verified = isVerified fileStatus
    }

statusIcon :: FileStatus -> Text
statusIcon = \case
  FileStatus.Pending -> "radio-symbolic"
  (FileStatus.Hashing; FileStatus.Copying; FileStatus.Flushing; FileStatus.Publishing; FileStatus.Verifying) -> "content-loading-symbolic"
  FileStatus.Done FileOutcome.Ok -> "emblem-ok-symbolic"
  FileStatus.Done FileOutcome.New -> "document-new-symbolic"
  (FileStatus.Done (FileOutcome.HashMismatch _); FileStatus.Done FileOutcome.Missing; FileStatus.Done (FileOutcome.IoError _)) -> "dialog-error-symbolic"
  FileStatus.Done (FileOutcome.Replaced _) -> "view-refresh-symbolic"

isVerified :: FileStatus -> Bool
isVerified = \case
  FileStatus.Done FileOutcome.Ok -> True
  _ -> False
