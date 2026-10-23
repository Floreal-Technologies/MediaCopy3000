module MediaCopy.Gtk.Widgets.FileRow
  ( FileRow (..)
  , newFileRow
  ) where

import Ascmhl.RelPath (RelPath)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Floreal.Commons.Gtk.Common (newCell, newLabel, renderCell, toggleClass)
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording (fileStatusText, humanBytes)

data FileRow = FileRow
  { row :: Gtk.Box
  , update :: Wording -> RelPath -> FileSize -> FileStatus -> IO ()
  }

newFileRow :: IO FileRow
newFileRow = do
  name <- newLabel "" [#xalign := 0, #hexpand := True, #ellipsize := Pango.EllipsizeModeEnd] []
  size <- newLabel "" [#xalign := 1, #widthChars := 10] ["dim-label"]
  dot <- new Gtk.Image [#accessibleRole := Gtk.AccessibleRolePresentation]
  status <-
    newLabel "" [#xalign := 0, #widthChars := 22, #maxWidthChars := 22, #ellipsize := Pango.EllipsizeModeEnd] ["status"]
  row <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 10
      , #marginTop := 6
      , #marginBottom := 6
      , #marginStart := 10
      , #marginEnd := 10
      ]
  Gtk.boxAppend row name
  Gtk.boxAppend row size
  Gtk.boxAppend row dot
  Gtk.boxAppend row status
  cell <- newCell $ \(wording, path, fileSize, fileStatus) -> do
    set name [#label := display path]
    set size [#label := humanBytes wording fileSize]
    set status [#label := fileStatusText wording fileStatus]
    set dot [#iconName := statusIcon fileStatus]
    toggleClass row "error" (FileStatus.isFailure fileStatus)
    toggleClass status "success" (isVerified fileStatus)
  let update wording path fileSize fileStatus = renderCell cell (wording, path, fileSize, fileStatus)
  pure FileRow {row, update}

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
