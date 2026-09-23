module MediaCopy.Gtk.Widgets.FileRow
  ( FileRow (..)
  , newFileRow
  ) where

import Ascmhl.Path (RelPath)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.Text (Text)
import Data.Text.Display (display)
import Effectful (Eff, IOE, (:>))
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job (FileOutcome (..), FileSize, FileStatus (..), isFailure)
import MediaCopy.Gtk.Widgets.Common (newCell, newLabel, renderCell, toggleClass)
import MediaCopy.Interface.Wording (humanBytes)

data FileRow es = FileRow
  { row :: Gtk.ListBoxRow
  , update :: FileSize -> FileStatus -> Eff es ()
  }

newFileRow :: (IOE :> es) => RelPath -> Eff es (FileRow es)
newFileRow path = do
  name <- newLabel (display path) [#xalign := 0, #hexpand := True, #ellipsize := Pango.EllipsizeModeEnd] []
  size <- newLabel "" [#xalign := 1, #widthChars := 10] ["dim-label"]
  dot <- new Gtk.Image [#accessibleRole := Gtk.AccessibleRolePresentation]
  status <-
    newLabel "" [#xalign := 0, #widthChars := 22, #maxWidthChars := 22, #ellipsize := Pango.EllipsizeModeEnd] ["status"]
  body <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 10
      , #marginTop := 6
      , #marginBottom := 6
      , #marginStart := 10
      , #marginEnd := 10
      ]
  Gtk.boxAppend body name
  Gtk.boxAppend body size
  Gtk.boxAppend body dot
  Gtk.boxAppend body status
  row <- new Gtk.ListBoxRow [#child := body, #activatable := False]
  cell <- newCell $ \(fileSize, fileStatus) -> do
    set size [#label := humanBytes fileSize]
    set status [#label := display fileStatus]
    set dot [#iconName := statusIcon fileStatus]
    toggleClass row "error" (isFailure fileStatus)
    toggleClass status "success" (isVerified fileStatus)
  let update fileSize fileStatus = renderCell cell (fileSize, fileStatus)
  pure FileRow {row, update}

statusIcon :: FileStatus -> Text
statusIcon = \case
  Pending -> "radio-symbolic"
  (Hashing; Copying; Flushing; Publishing; Verifying) -> "content-loading-symbolic"
  Done Ok -> "emblem-ok-symbolic"
  Done New -> "document-new-symbolic"
  (Done (HashMismatch _); Done Missing; Done (IoError _)) -> "dialog-error-symbolic"
  Done (Replaced _) -> "view-refresh-symbolic"

isVerified :: FileStatus -> Bool
isVerified = \case
  Done Ok -> True
  _ -> False
