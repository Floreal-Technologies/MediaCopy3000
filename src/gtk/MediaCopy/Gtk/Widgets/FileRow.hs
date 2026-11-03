module MediaCopy.Gtk.Widgets.FileRow
  ( FileRow (..)
  , newFileRow
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Floreal.Commons.Gtk.Common (newCell, newLabel, renderCell, toggleClass)
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Interface.View.FileRow (FileRowView (..))

data FileRow = FileRow
  { row :: Gtk.Box
  , update :: FileRowView -> IO ()
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
  cell <- newCell $ \view -> do
    set name [#label := view.name]
    set size [#label := view.size]
    set status [#label := view.status]
    set dot [#iconName := view.icon]
    toggleClass row "error" view.failed
    toggleClass status "success" view.verified
  pure FileRow {row, update = renderCell cell}
