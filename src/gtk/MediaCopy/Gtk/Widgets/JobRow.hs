module MediaCopy.Gtk.Widgets.JobRow
  ( JobRow (..)
  , newJobRow
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.Time (UTCTime)
import Floreal.Commons.Gtk.Common (newCell, newLabel, paddedBox, renderCell, toggleClass)
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Translation
import MediaCopy.Interface.View.JobRow (JobRowView (..), jobRowView)
import MediaCopy.Interface.Wording

data JobRow = JobRow
  { row :: Gtk.Box
  , update :: Wording -> UTCTime -> JobState -> IO ()
  }

newJobRow :: IO JobRow
newJobRow = do
  icon <- new Gtk.Image [#valign := Gtk.AlignStart]
  name <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["heading"]
  sub <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["caption"]
  bar <- new Gtk.ProgressBar []
  lines' <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
  Gtk.boxAppend lines' name
  Gtk.boxAppend lines' sub
  Gtk.boxAppend lines' bar
  row <- paddedBox Gtk.OrientationHorizontal 10 6
  Gtk.boxAppend row icon
  Gtk.boxAppend row lines'
  names <- newCell $ \(kind, label) -> do
    nameAccessible icon kind
    nameAccessible bar (label <> " progress")
  cell <- newCell $ \view -> do
    set icon [#iconName := view.icon]
    set name [#label := view.label]
    set sub [#label := view.phase]
    Gtk.progressBarSetFraction bar view.fraction
    toggleClass sub "success" view.allOk
    toggleClass bar "success" view.allOk
    toggleClass sub "error" view.bad
    toggleClass bar "error" view.bad
  let update wording now current = do
        renderCell names (jobKindText wording (Job.kind current.spec.job), Job.label current.spec.job)
        renderCell cell (jobRowView wording now current)
  pure JobRow {row, update}
