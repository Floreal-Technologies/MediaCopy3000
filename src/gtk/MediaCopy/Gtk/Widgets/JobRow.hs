module MediaCopy.Gtk.Widgets.JobRow
  ( JobRow (..)
  , newJobRow
  , jobIdOfRow
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Translation
import MediaCopy.Interface.View.JobRow (JobRowView (..), jobRowView)
import MediaCopy.Interface.Wording

rowName :: JobId -> Text
rowName (JobId n) = "job-" <> T.show n

jobIdOfRow :: Gtk.ListBoxRow -> IO (Maybe JobId)
jobIdOfRow listRow = do
  name <- Gtk.widgetGetName listRow
  pure (T.stripPrefix "job-" name >>= \digits -> readJobId digits)

readJobId :: Text -> Maybe JobId
readJobId digits = case TR.decimal digits of
  Right (n, rest) | T.null rest -> Just (JobId n)
  _ -> Nothing

data JobRow = JobRow
  { row :: Gtk.ListBoxRow
  , update :: Wording -> UTCTime -> JobState -> IO ()
  }

newJobRow :: Wording -> JobState -> IO JobRow
newJobRow wording state = do
  icon <- new Gtk.Image [#valign := Gtk.AlignStart]
  nameAccessible icon (jobKindText wording (jobKind state.spec.job))
  name <- newLabel (jobLabel state.spec.job) [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["heading"]
  sub <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["caption"]
  bar <- new Gtk.ProgressBar []
  nameAccessible bar (jobLabel state.spec.job <> " progress")
  lines' <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
  Gtk.boxAppend lines' name
  Gtk.boxAppend lines' sub
  Gtk.boxAppend lines' bar
  body <- paddedBox Gtk.OrientationHorizontal 10 6
  Gtk.boxAppend body icon
  Gtk.boxAppend body lines'
  row <- new Gtk.ListBoxRow [#child := body, #name := rowName state.spec.jobId]
  cell <- newCell $ \view -> do
    set icon [#iconName := view.icon]
    set name [#label := view.label]
    set sub [#label := view.phase]
    Gtk.progressBarSetFraction bar view.fraction
    toggleClass sub "success" view.allOk
    toggleClass bar "success" view.allOk
    toggleClass sub "error" view.bad
    toggleClass bar "error" view.bad
  let update wording' now current = renderCell cell (jobRowView wording' now current)
  update wording state.lastMovedAt state
  pure JobRow {row, update}
