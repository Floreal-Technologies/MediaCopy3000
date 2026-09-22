module MediaCopy.Gtk.Widgets.JobRow
  ( JobRow (..)
  , newJobRow
  , jobIdOfRow
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job (JobId (..), JobPhase (..), JobResult (..), JobSpec (..), JobState (..), fractionOf, jobKind, jobLabel, plural, rateOf)
import MediaCopy.Gtk.Widgets.Common (nameAccessible, newCell, newLabel, paddedBox, renderCell, toggleClass)
import MediaCopy.Interface.Wording (KindUi (..), count, humanRate, kindUi, quietText)

rowName :: JobId -> Text
rowName (JobId n) = "job-" <> T.pack (show n)

jobIdOfRow :: Gtk.ListBoxRow -> IO (Maybe JobId)
jobIdOfRow listRow = do
  name <- Gtk.widgetGetName listRow
  pure (T.stripPrefix "job-" name >>= \digits -> readJobId digits)

readJobId :: Text -> Maybe JobId
readJobId digits = case TR.decimal digits of
  Right (n, rest) | T.null rest -> Just (JobId n)
  _ -> Nothing

data RowView = RowView
  { icon :: Text
  , label :: Text
  , phase :: Text
  , fraction :: Double
  , allOk :: Bool
  , bad :: Bool
  }
  deriving stock (Eq)

data JobRow = JobRow
  { row :: Gtk.ListBoxRow
  , update :: UTCTime -> JobState -> IO ()
  }

newJobRow :: JobState -> IO JobRow
newJobRow state = do
  icon <- new Gtk.Image [#valign := Gtk.AlignStart]
  nameAccessible icon (display (jobKind state.spec.job))
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
  let update now current = renderCell cell (rowView now current)
  update state.lastMovedAt state
  pure JobRow {row, update}

rowView :: UTCTime -> JobState -> RowView
rowView now state =
  RowView
    { icon = (kindUi (jobKind state.spec.job)).icon
    , label = jobLabel state.spec.job
    , phase = phaseText now state
    , fraction = fractionOf state
    , allOk = state.phase == Finished AllOk
    , bad = case state.phase of
        (Finished (WithFailures _); Failed _) -> True
        _ -> False
    }

phaseText :: UTCTime -> JobState -> Text
phaseText now state = case state.phase of
  Queued -> "Queued"
  NeedsReview -> "Needs review"
  Running -> runningText now state
  Finished AllOk -> "Finished · " <> plural "file" (Map.size state.files) <> " · all OK"
  Finished (WithFailures failures) -> "Finished · " <> plural "failure" failures
  Failed _ -> "Failed"
  Cancelled -> "Cancelled"

runningText :: UTCTime -> JobState -> Text
runningText now state
  | Just quiet <- quietText now state = quiet
  | ui.showsProgress = ui.runningVerb <> " · " <> count (floor (fractionOf state * 100) :: Int) <> " % · " <> humanRate (rateOf state)
  | otherwise = ui.runningVerb <> "…"
  where
    ui = kindUi (jobKind state.spec.job)
