module MediaCopy.Gtk.Widgets.History
  ( HistoryView (..)
  , newHistoryView
  , renderHistory
  ) where

import Ascmhl.History (History)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Floreal.Commons.Gtk.Common (Cell, newCell, renderCell)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Translation
import MediaCopy.Interface.View.JobDetail (historyRows)
import MediaCopy.Interface.View.Row (RowView)

data HistoryView = HistoryView
  { root :: Gtk.ListBox
  , cell :: Cell (Maybe (Text, Vector RowView))
  }

newHistoryView :: IO HistoryView
newHistoryView = do
  expander <- new Adw.ExpanderRow [#title := "History"]
  root <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  Gtk.widgetAddCssClass root "boxed-list"
  Gtk.listBoxAppend root expander
  rows <- newRowsCell (InExpander expander)
  cell <- newCell $ \wanted -> do
    let (subtitle, generations) = fromMaybe (Prose.plural "generation" 0, mempty) wanted
    set expander [#subtitle := subtitle]
    renderCell rows generations
  pure HistoryView {root, cell}

renderHistory :: HistoryView -> Wording -> Maybe History -> IO ()
renderHistory view wording history = do
  renderCell view.cell (historyRows wording <$> history)
  Gtk.widgetSetVisible view.root (isJust history)
