module MediaCopy.Gtk.Widgets.History
  ( HistoryView (..)
  , newHistoryView
  , renderHistory
  ) where

import Ascmhl.Types (MhlHistory (..))
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (plural)
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
  rows <- newIORef V.empty
  cell <- newCell $ \wanted -> do
    let (subtitle, generations) = fromMaybe (plural "generation" 0, V.empty) wanted
    set expander [#subtitle := subtitle]
    renderActionRows rows (InExpander expander) (V.map fromView generations)
  pure HistoryView {root, cell}

renderHistory :: HistoryView -> Wording -> Maybe MhlHistory -> IO ()
renderHistory view wording history = do
  renderCell view.cell (historyRows wording <$> history)
  Gtk.widgetSetVisible view.root (isJust history)
