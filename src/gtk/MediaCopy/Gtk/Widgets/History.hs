-- | The detail pane's MHL history expander: one row per manifest generation.
module MediaCopy.Gtk.Widgets.History
  ( HistoryView (..)
  , newHistoryView
  , renderHistory
  ) where

import Ascmhl.Types (CreatorInfo (..), Generation (..), MhlHistory (..), algosText)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (Cell, Row (..), RowHost (..), newCell, plainRow, renderActionRows, renderCell)
import MediaCopy.Interface.Wording (count, generationsText)

data HistoryView = HistoryView
  { root :: Gtk.ListBox
  , cell :: Cell (Maybe MhlHistory)
  }

newHistoryView :: IO HistoryView
newHistoryView = do
  expander <- new Adw.ExpanderRow [#title := "History"]
  root <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  Gtk.widgetAddCssClass root "boxed-list"
  Gtk.listBoxAppend root expander
  rows <- newIORef V.empty
  cell <- newCell (\history -> renderGenerations expander rows history)
  pure HistoryView {root, cell}

renderHistory :: HistoryView -> Maybe MhlHistory -> IO ()
renderHistory view history = do
  renderCell view.cell history
  Gtk.widgetSetVisible view.root (isJust history)

renderGenerations :: Adw.ExpanderRow -> IORef (Vector Adw.ActionRow) -> Maybe MhlHistory -> IO ()
renderGenerations expander rows history = do
  let generations = maybe V.empty (\loaded -> loaded.generations) history
  set expander [#subtitle := generationsText (V.length generations)]
  renderActionRows rows (InExpander expander) (V.map (\gen -> generationRow gen) generations)

generationRow :: Generation -> Row
generationRow generation =
  (plainRow (generationTitle generation) (generationSubtitle generation))
    { cssClass = if generation.failures > 0 then Just "error" else Nothing
    }

generationTitle :: Generation -> Text
generationTitle generation =
  T.justifyRight 4 '0' (count generation.number)
    <> " · "
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" generation.creator.creationDate)

generationSubtitle :: Generation -> Text
generationSubtitle generation =
  generation.creator.hostname
    <> " — "
    <> generation.creator.toolName
    <> maybe "" (\version -> " " <> version) generation.creator.toolVersion
    <> " · "
    <> algosText generation.algos
    <> " · "
    <> display generation.process
    <> failuresSuffix generation.failures

failuresSuffix :: Int -> Text
failuresSuffix failures
  | failures > 0 = " · " <> count failures <> " failures"
  | otherwise = ""
