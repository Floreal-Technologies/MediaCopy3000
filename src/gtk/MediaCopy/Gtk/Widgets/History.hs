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
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (plural)
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording

data HistoryView = HistoryView
  { root :: Gtk.ListBox
  , cell :: Cell (Wording, Maybe MhlHistory)
  }

newHistoryView :: IO HistoryView
newHistoryView = do
  expander <- new Adw.ExpanderRow [#title := "History"]
  root <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  Gtk.widgetAddCssClass root "boxed-list"
  Gtk.listBoxAppend root expander
  rows <- newIORef V.empty
  cell <- newCell (\(wording, history) -> renderGenerations expander rows wording history)
  pure HistoryView {root, cell}

renderHistory :: HistoryView -> Wording -> Maybe MhlHistory -> IO ()
renderHistory view wording history = do
  renderCell view.cell (wording, history)
  Gtk.widgetSetVisible view.root (isJust history)

renderGenerations :: Adw.ExpanderRow -> IORef (Vector Adw.ActionRow) -> Wording -> Maybe MhlHistory -> IO ()
renderGenerations expander rows wording history = do
  let generations = maybe V.empty (.generations) history
  set expander [#subtitle := plural "generation" (V.length generations)]
  renderActionRows rows (InExpander expander) (V.map (generationRow wording) generations)

generationRow :: Wording -> Generation -> Row
generationRow wording generation =
  (plainRow (generationTitle generation) (generationSubtitle wording generation))
    { cssClass = if generation.failures > 0 then Just "error" else Nothing
    }

generationTitle :: Generation -> Text
generationTitle generation =
  T.justifyRight 4 '0' (count generation.number)
    <> " · "
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" generation.creator.creationDate)

generationSubtitle :: Wording -> Generation -> Text
generationSubtitle wording generation =
  generation.creator.hostname
    <> " — "
    <> generation.creator.toolName
    <> maybe "" (" " <>) generation.creator.toolVersion
    <> " · "
    <> algosText generation.algos
    <> " · "
    <> processKindText wording generation.process
    <> failuresSuffix generation.failures

failuresSuffix :: Int -> Text
failuresSuffix failures
  | failures > 0 = " · " <> plural "failure" failures
  | otherwise = ""
