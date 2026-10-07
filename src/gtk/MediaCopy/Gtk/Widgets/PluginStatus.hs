module MediaCopy.Gtk.Widgets.PluginStatus
  ( PluginStatus (..)
  , newPluginStatus
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (plural)
import MediaCopy.Domain.Plugin
import MediaCopy.Gtk.Widgets.Common (Cell, newCell)
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Interface.Wording (pluginFindingTexts)

data PluginStatus = PluginStatus
  { group :: Adw.PreferencesGroup
  , cell :: Cell (Wording, PluginState)
  }

newPluginStatus :: IO PluginStatus
newPluginStatus = do
  group <- new Adw.PreferencesGroup [#title := "Plug-ins", #visible := False]
  rows <- newIORef []
  cell <- newCell (renderStatus group rows)
  pure PluginStatus {group, cell}

renderStatus :: Adw.PreferencesGroup -> IORef [Adw.ActionRow] -> (Wording, PluginState) -> IO ()
renderStatus group rowsRef (wording, st) = do
  readIORef rowsRef >>= mapM_ (Adw.preferencesGroupRemove group)
  inspecting <-
    if st.inspectionsLeft > 0
      then pure <$> row ("Inspecting: " <> plural "file" st.inspectionsLeft <> " left") "" []
      else pure []
  warnings <- traverse (\finding -> let (title, detail) = pluginFindingTexts wording finding in row title detail ["warning"]) (V.toList st.warnings)
  skipped <- traverse (\(ref, left) -> row (ref.name <> ": " <> plural "file" left <> " not inspected") "" ["warning"]) (Map.toList st.notInspected)
  let rows = inspecting <> warnings <> skipped
  forM_ rows (Adw.preferencesGroupAdd group)
  writeIORef rowsRef rows
  Gtk.widgetSetVisible group (not (null rows))
  where
    row :: Text -> Text -> [Text] -> IO Adw.ActionRow
    row title subtitle classes = do
      built <- new Adw.ActionRow [#useMarkup := False]
      set built [#title := title, #subtitle := subtitle]
      forM_ classes (Gtk.widgetAddCssClass built)
      pure built
