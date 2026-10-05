module MediaCopy.Gtk.Widgets.PluginStatus
  ( PluginStatus (..)
  , newPluginStatus
  ) where

import Ascmhl.Path (pathText)
import Data.GI.Base (AttrOp (On, (:=)), new, set)
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
import MediaCopy.Model (UiMessage (..))

data PluginStatus = PluginStatus
  { group :: Adw.PreferencesGroup
  , cell :: Cell (Wording, PluginState)
  }

newPluginStatus :: (UiMessage -> IO ()) -> IO PluginStatus
newPluginStatus dispatch = do
  group <- new Adw.PreferencesGroup [#title := "Plug-ins", #visible := False]
  rows <- newIORef []
  cell <- newCell (renderStatus group rows dispatch)
  pure PluginStatus {group, cell}

renderStatus :: Adw.PreferencesGroup -> IORef [Adw.ActionRow] -> (UiMessage -> IO ()) -> (Wording, PluginState) -> IO ()
renderStatus group rowsRef dispatch (wording, st) = do
  readIORef rowsRef >>= mapM_ (Adw.preferencesGroupRemove group)
  inspecting <-
    if st.inspectionsLeft > 0
      then pure <$> row ("Inspecting: " <> plural "file" st.inspectionsLeft <> " left") "" []
      else pure []
  warnings <- traverse (\finding -> let (title, detail) = pluginFindingTexts wording finding in row title detail ["warning"]) (V.toList st.warnings)
  skipped <- traverse (\(ref, left) -> row (ref.name <> ": " <> plural "file" left <> " not inspected") "" ["warning"]) (Map.toList st.notInspected)
  produced <- traverse artifactRow (V.toList st.artifacts)
  delivered <- traverse deliveryRow (V.toList st.deliveries)
  let rows = inspecting <> warnings <> skipped <> produced <> delivered
  mapM_ (Adw.preferencesGroupAdd group) rows
  writeIORef rowsRef rows
  Gtk.widgetSetVisible group (not (null rows))
  where
    row :: Text -> Text -> [Text] -> IO Adw.ActionRow
    row title subtitle classes = do
      built <- new Adw.ActionRow [#useMarkup := False]
      set built [#title := title, #subtitle := subtitle]
      mapM_ (Gtk.widgetAddCssClass built) classes
      pure built
    artifactRow artifact = do
      built <- row (artifact.plugin.name <> ": " <> artifact.label) (pathText artifact.path) []
      open <- new Gtk.Button [#label := "Open", #valign := Gtk.AlignCenter, On #clicked (dispatch (OpenArtifact artifact.path))]
      Gtk.widgetAddCssClass open "flat"
      Adw.actionRowAddSuffix built open
      pure built
    deliveryRow delivery =
      row
        (delivery.plugin.name <> ": " <> (if delivery.delivered then "delivered" else "not delivered"))
        (delivery.target <> (if delivery.detail == "" then "" else " · " <> delivery.detail))
        ["error" | not delivery.delivered]
