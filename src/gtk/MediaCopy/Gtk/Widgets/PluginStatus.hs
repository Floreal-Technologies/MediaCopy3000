module MediaCopy.Gtk.Widgets.PluginStatus
  ( PluginStatus (..)
  , newPluginStatus
  ) where

import Data.GI.Base (AttrOp ((:=)), new)
import Data.IORef (newIORef)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (Cell, RowHost (..), fromView, newCell, renderActionRows)
import MediaCopy.Interface.View.Row (RowView)

data PluginStatus = PluginStatus
  { group :: Adw.PreferencesGroup
  , cell :: Cell (Vector RowView)
  }

newPluginStatus :: IO PluginStatus
newPluginStatus = do
  group <- new Adw.PreferencesGroup [#title := "Plug-ins", #visible := False]
  rows <- newIORef V.empty
  cell <- newCell $ \wanted -> do
    renderActionRows rows (InGroup group) (V.map fromView wanted)
    Gtk.widgetSetVisible group (not (V.null wanted))
  pure PluginStatus {group, cell}
