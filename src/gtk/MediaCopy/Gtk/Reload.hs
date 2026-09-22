module MediaCopy.Gtk.Reload
  ( loadCss
  ) where

import Control.Exception (catch)
import Control.Monad (void, when)
import Data.GI.Base (GError, disownObject, gerrorMessage, on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Text qualified as T
import Data.Word (Word32)
import Effectful.Log (logAttention_, logInfo_)
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment (..), Mode (..), logWith)

loadCss :: Environment -> IO ()
loadCss environment = do
  provider <- Gtk.cssProviderNew
  path <- resolveAsset environment "assets/styles.css"
  Gtk.cssProviderLoadFromPath provider path
  when (environment.mode == Development) (watchCss environment provider path)
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

watchCss :: Environment -> Gtk.CssProvider -> FilePath -> IO ()
watchCss environment provider path =
  startWatch `catch` \(err :: GError) -> do
    message <- gerrorMessage err
    logWith environment (logAttention_ ("CSS live-reload disabled: " <> message))
  where
    startWatch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logWith environment (logInfo_ ("watching " <> T.pack path))
      pendingReload <- newIORef Nothing
      on monitor #changed $ \_file _otherFile eventType ->
        when (eventType `elem` reloadEvents) (scheduleReload environment provider path pendingReload)
      void (disownObject monitor)

scheduleReload :: Environment -> Gtk.CssProvider -> FilePath -> IORef (Maybe Word32) -> IO ()
scheduleReload environment provider path pendingReload = do
  pending <- readIORef pendingReload
  mapM_ (\sourceId -> GLib.sourceRemove sourceId) pending
  sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
    writeIORef pendingReload Nothing
    Gtk.cssProviderLoadFromPath provider path
    logWith environment (logInfo_ ("reloaded " <> T.pack path))
    pure False
  writeIORef pendingReload (Just sourceId)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]
