-- | The application stylesheet: loaded at start-up, and watched for live reload
-- in a development run (@MC3K_ENV=dev@).
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
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment (Development))
import MediaCopy.Gtk.Log (logLine)

-- | Loads the stylesheet for the display. A development run watches the file as well.
loadCss :: Environment -> IO ()
loadCss environment = do
  provider <- Gtk.cssProviderNew
  path <- resolveAsset environment "assets/styles.css"
  Gtk.cssProviderLoadFromPath provider path
  when (environment == Development) (watchCss provider path)
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

watchCss :: Gtk.CssProvider -> FilePath -> IO ()
watchCss provider path =
  startWatch `catch` \(err :: GError) -> do
    message <- gerrorMessage err
    logLine ("CSS live-reload disabled: " <> T.unpack message)
  where
    startWatch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logLine ("watching " <> path)
      pendingReload <- newIORef Nothing
      on monitor #changed $ \_file _otherFile eventType ->
        when (eventType `elem` reloadEvents) (scheduleReload provider path pendingReload)
      -- The monitor must outlive this scope. Nothing else holds a reference to it.
      void (disownObject monitor)

-- | Debounce events with a 200ms delay
scheduleReload :: Gtk.CssProvider -> FilePath -> IORef (Maybe Word32) -> IO ()
scheduleReload provider path pendingReload = do
  pending <- readIORef pendingReload
  mapM_ (\sourceId -> GLib.sourceRemove sourceId) pending
  sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
    writeIORef pendingReload Nothing
    Gtk.cssProviderLoadFromPath provider path
    logLine ("reloaded " <> path)
    pure False
  writeIORef pendingReload (Just sourceId)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]
