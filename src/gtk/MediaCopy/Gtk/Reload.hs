module MediaCopy.Gtk.Reload
  ( loadCss
  ) where

import Control.Monad (void, when)
import Data.GI.Base (GError, disownObject, gerrorMessage)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Text qualified as T
import Data.Word (Word32)
import Effectful (Eff, liftIO)
import Effectful.Exception (catch, throwIO)
import Effectful.Log (logAttention_, logInfo_)
import Effectful.Reader.Static (ask)
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Eff (onE, timeoutE, withStreak)
import MediaCopy.Gtk.Environment (Environment (..), Mode (..), Ui, logFault)

loadCss :: (Ui es) => Eff es ()
loadCss = do
  environment <- ask @Environment
  provider <- Gtk.cssProviderNew
  path <- resolveAsset "assets/styles.css"
  Gtk.cssProviderLoadFromPath provider path
  when (environment.mode == Development) (watchCss provider path)
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

watchCss :: (Ui es) => Gtk.CssProvider -> FilePath -> Eff es ()
watchCss provider path =
  startWatch `catch` \(err :: GError) -> do
    message <- liftIO (gerrorMessage err)
    logAttention_ ("CSS live-reload disabled: " <> message)
  where
    startWatch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logInfo_ ("watching " <> T.pack path)
      pendingReload <- liftIO (newIORef Nothing)
      failing <- liftIO (newIORef False)
      onE monitor #changed $ \_file _otherFile eventType ->
        when (eventType `elem` reloadEvents) (scheduleReload provider path pendingReload failing)
      void (liftIO (disownObject monitor))

scheduleReload :: (Ui es) => Gtk.CssProvider -> FilePath -> IORef (Maybe Word32) -> IORef Bool -> Eff es ()
scheduleReload provider path pendingReload failing = do
  pending <- liftIO (readIORef pendingReload)
  mapM_ (\sourceId -> GLib.sourceRemove sourceId) pending
  sourceId <- timeoutE 200 $ do
    liftIO (writeIORef pendingReload Nothing)
    withStreak failing throwIO (void . logFault) reload
    pure False
  liftIO (writeIORef pendingReload (Just sourceId))
  where
    reload = do
      Gtk.cssProviderLoadFromPath provider path
      logInfo_ ("reloaded " <> T.pack path)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]
