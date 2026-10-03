module MediaCopy.Gtk.Reload
  ( loadCss
  ) where

import Control.Exception (catch)
import Control.Monad (void, when)
import Data.GI.Base (GError, disownObject, gerrorMessage, on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
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
  when (environment.mode == Development) $ do
    watchFile environment "CSS" path $ do
      Gtk.cssProviderLoadFromPath provider path
      logWith environment (logInfo_ ("reload " <> T.pack path))
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

watchFile :: Environment -> Text -> FilePath -> IO () -> IO ()
watchFile environment label path action =
  startWatch `catch` \(err :: GError) -> do
    message <- gerrorMessage err
    logWith environment (logAttention_ (label <>  " live-reload disabled: " <> message))
  where
    startWatch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logWith environment (logInfo_ ("watching " <> T.pack path))
      pendingReload <- newIORef Nothing
      on monitor #changed $ \_file _otherFile eventType ->
        when (eventType `elem` reloadEvents) (scheduleReload action pendingReload)
      void (disownObject monitor)

scheduleReload :: IO () -> IORef (Maybe Word32) -> IO ()
scheduleReload action pendingReload = do
  pending <- readIORef pendingReload
  mapM_ (\sourceId -> GLib.sourceRemove sourceId) pending
  sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
    writeIORef pendingReload Nothing
    action
    pure False
  writeIORef pendingReload (Just sourceId)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]
