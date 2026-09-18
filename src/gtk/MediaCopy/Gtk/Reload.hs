-- | The application stylesheet: loaded at start-up, and watched for live reload
-- in a development run (@MC3K_ENV=dev@).
module MediaCopy.Gtk.Reload
  ( Environment (..)
  , readEnvironment
  , loadCss
  ) where

import Control.Exception (catch)
import Control.Monad (void, when)
import Data.Functor ((<&>))
import Data.GI.Base (GError, disownObject, gerrorMessage, on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Text qualified as T
import Data.Word (Word32)
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import MediaCopy.Gtk.Assets (resolveAsset)

data Environment = Production | Development
  deriving stock (Eq, Show)

-- | @MC3K_ENV=dev@ turns on the development helpers.
-- The runtime reads it once, before GTK starts.
readEnvironment :: IO Environment
readEnvironment =
  lookupEnv "MC3K_ENV" <&> \case
    Just "dev" -> Development
    _ -> Production

-- | Loads the stylesheet for the display. A development run watches the file as well.
loadCss :: Environment -> IO ()
loadCss environment = do
  provider <- Gtk.cssProviderNew
  path <- resolveAsset "assets/styles.css"
  Gtk.cssProviderLoadFromPath provider path
  when (environment == Development) (watchCss provider path)
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

watchCss :: Gtk.CssProvider -> FilePath -> IO ()
watchCss provider path =
  startWatch `catch` \(err :: GError) -> do
    message <- gerrorMessage err
    logDev ("CSS live-reload disabled: " <> T.unpack message)
  where
    startWatch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logDev ("watching " <> path)
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
    logDev ("reloaded " <> path)
    pure False
  writeIORef pendingReload (Just sourceId)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]

logDev :: String -> IO ()
logDev message = hPutStrLn stderr ("mediacopy3000: " <> message)
