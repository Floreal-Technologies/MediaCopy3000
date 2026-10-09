module MediaCopy.Gtk.Reload
  ( loadCss
  , loadWording
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad (forM_, unless, void, when)
import Data.ByteString qualified as ByteString
import Data.GI.Base (GError, disownObject, gerrorMessage, on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word32)
import Effectful.Log (logAttention_, logInfo_)
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Directory (doesFileExist)
import System.OsPath qualified as OsPath

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment (..), Mode (..), logWith)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Coverage

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

loadWording :: Environment -> SupportedLanguage -> (Wording -> IO ()) -> (Text -> IO ()) -> IO ()
loadWording environment language publish warn =
  when (environment.mode == Development) $ do
    wordingPath <- OsPath.decodeUtf (localeFile language)
    doesFileExist wordingPath >>= \case
      False -> logWith environment (logAttention_ ("wording live-reload disabled: no " <> T.pack wordingPath))
      True -> do
        revision <- newIORef 0
        streak <- newIORef False
        let reload = reloadWording environment language publish warn revision streak
        reload
        watchFile environment "wording" wordingPath reload

reloadWording :: Environment -> SupportedLanguage -> (Wording -> IO ()) -> (Text -> IO ()) -> IORef Word -> IORef Bool -> IO ()
reloadWording environment language publish warn revision streak = do
  wordingPath <- OsPath.decodeUtf (localeFile language)
  next <- (+ 1) <$> readIORef revision
  try @IOException (ByteString.readFile wordingPath) >>= \case
    Left err -> fault wordingPath [T.show err]
    Right bytes -> case decodeUtf8' bytes of
      Left err -> fault wordingPath [T.show err]
      Right source -> case parseWording language next source of
        Left junk -> fault wordingPath (map (\entry -> "cannot parse: " <> T.strip entry) (NE.toList junk))
        Right wording ->
          wordingFaults wording >>= \case
            [] -> do
              writeIORef revision next
              writeIORef streak False
              publish wording
              logWith environment (logInfo_ ("reloaded " <> T.pack wordingPath <> " (revision " <> T.show next <> ")"))
            faults -> fault wordingPath faults
  where
    fault :: FilePath -> List Text -> IO ()
    fault wordingPath faults = do
      forM_ faults (\message -> logWith environment (logAttention_ (T.pack wordingPath <> ": " <> message)))
      shown <- readIORef streak
      unless shown (warn (summary wordingPath faults))
      writeIORef streak True
    summary wordingPath faults = case faults of
      [] -> T.pack wordingPath
      first' : _ -> T.pack wordingPath <> ": " <> T.show (length faults) <> " faults; first: " <> first'

watchFile :: Environment -> Text -> FilePath -> IO () -> IO ()
watchFile environment label path action =
  startWatch `catch` \(err :: GError) -> do
    message <- gerrorMessage err
    logWith environment (logAttention_ (label <> " live-reload disabled: " <> message))
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
  forM_ pending GLib.sourceRemove
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
