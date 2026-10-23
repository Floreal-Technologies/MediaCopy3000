module MediaCopy.Gtk.Reload
  ( loadCss
  , loadFonts
  , loadWording
  ) where

import Control.Exception (catch)
import Data.GI.Base (GError, disownObject, gerrorMessage)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Effectful.Exception (tryIO)
import Effectful.Log (logAttention_, logInfo_)
import Floreal.Commons.Exception (guardedLog)
import Floreal.Commons.GLib.MainLoop (debounced)
import Floreal.Commons.Gtk.Signal (onSelf)
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango
import System.Info (os)
import System.OsPath qualified as OsPath
import Prelude hiding (display)

import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment (..), Mode (..), logWith, runIn)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Coverage

loadCss :: Environment -> IO ()
loadCss environment = do
  provider <- Gtk.cssProviderNew
  path <- runIn environment (resolveAsset environment.mode "assets/styles.css")
  Gtk.cssProviderLoadFromPath provider path
  when (environment.mode == Development) $ do
    watchFile environment "CSS" path $ do
      Gtk.cssProviderLoadFromPath provider path
      logWith environment (logInfo_ ("reload " <> T.pack path))
  Gdk.displayGetDefault
    >>= mapM_ (\display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION))

loadFonts :: (Gtk.IsWidget widget) => Environment -> widget -> IO ()
loadFonts environment widget =
  unless (os == "darwin") $
    Gtk.widgetGetPangoContext widget >>= Pango.contextGetFontMap >>= mapM_ addFonts
  where
    addFonts fontMap =
      forM_ ["assets/fonts/InterVariable.ttf", "assets/fonts/InterVariable-Italic.ttf"] $ \relative -> do
        path <- runIn environment (resolveAsset environment.mode relative)
        Pango.fontMapAddFontFile fontMap path `catch` \(err :: GError) -> do
          message <- gerrorMessage err
          logWith environment (logAttention_ ("cannot add font " <> T.pack path <> ": " <> message))

loadWording :: Environment -> SupportedLanguage -> (Wording -> IO ()) -> (Text -> IO ()) -> IO ()
loadWording environment language publish warn =
  when (environment.mode == Development) $ do
    wordingPath <- OsPath.decodeUtf (localeFile language)
    runIn environment (FileSystem.doesFileExist (localeFile language)) >>= \case
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
  runIn environment (tryIO (FileSystem.readBytes (localeFile language))) >>= \case
    Left err -> fault wordingPath [T.show err]
    Right bytes -> case decodeUtf8' bytes of
      Left err -> fault wordingPath [T.show err]
      Right source -> case mkWording language next <$> parseFtl source of
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
      reload <- debounced 200
      onSelf monitor #changed $ \_file _otherFile eventType ->
        when (eventType `elem` reloadEvents) (reload (runIn environment (guardedLog (label <> " reload") (liftIO action))))
      void (disownObject monitor)

reloadEvents :: List Gio.FileMonitorEvent
reloadEvents =
  [ Gio.FileMonitorEventChangesDoneHint
  , Gio.FileMonitorEventRenamed
  , Gio.FileMonitorEventMovedIn
  , Gio.FileMonitorEventCreated
  ]
