module MediaCopy.Gtk.Theme
  ( ThemeAdapter (..)
  , newThemeAdapter
  , apply
  , readDesktopBase
  , onDesktopBase
  , readDesktopAccent
  , onDesktopAccent
  ) where

import Control.Monad.Extra
import Data.GI.Base (AttrOp ((:=)), on, set)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Effectful.Exception (tryIO)
import Effectful.Log (Log, logAttention_)
import Floreal.Commons.Environment qualified as Env
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import System.OsPath qualified as OsPath
import Prelude hiding (display)

import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment (..), Mode, runIn)
import MediaCopy.Interface.Theme

data ThemeAdapter = ThemeAdapter
  { provider :: Gtk.CssProvider
  , manager :: Adw.StyleManager
  , darkSheet :: Text
  , forced :: IORef (Maybe PaletteMode)
  , handler :: IORef (Maybe (PaletteMode -> IO ()))
  }

newThemeAdapter :: Environment -> IO ThemeAdapter
newThemeAdapter environment = do
  provider <- Gtk.cssProviderNew
  whenJustM Gdk.displayGetDefault $ \display ->
    Gtk.styleContextAddProviderForDisplay
      display
      provider
      (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION + 1)
  manager <- Adw.styleManagerGetDefault
  darkSheet <- runIn environment (readDarkSheet environment.mode)
  forced <- newIORef Nothing
  handler <- newIORef Nothing
  let adapter = ThemeAdapter {provider, manager, darkSheet, forced, handler}
  void $ on manager (Adw.PropertyNotify #dark) $ \_ ->
    readIORef forced >>= \case
      Just _ -> pure ()
      Nothing ->
        readDesktopBase adapter
          >>= \observed -> report adapter observed
  pure adapter

readDarkSheet :: (Env.Environment :> es, FileSystem :> es, Log :> es, IOE :> es) => Mode -> Eff es Text
readDarkSheet mode = do
  path <- resolveAsset mode "assets/dark.css"
  osPath <- liftIO (OsPath.encodeUtf path)
  tryIO (FileSystem.readBytes osPath) >>= \case
    Left reason -> unreadable path (show reason)
    Right bytes -> either (unreadable path . show) pure (decodeUtf8' bytes)
  where
    unreadable path reason = do
      logAttention_ (T.pack (path <> ": " <> reason))
      pure ""

apply :: ThemeAdapter -> Appearance -> PaletteMode -> Accent -> IO ()
apply adapter appearance desktop desktopAccent = do
  let mode = resolveMode appearance.base desktop
      sheet = case mode of
        DarkPalette -> adapter.darkSheet
        LightPalette -> ""
  Gtk.cssProviderLoadFromString
    adapter.provider
    (sheet <> accentCss mode (fromMaybe desktopAccent appearance.accent) desktopAccent)
  previous <- readIORef adapter.forced
  let wanted = forcedBase appearance.base
  writeIORef adapter.forced wanted
  set
    adapter.manager
    [ #colorScheme := case wanted of
        Nothing -> Adw.ColorSchemeDefault
        Just LightPalette -> Adw.ColorSchemeForceLight
        Just DarkPalette -> Adw.ColorSchemeForceDark
    ]
  case (previous, wanted) of
    (Just _, Nothing) -> do
      observed <- readDesktopBase adapter
      when (observed /= desktop) (report adapter observed)
    _ -> pure ()

readDesktopBase :: ThemeAdapter -> IO PaletteMode
readDesktopBase adapter = Adw.styleManagerGetDark adapter.manager <&> \dark -> if dark then DarkPalette else LightPalette

onDesktopBase :: ThemeAdapter -> (PaletteMode -> IO ()) -> IO ()
onDesktopBase adapter notify = do
  writeIORef adapter.handler (Just notify)
  readIORef adapter.forced >>= \case
    Just _ -> pure ()
    Nothing -> readDesktopBase adapter >>= \observed -> notify observed

report :: ThemeAdapter -> PaletteMode -> IO ()
report adapter observed =
  readIORef adapter.handler >>= mapM_ (\notify -> notify observed)

readDesktopAccent :: ThemeAdapter -> IO Accent
readDesktopAccent adapter =
  Adw.styleManagerGetAccentColor adapter.manager <&> \case
    Adw.AccentColorBlue -> Blue
    Adw.AccentColorTeal -> Teal
    Adw.AccentColorGreen -> Green
    Adw.AccentColorYellow -> Yellow
    Adw.AccentColorOrange -> Orange
    Adw.AccentColorRed -> Red
    Adw.AccentColorPink -> Pink
    Adw.AccentColorPurple -> Purple
    Adw.AccentColorSlate -> Slate
    Adw.AnotherAccentColor _ -> Blue

onDesktopAccent :: ThemeAdapter -> (Accent -> IO ()) -> IO ()
onDesktopAccent adapter notify =
  void $
    on adapter.manager (Adw.PropertyNotify #accentColor) $ \_ ->
      readDesktopAccent adapter >>= notify
