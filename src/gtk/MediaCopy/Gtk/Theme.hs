module MediaCopy.Gtk.Theme
  ( ThemeAdapter (..)
  , newThemeAdapter
  , apply
  , readDesktopBase
  , onDesktopBase
  , readDesktopAccent
  , onDesktopAccent
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Extra
import Data.ByteString qualified as ByteString
import Data.Functor ((<&>))
import Data.GI.Base (AttrOp ((:=)), on, set)
import Data.GI.Base.Properties (getObjectPropertyInt32)
import Data.GI.Base.Signals (SignalProxy ((:::)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Effectful.Log (logAttention_)
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment, logWith)
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
  darkSheet <- readDarkSheet environment
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

readDarkSheet :: Environment -> IO Text
readDarkSheet environment = do
  path <- resolveAsset environment "assets/dark.css"
  try @IOException (ByteString.readFile path) >>= \case
    Left reason -> unreadable path (show reason)
    Right bytes -> either (unreadable path . show) pure (decodeUtf8' bytes)
  where
    unreadable path reason = do
      logWith environment (logAttention_ (T.pack (path <> ": " <> reason)))
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
  ifM
    hasAccentColor
    (getObjectPropertyInt32 adapter.manager "accent-color" <&> accentAt)
    (pure Blue)
  where
    accentAt index = fromMaybe Blue (lookup index (zip [0 ..] [minBound .. maxBound]))

onDesktopAccent :: ThemeAdapter -> (Accent -> IO ()) -> IO ()
onDesktopAccent adapter notify =
  whenM hasAccentColor $
    void $
      on adapter.manager (#notify ::: "accent-color") $ \_ ->
        readDesktopAccent adapter >>= notify

hasAccentColor :: IO Bool
hasAccentColor = do
  major <- Adw.getMajorVersion
  minor <- Adw.getMinorVersion
  pure ((major, minor) >= (1, 6))
