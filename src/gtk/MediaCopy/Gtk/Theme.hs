module MediaCopy.Gtk.Theme
  ( ThemeAdapter (..)
  , newThemeAdapter
  , apply
  , readDesktopBase
  , onDesktopBase
  , loadPalettes
  ) where

import Control.Monad.Extra
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List, find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Log (logAttention_)
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Eff (onE)
import MediaCopy.Gtk.Environment (Ui)
import MediaCopy.Interface.Theme

data ThemeAdapter es = ThemeAdapter
  { provider :: Gtk.CssProvider
  , manager :: Adw.StyleManager
  , forced :: IORef (Maybe PaletteMode)
  , handler :: IORef (Maybe (PaletteMode -> Eff es ()))
  }

newThemeAdapter :: (Ui es) => Eff es (ThemeAdapter es)
newThemeAdapter = do
  provider <- Gtk.cssProviderNew
  whenJustM Gdk.displayGetDefault $ \display ->
    Gtk.styleContextAddProviderForDisplay
      display
      provider
      (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION + 1)
  manager <- Adw.styleManagerGetDefault
  forced <- liftIO (newIORef Nothing)
  handler <- liftIO (newIORef Nothing)
  let adapter = ThemeAdapter {provider, manager, forced, handler}
  void $ onE manager (Adw.PropertyNotify #dark) $ \_ ->
    liftIO (readIORef forced) >>= \case
      Just _ -> pure ()
      Nothing ->
        readDesktopBase adapter
          >>= \observed -> report adapter observed
  pure adapter

apply :: (Ui es) => ThemeAdapter es -> Appearance -> PaletteMode -> Eff es ()
apply adapter appearance desktop = do
  case themeAsset (resolveTheme appearance desktop) of
    Nothing -> Gtk.cssProviderLoadFromString adapter.provider ""
    Just relative -> do
      path <- resolveAsset relative
      Gtk.cssProviderLoadFromPath adapter.provider path
  previous <- liftIO (readIORef adapter.forced)
  let wanted = forcedBase appearance.base
  liftIO (writeIORef adapter.forced wanted)
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

readDesktopBase :: (IOE :> es) => ThemeAdapter es -> Eff es PaletteMode
readDesktopBase adapter = Adw.styleManagerGetDark adapter.manager <&> \dark -> if dark then DarkPalette else LightPalette

onDesktopBase :: (IOE :> es) => ThemeAdapter es -> (PaletteMode -> Eff es ()) -> Eff es ()
onDesktopBase adapter notify = do
  liftIO (writeIORef adapter.handler (Just notify))
  liftIO (readIORef adapter.forced) >>= \case
    Just _ -> pure ()
    Nothing -> readDesktopBase adapter >>= \observed -> notify observed

report :: (IOE :> es) => ThemeAdapter es -> PaletteMode -> Eff es ()
report adapter observed =
  liftIO (readIORef adapter.handler) >>= mapM_ (\notify -> notify observed)

loadPalettes :: (Ui es) => Eff es (Vector Palette)
loadPalettes = readThemeListing <&> maybe V.empty (uncurry palettesFrom)

readThemeListing :: (Ui es) => Eff es (Maybe (FilePath, ThemeListing))
readThemeListing = do
  root <- resolveAsset themeRoot
  present <- liftIO (doesDirectoryExist root)
  if not present
    then pure Nothing
    else do
      directories <- liftIO (childDirectories root)
      listed <- mapM (\family -> familyListing root family) directories
      pure (Just (root, ThemeListing {families = V.fromList listed}))

themeRoot :: FilePath
themeRoot = "assets" </> "themes"

familyListing :: (Ui es) => FilePath -> FilePath -> Eff es FamilyListing
familyListing root family = do
  info <- familyInfo (root </> family)
  directories <- liftIO (childDirectories (root </> family))
  listed <- liftIO (mapM (\directory -> modeListing (root </> family) directory) directories)
  pure FamilyListing {directory = T.pack family, info, modes = V.fromList (catMaybes listed)}

modeListing :: FilePath -> FilePath -> IO (Maybe (PaletteMode, Vector Text))
modeListing familyDir directory = case paletteMode directory of
  Nothing -> pure Nothing
  Just mode -> do
    entries <- listDirectory (familyDir </> directory)
    let files = entries & filter (\entry -> takeExtension entry == ".css") & map T.pack & V.fromList
    pure (Just (mode, files))

paletteMode :: FilePath -> Maybe PaletteMode
paletteMode directory =
  find (\mode -> modeDirectory mode == T.pack directory) [minBound .. maxBound]

familyInfo :: (Ui es) => FilePath -> Eff es FamilyInfo
familyInfo familyDir = do
  present <- liftIO (doesFileExist path)
  if not present
    then pure noFamilyInfo
    else
      liftIO (Aeson.eitherDecodeFileStrict' path) >>= \case
        Left reason -> do
          logAttention_ (T.pack (path <> ": " <> reason))
          pure noFamilyInfo
        Right found -> pure found
  where
    path = familyDir </> "family.json"

childDirectories :: FilePath -> IO (List FilePath)
childDirectories parent = do
  entries <- listDirectory parent
  filterM (\entry -> doesDirectoryExist (parent </> entry)) entries
