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
import Data.GI.Base (AttrOp ((:=)), on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List, find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful.Log (logAttention_)
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment, logWith)
import MediaCopy.Interface.Theme

data ThemeAdapter = ThemeAdapter
  { environment :: Environment
  , provider :: Gtk.CssProvider
  , manager :: Adw.StyleManager
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
  forced <- newIORef Nothing
  handler <- newIORef Nothing
  let adapter = ThemeAdapter {environment, provider, manager, forced, handler}
  void $ on manager (Adw.PropertyNotify #dark) $ \_ ->
    readIORef forced >>= \case
      Just _ -> pure ()
      Nothing ->
        readDesktopBase adapter
          >>= \observed -> report adapter observed
  pure adapter

apply :: ThemeAdapter -> Appearance -> PaletteMode -> IO ()
apply adapter appearance desktop = do
  case themeAsset (resolveTheme appearance desktop) of
    Nothing -> Gtk.cssProviderLoadFromString adapter.provider ""
    Just relative -> do
      path <- resolveAsset adapter.environment relative
      Gtk.cssProviderLoadFromPath adapter.provider path
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

loadPalettes :: Environment -> IO (Vector Palette)
loadPalettes environment = readThemeListing environment <&> maybe V.empty (uncurry palettesFrom)

readThemeListing :: Environment -> IO (Maybe (FilePath, ThemeListing))
readThemeListing environment = do
  root <- resolveAsset environment themeRoot
  present <- doesDirectoryExist root
  if not present
    then pure Nothing
    else do
      directories <- childDirectories root
      listed <- mapM (\family -> familyListing environment root family) directories
      pure (Just (root, ThemeListing {families = V.fromList listed}))

themeRoot :: FilePath
themeRoot = "assets" </> "themes"

familyListing :: Environment -> FilePath -> FilePath -> IO FamilyListing
familyListing environment root family = do
  info <- familyInfo environment (root </> family)
  directories <- childDirectories (root </> family)
  listed <- mapM (\directory -> modeListing (root </> family) directory) directories
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

familyInfo :: Environment -> FilePath -> IO FamilyInfo
familyInfo environment familyDir = do
  present <- doesFileExist path
  if not present
    then pure noFamilyInfo
    else
      Aeson.eitherDecodeFileStrict' path >>= \case
        Left reason -> do
          logWith environment (logAttention_ (T.pack (path <> ": " <> reason)))
          pure noFamilyInfo
        Right found -> pure found
  where
    path = familyDir </> "family.json"

childDirectories :: FilePath -> IO (List FilePath)
childDirectories parent = do
  entries <- listDirectory parent
  filterM (\entry -> doesDirectoryExist (parent </> entry)) entries
