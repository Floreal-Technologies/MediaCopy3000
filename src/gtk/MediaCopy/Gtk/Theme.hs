-- | The stylesheet that carries the operator's chosen palette, and where the palettes come from.
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
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import MediaCopy.Gtk.Assets (resolveAsset)
import MediaCopy.Gtk.Environment (Environment)
import MediaCopy.Gtk.Log (logLine)
import MediaCopy.Interface.Theme

-- | While a color scheme is forced, 'Adw.styleManagerGetDark' answers with that forced scheme and
-- not with the desktop's wish. So the adapter reports a base only while it forces nothing, and
-- reads the true base again the moment it stops forcing one.
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
    -- An empty stylesheet leaves the operator's own desktop theme in place.
    Nothing -> Gtk.cssProviderLoadFromString adapter.provider ""
    Just relative -> do
      path <- resolveAsset adapter.environment relative
      Gtk.cssProviderLoadFromPath adapter.provider path
  previous <- readIORef adapter.forced
  let wanted = forcedScheme appearance
  writeIORef adapter.forced wanted
  set adapter.manager [#colorScheme := schemeOf wanted]
  case (previous, wanted) of
    (Just _, Nothing) -> do
      observed <- readDesktopBase adapter
      when (observed /= desktop) (report adapter observed)
    _ -> pure ()

-- | Describe the desktop only while the adapter forces
-- nothing, which holds at start-up, before any appearance is applied.
readDesktopBase :: ThemeAdapter -> IO PaletteMode
readDesktopBase adapter = Adw.styleManagerGetDark adapter.manager <&> \dark -> modeOfDark dark

-- | Register the adapter's handler, then reports the base once, so a change the
-- desktop made between 'readDesktopBase' at start-up and this call is not lost.
onDesktopBase :: ThemeAdapter -> (PaletteMode -> IO ()) -> IO ()
onDesktopBase adapter notify = do
  writeIORef adapter.handler (Just notify)
  readIORef adapter.forced >>= \case
    Just _ -> pure ()
    Nothing -> readDesktopBase adapter >>= \observed -> notify observed

-- | A report before 'onDesktopBase' runs goes nowhere; the registration's own
-- report covers that window.
report :: ThemeAdapter -> PaletteMode -> IO ()
report adapter observed =
  readIORef adapter.handler >>= mapM_ (\notify -> notify observed)

-- | The toolkit answers with a flag; the domain names the two bases.
modeOfDark :: Bool -> PaletteMode
modeOfDark dark = if dark then DarkPalette else LightPalette

schemeOf :: Maybe PaletteMode -> Adw.ColorScheme
schemeOf = \case
  Nothing -> Adw.ColorSchemeDefault
  Just LightPalette -> Adw.ColorSchemeForceLight
  Just DarkPalette -> Adw.ColorSchemeForceDark

-- | Every palette the asset tree holds. An absent tree holds none.
loadPalettes :: Environment -> IO (Vector Palette)
loadPalettes environment = readThemeListing environment <&> maybe V.empty (uncurry palettesFrom)

-- | The tree as it stands, with the root it was found under. 'Nothing' when
-- there is no tree. The reader names directories and files and decides nothing
-- else.
readThemeListing :: Environment -> IO (Maybe (FilePath, ThemeListing))
readThemeListing environment = do
  root <- resolveAsset environment themeRoot
  present <- doesDirectoryExist root
  if not present
    then pure Nothing
    else do
      directories <- childDirectories root
      listed <- mapM (\family -> familyListing root family) directories
      pure (Just (root, ThemeListing {families = V.fromList listed}))

themeRoot :: FilePath
themeRoot = "assets" </> "themes"

familyListing :: FilePath -> FilePath -> IO FamilyListing
familyListing root family = do
  info <- familyInfo (root </> family)
  directories <- childDirectories (root </> family)
  listed <- mapM (\directory -> modeListing (root </> family) directory) directories
  pure FamilyListing {directory = T.pack family, info, modes = V.fromList (catMaybes listed)}

-- | A directory that names no mode holds no palette of ours, so the listing
-- never carries it.
modeListing :: FilePath -> FilePath -> IO (Maybe (PaletteMode, Vector Text))
modeListing familyDir directory = case paletteMode directory of
  Nothing -> pure Nothing
  Just mode -> do
    entries <- listDirectory (familyDir </> directory)
    let files = entries & filter (\entry -> takeExtension entry == ".css") & map T.pack & V.fromList
    pure (Just (mode, files))

-- | The name a mode's directory has is 'modeDirectory'; this reads that one
-- rule backwards.
paletteMode :: FilePath -> Maybe PaletteMode
paletteMode directory =
  find (\mode -> modeDirectory mode == T.pack directory) [minBound .. maxBound]

-- | A family says its name and its page in one JSON file.
familyInfo :: FilePath -> IO FamilyInfo
familyInfo familyDir = do
  present <- doesFileExist path
  if not present
    then pure noFamilyInfo
    else
      Aeson.eitherDecodeFileStrict' path >>= \case
        Left reason -> do
          logLine (path <> ": " <> reason)
          pure noFamilyInfo
        Right found -> pure found
  where
    path = familyDir </> familyFile

familyFile :: FilePath
familyFile = "family.json"

childDirectories :: FilePath -> IO (List FilePath)
childDirectories parent = do
  entries <- listDirectory parent
  filterM (\entry -> doesDirectoryExist (parent </> entry)) entries
