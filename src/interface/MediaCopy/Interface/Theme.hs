module MediaCopy.Interface.Theme
  ( Theme (..)
  , Palette (..)
  , FamilyInfo (..)
  , noFamilyInfo
  , ThemeListing (..)
  , FamilyListing (..)
  , palettesFrom
  , modeDirectory
  , PaletteMode (..)
  , Base (..)
  , Appearance (..)
  , systemAppearance
  , setPalette
  , resolveTheme
  , forcedBase
  , usesBase
  , ThemeSection (..)
  , themeSections
  , themeRowLabel
  , themeAsset
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:?))
import Data.Char (toUpper)
import Data.Function ((&))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.FilePath (takeBaseName, (</>))

data PaletteMode = DarkPalette | LightPalette
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display PaletteMode where
  displayBuilder = \case
    DarkPalette -> "Dark"
    LightPalette -> "Light"

data FamilyInfo = FamilyInfo
  { name :: Maybe Text
  , homepage :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON FamilyInfo where
  parseJSON =
    withObject "FamilyInfo" $ \fields -> do
      name <- fields .:? "name"
      homepage <- fields .:? "homepage"
      pure FamilyInfo {name, homepage}

noFamilyInfo :: FamilyInfo
noFamilyInfo = FamilyInfo {name = Nothing, homepage = Nothing}

data Palette = Palette
  { family :: Text
  , mode :: PaletteMode
  , variant :: Text
  , asset :: FilePath
  , info :: FamilyInfo
  }
  deriving stock (Eq, Ord, Show)

data ThemeListing = ThemeListing
  { families :: Vector FamilyListing
  }
  deriving stock (Eq, Ord, Show)

data FamilyListing = FamilyListing
  { directory :: Text
  , info :: FamilyInfo
  , modes :: Vector (PaletteMode, Vector Text)
  }
  deriving stock (Eq, Ord, Show)

palettesFrom :: FilePath -> ThemeListing -> Vector Palette
palettesFrom root listing = V.concatMap (\family -> familyPalettes root family) listing.families

familyPalettes :: FilePath -> FamilyListing -> Vector Palette
familyPalettes root family =
  V.concatMap
    (\(mode, files) -> V.map (\file -> paletteAt mode file) files)
    family.modes
  where
    paletteAt mode file =
      Palette
        { family = family.directory
        , mode
        , variant = T.pack (takeBaseName (T.unpack file))
        , asset = root </> T.unpack family.directory </> T.unpack (modeDirectory mode) </> T.unpack file
        , info = family.info
        }

modeDirectory :: PaletteMode -> Text
modeDirectory = \case
  DarkPalette -> "dark"
  LightPalette -> "light"

data Theme
  = SystemTheme PaletteMode
  | PaletteTheme Palette
  deriving stock (Eq, Ord, Show)

data Base
  = FollowDesktop
  | AlwaysLight
  | AlwaysDark
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display Base where
  displayBuilder = \case
    FollowDesktop -> "Follow desktop"
    AlwaysLight -> "Always light"
    AlwaysDark -> "Always dark"

data Appearance = Appearance
  { base :: Base
  , light :: Theme
  , dark :: Theme
  }
  deriving stock (Eq, Ord, Show)

systemAppearance :: Appearance
systemAppearance =
  Appearance
    { base = FollowDesktop
    , light = SystemTheme LightPalette
    , dark = SystemTheme DarkPalette
    }

setPalette :: Theme -> Appearance -> Appearance
setPalette wanted appearance = case mode of
  LightPalette -> appearance {light = wanted}
  DarkPalette -> appearance {dark = wanted}
  where
    mode = case wanted of
      SystemTheme held -> held
      PaletteTheme palette -> palette.mode

resolveTheme :: Appearance -> PaletteMode -> Theme
resolveTheme appearance desktop = case fromMaybe desktop (forcedBase appearance.base) of
  LightPalette -> appearance.light
  DarkPalette -> appearance.dark

usesBase :: PaletteMode -> Base -> Bool
usesBase mode base = maybe True (\held -> held == mode) (forcedBase base)

forcedBase :: Base -> Maybe PaletteMode
forcedBase = \case
  FollowDesktop -> Nothing
  AlwaysLight -> Just LightPalette
  AlwaysDark -> Just DarkPalette

data ThemeSection = ThemeSection
  { heading :: Text
  , homepage :: Maybe Text
  , themes :: Vector Theme
  }
  deriving stock (Eq, Show)

themeSections :: PaletteMode -> Vector Palette -> Vector ThemeSection
themeSections mode palettes =
  V.toList palettes
    & filter (\palette -> palette.mode == mode)
    & sort
    & foldl (\sections palette -> addPalette sections palette) (V.singleton (systemSection mode))

systemSection :: PaletteMode -> ThemeSection
systemSection mode =
  ThemeSection {heading = "System", homepage = Nothing, themes = V.singleton (SystemTheme mode)}

addPalette :: Vector ThemeSection -> Palette -> Vector ThemeSection
addPalette sections palette =
  case V.unsnoc sections of
    Just (earlier, latest)
      | latest.heading == heading ->
          V.snoc
            earlier
            ThemeSection {heading = latest.heading, homepage = latest.homepage, themes = V.snoc latest.themes row}
    _ -> V.snoc sections ThemeSection {heading, homepage = palette.info.homepage, themes = V.singleton row}
  where
    heading = sectionHeading palette
    row = PaletteTheme palette

themeRowLabel :: Theme -> Text
themeRowLabel = \case
  SystemTheme mode -> "System " <> T.toLower (display mode)
  PaletteTheme palette -> titleCase palette.variant

themeAsset :: Theme -> Maybe FilePath
themeAsset = \case
  SystemTheme _ -> Nothing
  PaletteTheme palette -> Just palette.asset

sectionHeading :: Palette -> Text
sectionHeading palette = fromMaybe (titleCase palette.family) palette.info.name

titleCase :: Text -> Text
titleCase name =
  T.split (\letter -> letter == '-' || letter == '_') name
    & map (\word -> capitalise word)
    & T.unwords

capitalise :: Text -> Text
capitalise word = case T.uncons word of
  Nothing -> word
  Just (first, rest) -> T.cons (toUpper first) rest
