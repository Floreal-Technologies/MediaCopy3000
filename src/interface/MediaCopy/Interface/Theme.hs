-- | The palettes the operator can choose between, as the asset tree presents them.
module MediaCopy.Interface.Theme
  ( Theme (..)
  , themeMode
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
  , paletteOf
  , setPalette
  , resolveTheme
  , forcedScheme
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

-- | Which of the toolkit's two base stylesheets a palette was made for.
data PaletteMode = DarkPalette | LightPalette
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display PaletteMode where
  displayBuilder = \case
    DarkPalette -> "Dark"
    LightPalette -> "Light"

-- | What a family says about itself, in @\<family\>\/family.json@. A field the file leaves out
-- falls back to the tree, so a family needs no file at all.
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

-- | What a family with no file of its own says: nothing.
noFamilyInfo :: FamilyInfo
noFamilyInfo = FamilyInfo {name = Nothing, homepage = Nothing}

-- | One stylesheet under @assets\/themes@, named by the directories above it. 'family' and
-- 'variant' hold the names on disk, and the dropdown title-cases them. Every palette of one family
-- carries that family's 'info'.
data Palette = Palette
  { family :: Text
  , mode :: PaletteMode
  , variant :: Text
  , asset :: FilePath
  , info :: FamilyInfo
  }
  deriving stock (Eq, Ord, Show)

-- | What a reader of @assets/themes@ found: one entry per family directory. A directory that names
-- neither @light@ nor @dark@ holds no palette of ours, and the reader drops it, so no
-- 'FamilyListing' ever carries one.
data ThemeListing = ThemeListing
  { families :: Vector FamilyListing
  }
  deriving stock (Eq, Ord, Show)

-- | One family directory: its name, what its own file says, and the CSS names under each mode.
data FamilyListing = FamilyListing
  { directory :: Text
  , info :: FamilyInfo
  , modes :: Vector (PaletteMode, Vector Text)
  -- ^ The file names with their extension, as @:family/(:light|:dark)/:name.css@ holds them.
  }
  deriving stock (Eq, Ord, Show)

-- | The palettes a listing names, under the root the reader found them in.
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

-- | The directory a palette of one mode sits in. The reader names a mode by this, and the
-- screenshot scenes name a picture by it.
modeDirectory :: PaletteMode -> Text
modeDirectory = \case
  DarkPalette -> "dark"
  LightPalette -> "light"

-- | 'SystemTheme' asks for no stylesheet of ours. It names the base it stands for, because each of
-- the two lists holds one of them.
data Theme
  = SystemTheme PaletteMode
  | PaletteTheme Palette
  deriving stock (Eq, Ord, Show)

-- | Every theme names its own base, which says which list it belongs to.
themeMode :: Theme -> PaletteMode
themeMode = \case
  SystemTheme mode -> mode
  PaletteTheme palette -> palette.mode

-- | The system row of one list, in the wording the dropdown shows.
systemLabel :: PaletteMode -> Text
systemLabel mode = "System " <> T.toLower (display mode)

-- | Who decides between the two halves of the pair.
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

-- | 'light' has mode 'LightPalette' and 'dark' has mode 'DarkPalette'.
data Appearance = Appearance
  { base :: Base
  , light :: Theme
  , dark :: Theme
  }
  deriving stock (Eq, Ord, Show)

-- | The start of a run: no stylesheet of ours, and no force.
systemAppearance :: Appearance
systemAppearance =
  Appearance
    { base = FollowDesktop
    , light = SystemTheme LightPalette
    , dark = SystemTheme DarkPalette
    }

-- | The half of the pair that dresses one base.
paletteOf :: PaletteMode -> Appearance -> Theme
paletteOf mode appearance = case mode of
  LightPalette -> appearance.light
  DarkPalette -> appearance.dark

-- | A theme replaces the half its own base names.
setPalette :: Theme -> Appearance -> Appearance
setPalette wanted appearance = case themeMode wanted of
  LightPalette -> appearance {light = wanted}
  DarkPalette -> appearance {dark = wanted}

-- | What the window wears, against the base the desktop reports.
resolveTheme :: Appearance -> PaletteMode -> Theme
resolveTheme appearance desktop =
  paletteOf (fromMaybe desktop (forcedBase appearance.base)) appearance

-- | 'Nothing' forces no base, which lets the desktop answer.
forcedScheme :: Appearance -> Maybe PaletteMode
forcedScheme appearance = forcedBase appearance.base

-- | A held base leaves one row with nothing to say, and that row goes insensitive.
usesBase :: PaletteMode -> Base -> Bool
usesBase mode base = maybe True (\held -> held == mode) (forcedBase base)

-- | The one reading of 'Base'. Every rule above answers from this, so the three
-- settings of the @Base@ row cannot drift apart.
forcedBase :: Base -> Maybe PaletteMode
forcedBase = \case
  FollowDesktop -> Nothing
  AlwaysLight -> Just LightPalette
  AlwaysDark -> Just DarkPalette

data ThemeSection = ThemeSection
  { heading :: Text
  , homepage :: Maybe Text
  -- ^ The family's own page. The dropdown puts a link on the heading that has one.
  , themes :: Vector Theme
  }
  deriving stock (Eq, Show)

-- | The sections of one palette list: that base's system row, then one section
-- for each family the asset tree holds with a stylesheet of that base.
themeSections :: PaletteMode -> Vector Palette -> Vector ThemeSection
themeSections mode palettes =
  V.toList palettes
    & filter (\palette -> palette.mode == mode)
    & sort
    & foldl (\sections palette -> addPalette sections palette) (V.singleton (systemSection mode))

-- | One list holds one system row, which names the base of the list it sits in.
systemSection :: PaletteMode -> ThemeSection
systemSection mode =
  ThemeSection {heading = "System", homepage = Nothing, themes = V.singleton (SystemTheme mode)}

-- | The palettes arrive sorted, so every palette of one family reaches the
-- section the first of them opened.
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

-- | The row a theme shows under its heading. The list it sits in names the
-- base, so the row does not.
themeRowLabel :: Theme -> Text
themeRowLabel = \case
  SystemTheme mode -> systemLabel mode
  PaletteTheme palette -> titleCase palette.variant

-- | 'Nothing' asks for no stylesheet of ours, so the desktop theme stays as it is.
themeAsset :: Theme -> Maybe FilePath
themeAsset = \case
  SystemTheme _ -> Nothing
  PaletteTheme palette -> Just palette.asset

-- | The file's own name for the family wins over the name of its directory.
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
