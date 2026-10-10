module MediaCopy.Interface.Theme
  ( PaletteMode (..)
  , Base (..)
  , Accent (..)
  , Appearance (..)
  , systemAppearance
  , forcedBase
  , resolveMode
  , displayBase
  , displayAccent
  , accentName
  , accentColour
  , accentCss
  ) where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Messages

data PaletteMode = DarkPalette | LightPalette
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Base
  = FollowDesktop
  | AlwaysLight
  | AlwaysDark
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Accent
  = Blue
  | Teal
  | Green
  | Yellow
  | Orange
  | Red
  | Pink
  | Purple
  | Slate
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Appearance = Appearance
  { base :: Base
  , accent :: Maybe Accent
  }
  deriving stock (Eq, Ord, Show)

systemAppearance :: Appearance
systemAppearance = Appearance {base = FollowDesktop, accent = Nothing}

forcedBase :: Base -> Maybe PaletteMode
forcedBase = \case
  FollowDesktop -> Nothing
  AlwaysLight -> Just LightPalette
  AlwaysDark -> Just DarkPalette

-- |
-- >>> resolveMode FollowDesktop DarkPalette
-- DarkPalette
-- >>> resolveMode AlwaysLight DarkPalette
-- LightPalette
resolveMode :: Base -> PaletteMode -> PaletteMode
resolveMode base desktop = fromMaybe desktop (forcedBase base)

displayBase :: Wording -> Base -> Text
displayBase wording = \case
  FollowDesktop -> getTranslation' wording themeBaseFollowDesktop []
  AlwaysLight -> getTranslation' wording themeBaseAlwaysLight []
  AlwaysDark -> getTranslation' wording themeBaseAlwaysDark []

displayAccent :: Wording -> Maybe Accent -> Text
displayAccent wording = \case
  Nothing -> getTranslation' wording themeAccentDesktop []
  Just accent -> accentLabel wording accent

accentLabel :: Wording -> Accent -> Text
accentLabel wording = \case
  Blue -> getTranslation' wording themeAccentBlue []
  Teal -> getTranslation' wording themeAccentTeal []
  Green -> getTranslation' wording themeAccentGreen []
  Yellow -> getTranslation' wording themeAccentYellow []
  Orange -> getTranslation' wording themeAccentOrange []
  Red -> getTranslation' wording themeAccentRed []
  Pink -> getTranslation' wording themeAccentPink []
  Purple -> getTranslation' wording themeAccentPurple []
  Slate -> getTranslation' wording themeAccentSlate []

-- |
-- >>> accentName Purple
-- "purple"
accentName :: Accent -> Text
accentName accent = T.toLower (T.pack (show accent))

-- |
-- >>> accentColour LightPalette Blue
-- "#3584e4"
-- >>> accentColour DarkPalette Orange
-- "#ef9f76"
accentColour :: PaletteMode -> Accent -> Text
accentColour = \case
  LightPalette -> \case
    Blue -> "#3584e4"
    Teal -> "#2190a4"
    Green -> "#3a944a"
    Yellow -> "#c88800"
    Orange -> "#ed5b00"
    Red -> "#e62d42"
    Pink -> "#d56199"
    Purple -> "#9141ac"
    Slate -> "#6f8396"
  DarkPalette -> \case
    Blue -> "#8caaee"
    Teal -> "#81c8be"
    Green -> "#a6d189"
    Yellow -> "#e5c890"
    Orange -> "#ef9f76"
    Red -> "#e78284"
    Pink -> "#f4b8e4"
    Purple -> "#ca9ee6"
    Slate -> "#949cbb"

-- |
-- >>> T.lines (accentCss LightPalette Teal Blue) & take 2
-- [":root { --accent-bg-color: #2190a4; }",".accent-swatch.desktop { background-color: #3584e4; }"]
-- >>> T.lines (accentCss DarkPalette Teal Blue) & take 1
-- [":root { --accent-bg-color: #81c8be; --accent-color: #81c8be; --accent-fg-color: #232634; }"]
accentCss :: PaletteMode -> Accent -> Accent -> Text
accentCss mode chosen desktop =
  (root : swatch "desktop" desktop : map (\accent -> swatch (accentName accent) accent) [minBound .. maxBound])
    & T.unlines
  where
    colour = accentColour mode chosen
    root = case mode of
      LightPalette -> ":root { --accent-bg-color: " <> colour <> "; }"
      DarkPalette ->
        ":root { --accent-bg-color: " <> colour <> "; --accent-color: " <> colour <> "; --accent-fg-color: #232634; }"
    swatch name accent = ".accent-swatch." <> name <> " { background-color: " <> accentColour mode accent <> "; }"
