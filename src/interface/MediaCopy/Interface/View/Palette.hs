module MediaCopy.Interface.View.Palette
  ( PaletteItem (..)
  , PaletteView (..)
  , paletteView
  ) where

import Data.List (List)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)

import MediaCopy.Interface.Command (Command, commandLabel, paletteNoMatch, palettePlaceholder)
import MediaCopy.Interface.Palette (PaletteRow (..), paletteRows)
import MediaCopy.Interface.Translation (Wording)

-- $setup
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)

data PaletteItem = PaletteItem
  { row :: PaletteRow
  , label :: Text
  }
  deriving stock (Eq, Show)

data PaletteView = PaletteView
  { open :: Bool
  , query :: Text
  , items :: List PaletteItem
  , placeholder :: Text
  , noMatch :: Text
  }
  deriving stock (Eq, Show)

-- |
-- >>> let view = paletteView (embeddedWording English) (const True) (Just "pref")
-- >>> (view.open, view.query, map (\item -> item.label) view.items)
-- (True,"pref",["Preferences"])
-- >>> (paletteView (embeddedWording English) (const True) Nothing).open
-- False
paletteView :: Wording -> (Command -> Bool) -> Maybe Text -> PaletteView
paletteView wording enabledNow palette =
  PaletteView
    { open = isJust palette
    , query = fromMaybe "" palette
    , items = map (\row -> PaletteItem {row, label = commandLabel wording row.command}) (paletteRows enabledNow wording palette)
    , placeholder = palettePlaceholder wording
    , noMatch = paletteNoMatch wording
    }
