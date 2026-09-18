module MediaCopy.Interface.Palette
  ( paletteEntries
  , paletteView
  ) where

import Floreal.Commons.Palette (Entry (..), PaletteView)
import Floreal.Commons.Palette qualified as Palette

import MediaCopy.Interface.Command
import MediaCopy.Interface.Translation (Wording)

paletteEntries :: Wording -> (Command -> Bool) -> List (Entry Command)
paletteEntries wording enabledNow =
  fmap
    (\command -> Entry {key = command, identifier = commandId command, label = commandLabel wording command, enabled = enabledNow command})
    (filter (/= CommandPalette) commands)

paletteView :: Wording -> (Command -> Bool) -> Maybe Text -> PaletteView Command
paletteView wording enabledNow =
  Palette.paletteView (palettePlaceholder wording) (paletteNoMatch wording) (paletteEntries wording enabledNow)
