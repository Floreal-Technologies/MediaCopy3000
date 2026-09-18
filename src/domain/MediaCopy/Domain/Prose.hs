module MediaCopy.Domain.Prose
  ( plural
  , oneLine
  ) where

import Data.Char (GeneralCategory (..), generalCategory, isControl)
import Data.Text qualified as T

plural :: Text -> Int -> Text
plural noun 1 = "1 " <> noun
plural noun n = T.show n <> " " <> noun <> "s"

-- |
-- >>> oneLine "two\nlines\tand\ESCa\8232separator"
-- "two lines and a separator"
oneLine :: Text -> Text
oneLine = T.map (\c -> if isControl c || generalCategory c `elem` [LineSeparator, ParagraphSeparator] then ' ' else c)
