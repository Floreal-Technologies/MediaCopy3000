module MediaCopy.Interface.View.Row
  ( Tone (..)
  , RowView (..)
  , rowView
  ) where

import Data.Text (Text)

data Tone = Good | Warn | Bad
  deriving stock (Eq, Show)

data RowView = RowView
  { title :: Text
  , subtitle :: Text
  , tone :: Maybe Tone
  }
  deriving stock (Eq, Show)

-- |
-- >>> rowView "7 files" "17.3 GB"
-- RowView {title = "7 files", subtitle = "17.3 GB", tone = Nothing}
rowView :: Text -> Text -> RowView
rowView title subtitle = RowView {title, subtitle, tone = Nothing}
