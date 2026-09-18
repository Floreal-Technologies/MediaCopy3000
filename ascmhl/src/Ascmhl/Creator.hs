module Ascmhl.Creator
  ( Creator (..)
  , new
  , addAuthors
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Node)

import Ascmhl.Author (Author)

data Creator = Creator
  { creationDate :: UTCTime
  , hostname :: Text
  , toolName :: Text
  , toolVersion :: Maybe Text
  , authors :: Vector Author
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

new :: UTCTime -> Text -> Text -> Text -> Creator
new creationDate hostname toolName toolVersion =
  Creator
    { creationDate
    , hostname
    , toolName
    , toolVersion = Just toolVersion
    , authors = V.empty
    , unknown = V.empty
    }

addAuthors :: Vector Author -> Creator -> Creator
addAuthors authors creator = creator {authors = creator.authors <> authors}
