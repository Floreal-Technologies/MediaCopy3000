module Ascmhl.Author
  ( Author (..)
  ) where

import Data.Text (Text)

data Author = Author
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , role :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)
