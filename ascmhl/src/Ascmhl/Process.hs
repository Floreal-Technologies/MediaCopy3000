module Ascmhl.Process
  ( Process (..)
  ) where

import Data.Text.Display (Display (..))

-- $setup
-- >>> import Data.Text.Display (display)

data Process = Transfer | InPlace | Flatten
  deriving stock (Bounded, Enum, Eq, Show)

-- |
-- >>> map display [Transfer, InPlace, Flatten]
-- ["transfer","in-place","flatten"]
instance Display Process where
  displayBuilder = \case
    Transfer -> "transfer"
    InPlace -> "in-place"
    Flatten -> "flatten"
