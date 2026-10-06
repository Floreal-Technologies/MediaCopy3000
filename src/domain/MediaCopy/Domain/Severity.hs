module MediaCopy.Domain.Severity
  ( Severity (..)
  ) where

import Data.Text.Display (Display (..))

-- $setup
-- >>> import Data.Text.Display (display)

data Severity = Blocker | Warning
  deriving stock (Eq, Ord, Show)

-- | >>> map display [Blocker, Warning]
-- ["blocker","warning"]
instance Display Severity where
  displayBuilder = \case
    Blocker -> "blocker"
    Warning -> "warning"
