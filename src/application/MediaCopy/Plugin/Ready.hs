module MediaCopy.Plugin.Ready
  ( Ready (..)
  ) where

import Data.Aeson (Value)
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)

import MediaCopy.Plugin.Installed qualified as Plugin (Installed)

data Ready = Ready
  { installed :: Plugin.Installed
  , granted :: Set Plugin.Capability
  , settings :: Map Text Value
  , trace :: Bool
  }
  deriving stock (Eq, Show)
