module MediaCopy.Plugin.Installed
  ( Installed (..)
  ) where

import MediaCopy.Plugin.Manifest qualified as Plugin (Manifest)

data Installed = Installed
  { folder :: OsPath
  , manifest :: Plugin.Manifest
  , executable :: OsPath
  }
  deriving stock (Eq, Show)
