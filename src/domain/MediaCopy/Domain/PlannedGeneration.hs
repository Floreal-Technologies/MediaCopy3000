module MediaCopy.Domain.PlannedGeneration
  ( PlannedGeneration (..)
  ) where

import Ascmhl.Process (Process)
import Ascmhl.RelPath (RelPath)

data PlannedGeneration = PlannedGeneration
  { folder :: OsPath
  , number :: Int
  , manifest :: OsPath
  , process :: Process
  , directories :: Vector RelPath
  }
  deriving stock (Eq, Show)
