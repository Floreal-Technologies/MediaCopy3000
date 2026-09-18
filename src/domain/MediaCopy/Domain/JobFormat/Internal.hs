module MediaCopy.Domain.JobFormat.Internal
  ( JobFormat (..)
  ) where

import Ascmhl.Hash (HashAlgo)

newtype JobFormat = JobFormat HashAlgo
  deriving stock (Eq, Show)
