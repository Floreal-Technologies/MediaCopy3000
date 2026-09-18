module MediaCopy.Domain.JobResult
  ( JobResult (..)
  ) where

data JobResult = AllOk | WithFailures Int
  deriving stock (Eq, Show)
