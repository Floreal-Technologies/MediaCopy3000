module MediaCopy.Domain.JobId
  ( JobId (..)
  ) where

newtype JobId = JobId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)
