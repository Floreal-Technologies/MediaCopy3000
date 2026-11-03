module MediaCopy.Domain.JobId
  ( JobId (..)
  , getJobIdNumber
  ) where

newtype JobId = JobId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

getJobIdNumber :: JobId -> Int
getJobIdNumber (JobId n) = n
