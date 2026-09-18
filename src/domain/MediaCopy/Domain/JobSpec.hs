module MediaCopy.Domain.JobSpec
  ( JobSpec (..)
  ) where

import Data.Time (UTCTime)

import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.JobId (JobId)

data JobSpec = JobSpec
  { id :: JobId
  , job :: Job
  , createdAt :: UTCTime
  , pluginFields :: Map Text (Map Text Text)
  }
  deriving stock (Eq, Show)
