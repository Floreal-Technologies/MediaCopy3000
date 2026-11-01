module MediaCopy.Domain.JobEnd
  ( JobEnd (..)
  , WrittenManifest (..)
  ) where

import MediaCopy.Domain.JobResult (JobResult)

data JobEnd = JobEnd
  { result :: Either Text JobResult
  , manifests :: Vector WrittenManifest
  }
  deriving stock (Eq, Show)

data WrittenManifest = WrittenManifest
  { folder :: OsPath
  , path :: OsPath
  }
  deriving stock (Eq, Show)
