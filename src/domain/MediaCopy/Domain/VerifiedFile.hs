module MediaCopy.Domain.VerifiedFile
  ( VerifiedFile (..)
  ) where

import Ascmhl.Hash (Hash)
import Ascmhl.RelPath (RelPath)

data VerifiedFile = VerifiedFile
  { path :: RelPath
  , location :: OsPath
  , hashes :: Vector Hash
  , size :: Int64
  }
  deriving stock (Eq, Show)
