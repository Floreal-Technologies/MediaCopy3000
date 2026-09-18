module Ascmhl.DirectoryEntry
  ( DirectoryEntry (..)
  , new
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Name, Node)

import Ascmhl.DirHash (DirHash)
import Ascmhl.RelPath (RelPath)

data DirectoryEntry = DirectoryEntry
  { path :: RelPath
  , lastModified :: UTCTime
  , hashes :: Vector DirHash
  , pathAttrs :: Map Name Text
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

new :: RelPath -> UTCTime -> DirHash -> DirectoryEntry
new path lastModified pair =
  DirectoryEntry
    { path
    , lastModified
    , hashes = V.singleton pair
    , pathAttrs = Map.empty
    , unknown = V.empty
    }
