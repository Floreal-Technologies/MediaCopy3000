module Ascmhl.FileEntry
  ( FileEntry (..)
  , HashRecord (..)
  , Action (..)
  , new
  , withMetadata
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Name, Node)

import Ascmhl.Fragment (Fragment)
import Ascmhl.Hash (Hash)
import Ascmhl.RelPath (RelPath)
import Ascmhl.Xml qualified as Xml

-- $setup
-- >>> import Data.Text.Display (display)

data Action = Original | Verified | Failed
  deriving stock (Eq, Show)

-- |
-- >>> map display [Original, Verified, Failed]
-- ["original","verified","failed"]
instance Display Action where
  displayBuilder = \case
    Original -> "original"
    Verified -> "verified"
    Failed -> "failed"

data HashRecord = HashRecord
  { hash :: Hash
  , action :: Action
  , hashDate :: Maybe UTCTime
  , extraAttrs :: Map Name Text
  }
  deriving stock (Eq, Show)

data FileEntry = FileEntry
  { path :: RelPath
  , size :: Int64
  , lastModified :: UTCTime
  , hashes :: Vector HashRecord
  , pathAttrs :: Map Name Text
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

new :: RelPath -> Int64 -> UTCTime -> Hash -> Action -> UTCTime -> FileEntry
new path size lastModified hash action hashDate =
  FileEntry
    { path
    , size
    , lastModified
    , hashes = V.singleton HashRecord {hash, action, hashDate = Just hashDate, extraAttrs = Map.empty}
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

withMetadata :: Fragment -> FileEntry -> FileEntry
withMetadata fragment entry =
  FileEntry
    { path = entry.path
    , size = entry.size
    , lastModified = entry.lastModified
    , hashes = entry.hashes
    , pathAttrs = entry.pathAttrs
    , unknown = V.snoc entry.unknown (Xml.metadataNode fragment)
    }
