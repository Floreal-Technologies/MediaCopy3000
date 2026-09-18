module Ascmhl.ChainEntry
  ( ChainEntry (..)
  , new
  ) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Node)

import Ascmhl.Hash (Hash)
import Ascmhl.RelPath (RelPath)

data ChainEntry = ChainEntry
  { sequenceNr :: Int
  , path :: RelPath
  , c4 :: Maybe Hash
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

new :: Int -> RelPath -> Maybe Hash -> ChainEntry
new sequenceNr path c4 = ChainEntry {sequenceNr, path, c4, unknown = V.empty}
