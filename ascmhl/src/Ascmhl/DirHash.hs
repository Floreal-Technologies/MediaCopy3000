module Ascmhl.DirHash
  ( DirHash (..)
  , new
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import Text.XML (Name)

import Ascmhl.Hash (Hash)

data DirHash = DirHash
  { content :: Hash
  , structure :: Hash
  , hashDate :: Maybe UTCTime
  , extraAttrs :: Map Name Text
  }
  deriving stock (Eq, Show)

new :: UTCTime -> Hash -> Hash -> DirHash
new hashDate content structure =
  DirHash {content, structure, hashDate = Just hashDate, extraAttrs = Map.empty}
