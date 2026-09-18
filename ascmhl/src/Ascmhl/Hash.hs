module Ascmhl.Hash
  ( Hash (..)
  , digest
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo

-- $setup
-- >>> import Ascmhl.Hex qualified as Hex

data Hash = Hash
  { algo :: Algo
  , value :: Text
  }
  deriving stock (Eq, Ord, Show)

-- |
-- >>> fmap Hex.fromBytes (digest Hash {algo = Algo.MD5, value = "0f1e"})
-- Just "0f1e"
-- >>> digest Hash {algo = Algo.MD5, value = "0f1"}
-- Nothing
digest :: Hash -> Maybe ByteString
digest h = (Algo.spec h.algo).toDigest h.value
