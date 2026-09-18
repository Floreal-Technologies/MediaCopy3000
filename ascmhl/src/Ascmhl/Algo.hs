module Ascmhl.Algo
  ( Algo (..)
  , Spec (..)
  , spec
  , preferred
  , fromMhlElement
  , renderSet
  ) where

import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)

import Ascmhl.C4 qualified as C4
import Ascmhl.Hex qualified as Hex

-- $setup
-- >>> import Data.Set qualified as Set

data Algo = XXH64 | MD5 | SHA1 | C4
  deriving stock (Eq, Ord, Show, Bounded, Enum)

data Spec = Spec
  { mhlElement :: Text
  , readValue :: Text -> Text
  , toDigest :: Text -> Maybe ByteString
  }

-- |
-- >>> (spec SHA1).readValue "AB12"
-- "ab12"
-- >>> (spec C4).readValue "c4Ab"
-- "c4Ab"
spec :: Algo -> Spec
spec = \case
  XXH64 -> hexSpec "xxh64"
  MD5 -> hexSpec "md5"
  SHA1 -> hexSpec "sha1"
  C4 -> Spec {mhlElement = "c4", readValue = id, toDigest = C4.toBytes}
  where
    hexSpec element =
      Spec {mhlElement = element, readValue = T.toLower, toDigest = Hex.toBytes}

-- |
-- >>> preferred
-- XXH64
preferred :: Algo
preferred = XXH64

-- |
-- >>> map display [minBound .. maxBound :: Algo]
-- ["xxh64","md5","sha1","c4"]
instance Display Algo where
  displayBuilder which = displayBuilder (spec which).mhlElement

fromMhlElement :: Text -> Maybe Algo
fromMhlElement (T.toLower -> t) =
  find (\which -> (spec which).mhlElement == t) [minBound .. maxBound]

-- |
-- >>> renderSet (Set.fromList [SHA1, XXH64])
-- "xxh64/sha1"
renderSet :: Set Algo -> Text
renderSet algos = algos & Set.toList & map display & T.intercalate "/"
