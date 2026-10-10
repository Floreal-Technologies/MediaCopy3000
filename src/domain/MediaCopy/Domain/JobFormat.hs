module MediaCopy.Domain.JobFormat
  ( FormatError (..)
  , settleFormat
  ) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath)
import Data.Function ((&))
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)

-- $setup
-- >>> import Ascmhl.Path (RelPath (..))

newtype FormatError = MixedFormats (List HashAlgo)
  deriving stock (Eq, Show)

-- | >>> display (MixedFormats [MD5, SHA1])
-- "ASC MHL: the originals hold more than one hash format: md5, sha1"
instance Display FormatError where
  displayBuilder (MixedFormats mixed) =
    displayBuilder ("ASC MHL: the originals hold more than one hash format: " <> T.intercalate ", " (map display mixed))

-- |
-- >>> settleFormat Map.empty Set.empty
-- Right XXH64
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = MD5, value = "0f"})]) (Set.fromList [RelPath "a.mxf"])
-- Right MD5
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = SHA1, value = "ab"})]) (Set.fromList [RelPath "b.mxf"])
-- Right SHA1
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = MD5, value = "0f"}), (RelPath "b.mxf", Hash {algo = SHA1, value = "ab"})]) (Set.fromList [RelPath "a.mxf", RelPath "b.mxf"])
-- Left (MixedFormats [MD5,SHA1])
settleFormat :: Map RelPath Hash -> Set RelPath -> Either FormatError HashAlgo
settleFormat expected present = case (algosOf (Map.restrictKeys expected present), algosOf expected) of
  ([algo], _) -> Right algo
  (mixed@(_ : _ : _), _) -> Left (MixedFormats mixed)
  ([], [algo]) -> Right algo
  ([], _) -> Right preferredAlgo

algosOf :: Map RelPath Hash -> List HashAlgo
algosOf hashes = hashes & Map.elems & map (.algo) & Set.fromList & Set.toList
