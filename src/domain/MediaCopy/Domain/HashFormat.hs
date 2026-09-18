module MediaCopy.Domain.HashFormat
  ( FormatError (..)
  , settle
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath (RelPath)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

newtype FormatError = MixedFormats (List Algo)
  deriving stock (Eq, Show)

-- | >>> display (MixedFormats [Algo.MD5, Algo.SHA1])
-- "ASC MHL: the originals hold more than one hash format: md5, sha1"
instance Display FormatError where
  displayBuilder (MixedFormats mixed) =
    displayBuilder ("ASC MHL: the originals hold more than one hash format: " <> T.intercalate ", " (map display mixed))

settle :: Map RelPath Hash -> Set RelPath -> Either FormatError Algo
settle expected present = case (algosOf (Map.restrictKeys expected present), algosOf expected) of
  ([algo], _) -> Right algo
  (mixed@(_ : _ : _), _) -> Left (MixedFormats mixed)
  ([], [algo]) -> Right algo
  ([], _) -> Right Algo.preferred

algosOf :: Map RelPath Hash -> List Algo
algosOf hashes = hashes & Map.elems & map (.algo) & Set.fromList & Set.toList
