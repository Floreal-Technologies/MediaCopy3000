module MediaCopy.Domain.JobFormat
  ( JobFormat
  , FormatError (..)
  , formatAlgo
  , settleFormat
  , chainFormat
  ) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath)
import Data.Function ((&))
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)

import MediaCopy.Domain.JobFormat.Internal (JobFormat (..))

-- $setup
-- >>> import Ascmhl.Path (RelPath (..))

formatAlgo :: JobFormat -> HashAlgo
formatAlgo (JobFormat algo) = algo

chainFormat :: JobFormat
chainFormat = JobFormat C4

newtype FormatError = MixedFormats (List HashAlgo)
  deriving stock (Eq, Show)

-- | >>> display (MixedFormats [MD5, SHA1])
-- "ASC MHL: the originals hold more than one hash format: md5, sha1"
instance Display FormatError where
  displayBuilder (MixedFormats mixed) = displayBuilder (mixedMessage mixed)

-- | The format the job will write in. The files the job touches decide it; the rest of the
-- recorded hashes decide it only when those files carry none; a preference decides it when nothing
-- else does.
--
-- >>> settleFormat Map.empty Set.empty
-- Right (JobFormat XXH64)
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = MD5, value = "0f"})]) (Set.fromList [RelPath "a.mxf"])
-- Right (JobFormat MD5)
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = SHA1, value = "ab"})]) (Set.fromList [RelPath "b.mxf"])
-- Right (JobFormat SHA1)
-- >>> settleFormat (Map.fromList [(RelPath "a.mxf", Hash {algo = MD5, value = "0f"}), (RelPath "b.mxf", Hash {algo = SHA1, value = "ab"})]) (Set.fromList [RelPath "a.mxf", RelPath "b.mxf"])
-- Left (MixedFormats [MD5,SHA1])
settleFormat :: Map RelPath Hash -> Set RelPath -> Either FormatError JobFormat
settleFormat expected present = case (algosOf (Map.restrictKeys expected present), algosOf expected) of
  ([algo], _) -> Right (JobFormat algo)
  (mixed@(_ : _ : _), _) -> Left (MixedFormats mixed)
  ([], [algo]) -> Right (JobFormat algo)
  -- No file this job will touch carries a recorded hash, so a preference exchanges no media source's format.
  ([], _) -> Right (JobFormat preferredAlgo)

mixedMessage :: List HashAlgo -> Text
mixedMessage mixed =
  "ASC MHL: the originals hold more than one hash format: " <> T.intercalate ", " (map (\algo -> display algo) mixed)

algosOf :: Map RelPath Hash -> List HashAlgo
algosOf hashes = hashes & Map.elems & map (\h -> h.algo) & Set.fromList & Set.toList
