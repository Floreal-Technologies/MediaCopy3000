module MediaCopy.Domain.DirectoryHash
  ( DirNode (..)
  , DirHashes (..)
  , DirectoryHashError (..)
  , buildTree
  , directoryHashes
  ) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (List, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (Error, throwError)

-- | The one fault the rollup can meet: a hash value that does not decode under its own format.
newtype DirectoryHashError = HashUndecodableValue Text
  deriving stock (Eq, Show)

instance Display DirectoryHashError where
  displayBuilder (HashUndecodableValue value) =
    "ASC MHL: undecodable hash value: " <> displayBuilder value

-- | The root carries the empty path and the empty name.
data DirNode = DirNode
  { path :: RelPath
  , name :: Text
  , files :: Vector (Text, Hash)
  , subdirs :: Vector DirNode
  }
  deriving stock (Eq, Show)

data DirHashes = DirHashes
  { content :: Hash
  , structure :: Hash
  }
  deriving stock (Eq, Show)

buildTree :: Vector (RelPath, Hash) -> Vector RelPath -> DirNode
buildTree files dirs = nodeFor (RelPath "") ""
  where
    -- The append order decides sibling order before the sort, so both groupings must agree on it.
    groupByParent :: List (Text, b) -> Map Text (List b)
    groupByParent keyed =
      keyed
        & map (\pair -> (parentOf (fst pair), [snd pair]))
        & Map.fromListWith (\new old -> old <> new)
    filesByParent :: Map Text (List (Text, Hash))
    filesByParent =
      files
        & V.toList
        & map (\entry -> let full = display (fst entry) in (full, (baseOf full, snd entry)))
        & groupByParent
    dirsByParent :: Map Text (List Text)
    dirsByParent =
      dirs
        & V.toList
        & map (\dir -> let full = display dir in (full, full))
        & groupByParent
    nodeFor rp nm =
      DirNode
        { path = rp
        , name = nm
        , files =
            Map.findWithDefault [] (display rp) filesByParent
              & sortOn (\entry -> fst entry)
              & V.fromList
        , subdirs =
            Map.findWithDefault [] (display rp) dirsByParent
              & sort
              & map (\child -> nodeFor (RelPath child) (baseOf child))
              & V.fromList
        }
    parentOf p = case T.breakOnEnd "/" p of
      (before, _) -> T.dropEnd 1 before
    baseOf p = case T.breakOnEnd "/" p of
      (_, after) -> after

-- | Appendix G: the function sorts the hash strings, decodes each one, and hashes the concatenation.
hashOfHashList :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Vector Hash -> Eff es Hash
hashOfHashList hashWith hashes = do
  let sorted = hashes & V.toList & sortOn (\h -> h.value)
  chunks <- traverse (\h -> requireBytes h) sorted
  hashWith (BS.concat chunks)

-- | A hash decodes under its own format. The domain reports a value that does not; it
-- throws nothing, because the domain runs no IO.
requireBytes :: (Error DirectoryHashError :> es) => Hash -> Eff es ByteString
requireBytes h = case digestBytes h of
  Nothing -> throwError (HashUndecodableValue h.value)
  Just bytes -> pure bytes

-- | A child's name bytes, then its hash bytes, with no separator.
perChild :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Text -> Hash -> Eff es Hash
perChild hashWith childName h = do
  bytes <- requireBytes h
  hashWith (TE.encodeUtf8 childName <> bytes)

-- | Each strict descendant directory gives one row, in post-order, and the root gives none.
directoryHashes
  :: (Error DirectoryHashError :> es)
  => (ByteString -> Eff es Hash)
  -> DirNode
  -> Eff es (DirHashes, Vector (RelPath, DirHashes))
directoryHashes hashWith node = do
  subResults <- traverse (\sub -> directoryHashes hashWith sub) (V.toList node.subdirs)
  -- This line pairs a subdirectory and its result once, so no later step can pair them
  -- differently.
  let paired = zip (V.toList node.subdirs) subResults
      childRows = V.concat (map (\(sub, result) -> snd result <> V.singleton (sub.path, fst result)) paired)
      childContents = V.fromList (map (\(_, result) -> (fst result).content) paired)
      fileHashes = V.map (\entry -> snd entry) node.files
  contentHash <- hashOfHashList hashWith (childContents <> fileHashes)
  fileStructures <- traverse (\entry -> perChild hashWith (fst entry) (snd entry)) (V.toList node.files)
  dirStructures <- traverse (\(sub, result) -> perChild hashWith sub.name (fst result).structure) paired
  structureHash <- hashOfHashList hashWith (V.fromList (fileStructures <> dirStructures))
  let here = DirHashes {content = contentHash, structure = structureHash}
  pure (here, childRows)
