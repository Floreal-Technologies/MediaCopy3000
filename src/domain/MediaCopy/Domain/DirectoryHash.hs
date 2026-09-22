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

newtype DirectoryHashError = HashUndecodableValue Text
  deriving stock (Eq, Show)

instance Display DirectoryHashError where
  displayBuilder (HashUndecodableValue value) =
    "ASC MHL: undecodable hash value: " <> displayBuilder value

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

hashOfHashList :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Vector Hash -> Eff es Hash
hashOfHashList hashWith hashes = do
  let sorted = hashes & V.toList & sortOn (\h -> h.value)
  chunks <- traverse (\h -> requireBytes h) sorted
  hashWith (BS.concat chunks)

requireBytes :: (Error DirectoryHashError :> es) => Hash -> Eff es ByteString
requireBytes h = case digestBytes h of
  Nothing -> throwError (HashUndecodableValue h.value)
  Just bytes -> pure bytes

perChild :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Text -> Hash -> Eff es Hash
perChild hashWith childName h = do
  bytes <- requireBytes h
  hashWith (TE.encodeUtf8 childName <> bytes)

directoryHashes
  :: (Error DirectoryHashError :> es)
  => (ByteString -> Eff es Hash)
  -> DirNode
  -> Eff es (DirHashes, Vector (RelPath, DirHashes))
directoryHashes hashWith node = do
  subResults <- traverse (\sub -> directoryHashes hashWith sub) (V.toList node.subdirs)
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
