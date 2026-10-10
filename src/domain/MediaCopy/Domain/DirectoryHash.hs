module MediaCopy.Domain.DirectoryHash
  ( DirNode (..)
  , DirHashes (..)
  , DirectoryHashError (..)
  , buildTree
  , directoryHashes
  ) where

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..), baseName, parentOf)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (List, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Display (Display (..))
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
    groupByParent :: List (RelPath, b) -> Map RelPath (List b)
    groupByParent keyed =
      keyed
        & map (\pair -> (parentOf (fst pair), [snd pair]))
        & Map.fromListWith (flip (<>))
    filesByParent :: Map RelPath (List (Text, Hash))
    filesByParent =
      files
        & V.toList
        & map (\entry -> (fst entry, first baseName entry))
        & groupByParent
    dirsByParent :: Map RelPath (List RelPath)
    dirsByParent =
      dirs
        & V.toList
        & map (\dir -> (dir, dir))
        & groupByParent
    nodeFor rp nm =
      DirNode
        { path = rp
        , name = nm
        , files =
            Map.findWithDefault [] rp filesByParent
              & sortOn fst
              & V.fromList
        , subdirs =
            Map.findWithDefault [] rp dirsByParent
              & sort
              & map (\child -> nodeFor child (baseName child))
              & V.fromList
        }

hashOfHashList :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Vector Hash -> Eff es Hash
hashOfHashList hashWith hashes = do
  let sorted = hashes & V.toList & sortOn (.value)
  chunks <- traverse requireBytes sorted
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
  subResults <- traverse (directoryHashes hashWith) (V.toList node.subdirs)
  let paired = zip (V.toList node.subdirs) subResults
      childRows = V.concat (map (\(sub, result) -> snd result <> V.singleton (sub.path, fst result)) paired)
      childContents = V.fromList (map (\(_, result) -> (fst result).content) paired)
      fileHashes = V.map snd node.files
  contentHash <- hashOfHashList hashWith (childContents <> fileHashes)
  fileStructures <- traverse (\entry -> perChild hashWith (fst entry) (snd entry)) (V.toList node.files)
  dirStructures <- traverse (\(sub, result) -> perChild hashWith sub.name (fst result).structure) paired
  structureHash <- hashOfHashList hashWith (V.fromList (fileStructures <> dirStructures))
  let here = DirHashes {content = contentHash, structure = structureHash}
  pure (here, childRows)
