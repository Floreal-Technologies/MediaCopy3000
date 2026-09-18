module MediaCopy.Domain.DirectoryHash
  ( DirectoryHash (..)
  , Node (..)
  , DirectoryHashError (..)
  , buildTree
  , compute
  ) where

import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful.Error.Static (Error, throwError)

newtype DirectoryHashError = HashUndecodableValue Text
  deriving stock (Eq, Show)

instance Display DirectoryHashError where
  displayBuilder (HashUndecodableValue value) =
    "ASC MHL: undecodable hash value: " <> displayBuilder value

data Node = Node
  { path :: RelPath
  , name :: Text
  , files :: Vector (Text, Hash)
  , subdirs :: Vector Node
  }
  deriving stock (Eq, Show)

data DirectoryHash = DirectoryHash
  { content :: Hash
  , structure :: Hash
  }
  deriving stock (Eq, Show)

buildTree :: Vector (RelPath, Hash) -> Vector RelPath -> Node
buildTree files dirs = nodeFor (RelPath.RelPath "") ""
  where
    groupByParent :: List (RelPath, b) -> Map RelPath (List b)
    groupByParent keyed =
      keyed
        & map (\pair -> (RelPath.parent (fst pair), [snd pair]))
        & Map.fromListWith (flip (<>))
    filesByParent :: Map RelPath (List (Text, Hash))
    filesByParent =
      files
        & V.toList
        & map (\entry -> (fst entry, first RelPath.baseName entry))
        & groupByParent
    dirsByParent :: Map RelPath (List RelPath)
    dirsByParent =
      dirs
        & V.toList
        & map (\dir -> (dir, dir))
        & groupByParent
    nodeFor rp nm =
      Node
        { path = rp
        , name = nm
        , files =
            Map.findWithDefault [] rp filesByParent
              & sortOn fst
              & V.fromList
        , subdirs =
            Map.findWithDefault [] rp dirsByParent
              & sort
              & map (\child -> nodeFor child (RelPath.baseName child))
              & V.fromList
        }

hashOfHashList :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Vector Hash -> Eff es Hash
hashOfHashList hashWith hashes = do
  let sorted = hashes & V.toList & sortOn (.value)
  chunks <- traverse requireBytes sorted
  hashWith (BS.concat chunks)

requireBytes :: (Error DirectoryHashError :> es) => Hash -> Eff es ByteString
requireBytes h = case Hash.digest h of
  Nothing -> throwError (HashUndecodableValue h.value)
  Just bytes -> pure bytes

perChild :: (Error DirectoryHashError :> es) => (ByteString -> Eff es Hash) -> Text -> Hash -> Eff es Hash
perChild hashWith childName h = do
  bytes <- requireBytes h
  hashWith (TE.encodeUtf8 childName <> bytes)

compute
  :: (Error DirectoryHashError :> es)
  => (ByteString -> Eff es Hash)
  -> Node
  -> Eff es (DirectoryHash, Vector (RelPath, DirectoryHash))
compute hashWith node = do
  subResults <- traverse (compute hashWith) (V.toList node.subdirs)
  let paired = zip (V.toList node.subdirs) subResults
      childRows = V.concat (map (\(sub, result) -> snd result <> V.singleton (sub.path, fst result)) paired)
      childContents = V.fromList (map (\(_, result) -> (fst result).content) paired)
      fileHashes = V.map snd node.files
  contentHash <- hashOfHashList hashWith (childContents <> fileHashes)
  fileStructures <- traverse (\entry -> perChild hashWith (fst entry) (snd entry)) (V.toList node.files)
  dirStructures <- traverse (\(sub, result) -> perChild hashWith sub.name (fst result).structure) paired
  structureHash <- hashOfHashList hashWith (V.fromList (fileStructures <> dirStructures))
  let here = DirectoryHash {content = contentHash, structure = structureHash}
  pure (here, childRows)
