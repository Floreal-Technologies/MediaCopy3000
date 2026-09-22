module Ascmhl.Build
  ( creatorInfo
  , dirHash
  , fileEntry
  , directoryEntry
  , newManifest
  , appendGeneration
  , chainEntry
  , chainFromListing
  , orderedEntries
  ) where

import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath)

import Ascmhl.Hash (Hash)
import Ascmhl.Layout (sequenceOf)
import Ascmhl.Path (RelPath (..), mkRelPath, pathText)
import Ascmhl.Types

creatorInfo :: UTCTime -> Text -> Text -> Text -> CreatorInfo
creatorInfo creationDate hostname toolName toolVersion =
  CreatorInfo
    { creationDate
    , hostname
    , toolName
    , toolVersion = Just toolVersion
    , unknown = V.empty
    }

dirHash :: UTCTime -> Hash -> Hash -> DirHash
dirHash hashDate content structure =
  DirHash {content, structure, hashDate = Just hashDate, extraAttrs = Map.empty}

fileEntry :: RelPath -> Int64 -> UTCTime -> Hash -> HashAction -> UTCTime -> HashEntry
fileEntry path size lastModified hash action hashDate =
  HashEntry
    { path
    , size
    , lastModified
    , hashes = V.singleton ManifestHash {hash, action, hashDate = Just hashDate, extraAttrs = Map.empty}
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

directoryEntry :: RelPath -> UTCTime -> DirHash -> DirectoryEntry
directoryEntry path lastModified pair =
  DirectoryEntry
    { path
    , lastModified
    , hashes = V.singleton pair
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

newManifest :: CreatorInfo -> ProcessKind -> DirHash -> Vector Text -> Vector ManifestEntry -> Manifest
newManifest creator process rootPair ignorePatterns entries =
  Manifest
    { creator
    , process
    , rootHash = V.singleton rootPair
    , ignorePatterns
    , entries
    , unknown = V.empty
    }

chainEntry :: Int -> RelPath -> Maybe Hash -> ChainEntry
chainEntry sequenceNr path c4 = ChainEntry {sequenceNr, path, c4, unknown = V.empty}

appendGeneration :: Chain -> Int -> RelPath -> Hash -> Chain
appendGeneration chain number name c4 =
  Chain {entries = V.fromList (orderedChainEntries chain <> [chainEntry number name (Just c4)])}

chainFromListing :: Vector OsPath -> Chain
chainFromListing names =
  names
    & V.toList
    & mapMaybe (\name -> mkRelPath (pathText name))
    & zipWith entryFor [1 ..]
    & V.fromList
    & Chain
  where
    entryFor index p = chainEntry (sequenceOf index p) p Nothing

orderedEntries :: Vector HashEntry -> Vector DirectoryEntry -> Vector ManifestEntry
orderedEntries files dirs = V.fromList (below root)
  where
    root = RelPath ""
    dirPaths :: Set RelPath
    dirPaths = dirs & V.toList & map (\d -> d.path) & Set.fromList
    homeOf :: RelPath -> RelPath
    homeOf path = let p = parentOf path in if p == root || Set.member p dirPaths then p else root
    dirsByParent :: Map RelPath (List DirectoryEntry)
    dirsByParent = dirs & V.toList & filter (\d -> d.path /= root) & sortOn (\d -> d.path) & groupOn (\d -> homeOf d.path)
    filesByParent :: Map RelPath (List HashEntry)
    filesByParent = files & V.toList & sortOn (\e -> e.path) & groupOn (\e -> homeOf e.path)
    below :: RelPath -> List ManifestEntry
    below here =
      concatMap subtree (Map.findWithDefault [] here dirsByParent)
        <> map (\e -> ManifestFile e) (Map.findWithDefault [] here filesByParent)
    subtree :: DirectoryEntry -> List ManifestEntry
    subtree d = below d.path <> [ManifestDir d]

groupOn :: (a -> RelPath) -> List a -> Map RelPath (List a)
groupOn key xs = foldr (\x acc -> Map.insertWith (<>) (key x) [x] acc) Map.empty xs

parentOf :: RelPath -> RelPath
parentOf (RelPath t) = case T.breakOnEnd "/" t of
  (before, _) -> RelPath (T.dropEnd 1 before)
