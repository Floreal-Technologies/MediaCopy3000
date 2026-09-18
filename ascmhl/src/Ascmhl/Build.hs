-- | The builders that assemble a manifest and a chain.
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

-- | Who wrote a generation. The tool version is optional in the schema, and this project always has one.
creatorInfo :: UTCTime -> Text -> Text -> Text -> CreatorInfo
creatorInfo creationDate hostname toolName toolVersion =
  CreatorInfo
    { creationDate
    , hostname
    , toolName
    , toolVersion = Just toolVersion
    , unknown = V.empty
    }

-- | A directory's pair of hashes, dated. A directory hash never carries an action.
dirHash :: UTCTime -> Hash -> Hash -> DirHash
dirHash hashDate content structure =
  DirHash {content, structure, hashDate = Just hashDate, extraAttrs = Map.empty}

-- | One file's row: what it is, and what the job did with it.
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

-- | One directory's row. A row this project writes holds exactly one pair.
directoryEntry :: RelPath -> UTCTime -> DirHash -> DirectoryEntry
directoryEntry path lastModified pair =
  DirectoryEntry
    { path
    , lastModified
    , hashes = V.singleton pair
    , pathAttrs = Map.empty
    , unknown = V.empty
    }

-- | A manifest this project writes. The root pair belongs in @\<roothash\>@, so it is one argument
-- and never an entry.
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

-- | @HashlistType@ is the sequence path, c4. An entry this function builds has no other
-- child, so a legacy entry's extra children never survive a rewrite.
chainEntry :: Int -> RelPath -> Maybe Hash -> ChainEntry
chainEntry sequenceNr path c4 = ChainEntry {sequenceNr, path, c4, unknown = V.empty}

-- | The chain with one more generation at the end. The result is in sequence order, whatever order
-- the given chain held.
appendGeneration :: Chain -> Int -> RelPath -> Hash -> Chain
appendGeneration chain number name c4 =
  Chain {entries = V.fromList (orderedChainEntries chain <> [chainEntry number name (Just c4)])}

-- | The chain that a folder with manifests but no chain file implies. The caller passes the
-- manifest names, already filtered and sorted by 'Ascmhl.Layout.mhlFileNames'. A name with no
-- leading number keeps its listing position. No entry gets a c4, because this reads no manifest.
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

-- | §6.5 order: for each directory, its subdirectories in turn, then its own files, then the
-- directory itself. The root has no row, because its pair is the @\<roothash\>@. The order comes
-- from the paths alone: each row is filed under its parent, siblings sort by path, and a directory
-- row follows everything below it. A row whose parent has no row of its own is filed under the
-- root, so 'requirePlaced' still finds every file.
orderedEntries :: Vector HashEntry -> Vector DirectoryEntry -> Vector ManifestEntry
orderedEntries files dirs = V.fromList (below root)
  where
    root = RelPath ""
    dirPaths :: Set RelPath
    dirPaths = dirs & V.toList & map (\d -> d.path) & Set.fromList
    -- The row's parent when that parent has a row, and the root when it has none.
    homeOf :: RelPath -> RelPath
    homeOf path = let p = parentOf path in if p == root || Set.member p dirPaths then p else root
    -- A row for the root itself is refused here, not filed: its home would be itself, and the
    -- recursion below would never end. The root's pair belongs in the roothash, never in a row.
    dirsByParent :: Map RelPath (List DirectoryEntry)
    dirsByParent = dirs & V.toList & filter (\d -> d.path /= root) & sortOn (\d -> d.path) & groupOn (\d -> homeOf d.path)
    filesByParent :: Map RelPath (List HashEntry)
    filesByParent = files & V.toList & sortOn (\e -> e.path) & groupOn (\e -> homeOf e.path)
    -- Every row filed under this directory. Recursion ends because a child's path is strictly
    -- longer than its parent's, once the root row is gone.
    below :: RelPath -> List ManifestEntry
    below here =
      concatMap subtree (Map.findWithDefault [] here dirsByParent)
        <> map (\e -> ManifestFile e) (Map.findWithDefault [] here filesByParent)
    subtree :: DirectoryEntry -> List ManifestEntry
    subtree d = below d.path <> [ManifestDir d]

-- | Keeps the order of the given list inside each group, so one sort before the call orders every group.
groupOn :: (a -> RelPath) -> List a -> Map RelPath (List a)
groupOn key xs = foldr (\x acc -> Map.insertWith (<>) (key x) [x] acc) Map.empty xs

parentOf :: RelPath -> RelPath
parentOf (RelPath t) = case T.breakOnEnd "/" t of
  (before, _) -> RelPath (T.dropEnd 1 before)
