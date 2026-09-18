module Ascmhl.ManifestEntry
  ( ManifestEntry (..)
  , files
  , ordered
  ) where

import Data.Function ((&))
import Data.List (List, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V

import Ascmhl.DirectoryEntry (DirectoryEntry)
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry (FileEntry)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath

data ManifestEntry = File FileEntry | Directory DirectoryEntry
  deriving stock (Eq, Show)

files :: Vector ManifestEntry -> Vector FileEntry
files entries =
  V.mapMaybe
    ( \case
        File fe -> Just fe
        Directory _ -> Nothing
    )
    entries

ordered :: Vector FileEntry -> Vector DirectoryEntry -> Vector ManifestEntry
ordered fileList dirs = V.fromList (below root)
  where
    root = RelPath.RelPath ""
    dirPaths :: Set RelPath
    dirPaths = dirs & V.toList & map (.path) & Set.fromList
    homeOf :: RelPath -> RelPath
    homeOf path = let p = RelPath.parent path in if p == root || Set.member p dirPaths then p else root
    dirsByParent :: Map RelPath (List DirectoryEntry)
    dirsByParent = dirs & V.toList & filter (\d -> d.path /= root) & sortOn (.path) & groupOn (\d -> homeOf d.path)
    filesByParent :: Map RelPath (List FileEntry)
    filesByParent = fileList & V.toList & sortOn (.path) & groupOn (\e -> homeOf e.path)
    below :: RelPath -> List ManifestEntry
    below here =
      concatMap subtree (Map.findWithDefault [] here dirsByParent)
        <> map File (Map.findWithDefault [] here filesByParent)
    subtree :: DirectoryEntry -> List ManifestEntry
    subtree d = below d.path <> [Directory d]

groupOn :: (a -> RelPath) -> List a -> Map RelPath (List a)
groupOn key xs = foldr (\x acc -> Map.insertWith (<>) (key x) [x] acc) Map.empty xs
