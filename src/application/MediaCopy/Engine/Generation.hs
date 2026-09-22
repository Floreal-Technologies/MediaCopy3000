module MediaCopy.Engine.Generation
  ( writeGeneration
  , requireNextGeneration
  ) where

import Ascmhl.Build (appendGeneration, dirHash, directoryEntry, newManifest, orderedEntries)
import Ascmhl.Hash
import Ascmhl.Layout
import Ascmhl.Path (mkRelPath, pathText, relToOsPath)
import Ascmhl.Types
import Ascmhl.Write (renderChain, renderManifest)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (Reader, ask)
import System.OsPath (OsPath, takeFileName)

import MediaCopy.Domain.DirectoryHash
import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (JobFormat, chainFormat)
import MediaCopy.Domain.Plan (PlannedGeneration (..))
import MediaCopy.Effects.Emit
import MediaCopy.Effects.FileSystem
import MediaCopy.Effects.Hasher
import MediaCopy.Engine.Violation (PlanViolation (..), orThrow)
import MediaCopy.Mhl.Store

writeGeneration
  :: (FileSystem :> es, Hasher :> es, Emit :> es, Error PlanViolation :> es, Reader JobFormat :> es, Reader CreatorInfo :> es)
  => PlannedGeneration
  -> Vector Text
  -> Vector HashEntry
  -> Eff es ()
writeGeneration planned patterns files = do
  creator <- ask @CreatorInfo
  let t = creator.creationDate
  found <- loadChain planned.folder >>= orThrow . first (HistoryFaultAt planned.folder)
  let chain = fromMaybe (Chain {entries = V.empty}) found
  orThrow (requireNextGeneration planned.folder planned.number chain)
  hashedFiles <- traverse (\e -> orThrow (firstHash e) <&> \h -> (e.path, h)) files
  let tree = buildTree hashedFiles planned.directories
  fmt <- ask @JobFormat
  rolled <- runErrorNoCallStack @DirectoryHashError (directoryHashes (hashBytes fmt) tree)
  (rootPair, rows) <- orThrow (first (\err -> HashUndecodable err) rolled)
  let dirEntry (path, pair) = do
        mtime <- mtimeOf (relToOsPath planned.folder path)
        pure (directoryEntry path mtime (dirHash t pair.content pair.structure))
  dirRows <- traverse dirEntry rows
  let entries = orderedEntries files dirRows
  orThrow (requirePlaced files entries)
  let manifest =
        newManifest
          creator
          planned.process
          (dirHash t rootPair.content rootPair.structure)
          patterns
          entries
      txt = renderManifest manifest
      bytes = TE.encodeUtf8 txt
  writeTextAtomically planned.manifest txt
  mh <- hashBytes chainFormat bytes
  nameRel <- orThrow (maybe (Left (ManifestNameUnusable planned.manifest)) Right (mkRelPath (pathText (takeFileName planned.manifest))))
  backfilled <-
    traverse
      (\e -> backfillChainEntry planned.folder e >>= orThrow . first (HistoryFaultAt planned.folder))
      (orderedChainEntries chain)
  let chain' = appendGeneration Chain {entries = V.fromList backfilled} planned.number nameRel mh
  writeTextAtomically (chainPath planned.folder) (renderChain chain')
  emit (MhlWritten planned.manifest)

requireNextGeneration :: OsPath -> Int -> Chain -> Either PlanViolation ()
requireNextGeneration folder number chain
  | found == number = Right ()
  | otherwise = Left (GenerationRaced folder number found)
  where
    found = 1 + highestGeneration chain

firstHash :: HashEntry -> Either PlanViolation Hash
firstHash e = case e.hashes V.!? 0 of
  Just mh -> Right mh.hash
  Nothing -> Left (EntryWithoutHash e.path)

requirePlaced :: Vector HashEntry -> Vector ManifestEntry -> Either PlanViolation ()
requirePlaced files entries = case missing of
  [] -> Right ()
  paths -> Left (EntriesNotPlaced (V.fromList paths))
  where
    placed = Set.fromList (V.toList (V.map (\e -> e.path) (fileEntries entries)))
    missing =
      files
        & V.toList
        & map (\e -> e.path)
        & filter (\p -> not (Set.member p placed))
