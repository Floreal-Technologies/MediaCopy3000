-- | One generation: the manifest that describes a tree, and the chain entry that names it.
module MediaCopy.Engine.Generation
  ( writeGeneration
  , requireNextGeneration
  ) where

import Ascmhl.Build (appendGeneration, creatorInfo, dirHash, directoryEntry, newManifest, orderedEntries)
import Ascmhl.Hash
import Ascmhl.Layout
import Ascmhl.Path (RelPath, mkRelPath, pathText, relToOsPath)
import Ascmhl.Types
import Ascmhl.Write (renderChain, renderManifest)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
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
import MediaCopy.Engine.Config
import MediaCopy.Engine.Violation (PlanViolation (..), orThrow)
import MediaCopy.Mhl.Store

-- | A manifest describes the tree it sits inside, so the rows come from
-- 'planned.directories' and from no other generation's set.
writeGeneration
  :: (FileSystem :> es, Hasher :> es, Emit :> es, Error PlanViolation :> es, Reader JobFormat :> es, Reader ToolInfo :> es, Reader JobInstant :> es)
  => PlannedGeneration
  -> Vector Text
  -> Vector HashEntry
  -> Eff es ()
writeGeneration planned patterns files = do
  cfg <- ask @ToolInfo
  found <- loadChain planned.folder >>= orThrow . first (HistoryFaultAt planned.folder)
  let chain = fromMaybe (Chain {entries = V.empty}) found
  orThrow (requireNextGeneration planned.folder planned.number chain)
  JobInstant t <- ask @JobInstant
  hashedFiles <- traverse (\e -> orThrow (firstHash e) <&> \h -> (e.path, h)) files
  let tree = buildTree hashedFiles planned.directories
  fmt <- ask @JobFormat
  rolled <- runErrorNoCallStack @DirectoryHashError (directoryHashes (hashBytes fmt) tree)
  (rootPair, rows) <- orThrow (first (\err -> HashUndecodable err) rolled)
  dirRows <- traverse (\row -> dirEntryFor planned.folder t row) rows
  let entries = orderedEntries files dirRows
  orThrow (requirePlaced files entries)
  let manifest =
        newManifest
          (creatorInfo t cfg.hostname cfg.toolName cfg.toolVersion)
          planned.process
          (dirHash t rootPair.content rootPair.structure)
          patterns
          entries
      txt = renderManifest manifest
      bytes = TE.encodeUtf8 txt
  -- The write makes @ascmhl\/@ on its way, so the manifest is the first thing that needs the folder.
  -- The write is atomic, so no reader ever sees a half-written generation.
  writeTextAtomically planned.manifest txt
  mh <- hashBytes chainFormat bytes
  nameRel <- orThrow (maybe (Left (ManifestNameUnusable planned.manifest)) Right (mkRelPath (pathText (takeFileName planned.manifest))))
  backfilled <-
    traverse
      (\e -> backfillChainEntry planned.folder e >>= orThrow . first (HistoryFaultAt planned.folder))
      (orderedChainEntries chain)
  let chain' = appendGeneration Chain {entries = V.fromList backfilled} planned.number nameRel mh
  -- The chain follows the manifest, so a chain never names a manifest that is not there yet.
  writeTextAtomically (chainPath planned.folder) (renderChain chain')
  emit (MhlWritten planned.manifest)

dirEntryFor :: (FileSystem :> es) => OsPath -> UTCTime -> (RelPath, DirHashes) -> Eff es DirectoryEntry
dirEntryFor root t (path, pair) = do
  mtime <- mtimeOf (relToOsPath root path)
  pure (directoryEntry path mtime (dirHash t pair.content pair.structure))

-- | The same check for a folder and the number its chain must be about to give. The plan named the
-- generation the operator approved, so a history that moved since then fails the job rather than
-- writes a manifest nobody saw.
requireNextGeneration :: OsPath -> Int -> Chain -> Either PlanViolation ()
requireNextGeneration folder number chain
  | found == number = Right ()
  | otherwise = Left (GenerationRaced folder number found)
  where
    found = 1 + highestGeneration chain

-- | Every manifest entry the engine builds carries exactly one hash.
firstHash :: HashEntry -> Either PlanViolation Hash
firstHash e = case e.hashes V.!? 0 of
  Just mh -> Right mh.hash
  Nothing -> Left (EntryWithoutHash e.path)

-- | The recorded entries are the job's own account of itself, so ordering them must never drop
-- one. 'orderedEntries' places every file, so this costs nothing and still names the fault if that
-- stops being true.
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
