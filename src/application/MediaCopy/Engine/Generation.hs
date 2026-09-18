module MediaCopy.Engine.Generation
  ( write
  , require
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo
import Ascmhl.Chain (Chain)
import Ascmhl.Chain qualified as Chain
import Ascmhl.Creator (Creator)
import Ascmhl.Creator qualified as Creator
import Ascmhl.DirHash qualified as DirHash
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry (FileEntry)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Layout qualified as Layout
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry (ManifestEntry)
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (Reader, ask)
import System.OsPath (takeFileName)

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.DirectoryHash (DirectoryHashError)
import MediaCopy.Domain.DirectoryHash qualified as DirectoryHash
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.PlannedGeneration (PlannedGeneration)
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Effects.Emit (Emit)
import MediaCopy.Effects.Emit qualified as Emit
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Effects.Hasher (Hasher)
import MediaCopy.Effects.Hasher qualified as Hasher
import MediaCopy.Engine.PlanViolation (PlanViolation)
import MediaCopy.Engine.PlanViolation qualified as PlanViolation
import MediaCopy.Mhl.Store qualified as Store

require :: (FileSystem :> es, Error PlanViolation :> es) => OsPath -> Int -> Eff es Chain
require folder number = do
  found <- Store.loadChain folder >>= PlanViolation.orThrow . first (PlanViolation.HistoryFaultAt folder)
  let chain = fromMaybe (Chain.Chain {entries = V.empty}) found
  PlanViolation.orThrow (requireNextGeneration folder number chain)
  pure chain

write
  :: (FileSystem :> es, Hasher :> es, Emit :> es, Error PlanViolation :> es, Reader Algo :> es, Reader Creator :> es, Reader Contributions :> es)
  => PlannedGeneration
  -> Vector Text
  -> Vector FileEntry
  -> Eff es ()
write planned patterns files = do
  creator <- ask @Creator
  let t = creator.creationDate
  chain <- require planned.folder planned.number
  hashedFiles <- traverse (\e -> PlanViolation.orThrow (firstHash e) <&> \h -> (e.path, h)) files
  let tree = DirectoryHash.buildTree hashedFiles planned.directories
  fmt <- ask @Algo
  rolled <- runErrorNoCallStack @DirectoryHashError (DirectoryHash.compute (Hasher.hashBytes fmt) tree)
  (rootPair, rows) <- PlanViolation.orThrow (first PlanViolation.HashUndecodable rolled)
  let dirEntry (path, pair) = do
        mtime <- FileSystem.mtimeOf (RelPath.toOsPath planned.folder path)
        pure (DirectoryEntry.new path mtime (DirHash.new t pair.content pair.structure))
  dirRows <- traverse dirEntry rows
  contributions <- ask @Contributions
  let described = V.map (\e -> maybe e (\fragment -> FileEntry.withMetadata fragment e) (Map.lookup e.path contributions.fileMetadata)) files
      entries = ManifestEntry.ordered described dirRows
  PlanViolation.orThrow (requirePlaced files entries)
  let manifest =
        maybe id Manifest.withMetadata contributions.manifestMetadata $
          Manifest.new
            creator
            planned.process
            (DirHash.new t rootPair.content rootPair.structure)
            patterns
            entries
      txt = Manifest.render manifest
      bytes = TE.encodeUtf8 txt
  FileSystem.writeTextAtomically planned.manifest txt
  mh <- Hasher.hashBytes Algo.C4 bytes
  nameRel <- PlanViolation.orThrow (maybe (Left (PlanViolation.ManifestNameUnusable planned.manifest)) Right (RelPath.parse (OsPath.toText (takeFileName planned.manifest))))
  backfilled <-
    traverse
      (\e -> Store.backfillChainEntry planned.folder e >>= PlanViolation.orThrow . first (PlanViolation.HistoryFaultAt planned.folder))
      (Chain.inOrder chain)
  let chain' = Chain.append Chain.Chain {entries = V.fromList backfilled} planned.number nameRel mh
  FileSystem.writeTextAtomically (Layout.chainFile planned.folder) (Chain.render chain')
  Emit.event (JobEvent.MhlWritten planned.manifest)

requireNextGeneration :: OsPath -> Int -> Chain -> Either PlanViolation ()
requireNextGeneration folder number chain
  | found == number = Right ()
  | otherwise = Left (PlanViolation.GenerationRaced folder number found)
  where
    found = 1 + Chain.highestGeneration chain

firstHash :: FileEntry -> Either PlanViolation Hash
firstHash e = case e.hashes V.!? 0 of
  Just mh -> Right mh.hash
  Nothing -> Left (PlanViolation.EntryWithoutHash e.path)

requirePlaced :: Vector FileEntry -> Vector ManifestEntry -> Either PlanViolation ()
requirePlaced files entries = case missing of
  [] -> Right ()
  paths -> Left (PlanViolation.EntriesNotPlaced (V.fromList paths))
  where
    placed = Set.fromList (V.toList (V.map (.path) (ManifestEntry.files entries)))
    missing =
      files
        & V.toList
        & map (.path)
        & filter (\p -> not (Set.member p placed))
