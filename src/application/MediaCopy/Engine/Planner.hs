module MediaCopy.Engine.Planner
  ( plan
  ) where

import Data.Foldable (toList)
import Data.Vector qualified as V

import MediaCopy.Domain.Destination (Destination)
import MediaCopy.Domain.Destination qualified as Destination
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload (Offload)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Preflight qualified as Preflight
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Mhl.Store qualified as Store

plan :: (FileSystem :> es) => JobSpec -> Eff es Plan
plan spec = case spec.job of
  Job.Offload oj -> gatherOffload oj <&> Preflight.offload spec oj
  Job.VerifyFolder vj -> gatherGeneration vj.folder <&> Preflight.generation spec Preflight.RequireHistory
  Job.SealMediaSource sj -> gatherGeneration sj.folder <&> Preflight.generation spec Preflight.AllowFresh

gatherOffload :: (FileSystem :> es) => Offload -> Eff es Preflight.OffloadFacts
gatherOffload job = do
  sourceTree <- FileSystem.walk job.source
  targets <- V.mapM (gatherTarget job.source) (V.fromList (toList job.destinations))
  originals <- Store.resolveOriginals job.source
  sourceHistory <- Store.historyHashes job.source
  pure Preflight.OffloadFacts {sourceTree, targets, originals, sourceHistory}

gatherTarget :: (FileSystem :> es) => OsPath -> OsPath -> Eff es Destination
gatherTarget source parent = do
  let root = Destination.folderFor parent source
  free <- FileSystem.freeSpaceOf parent
  history <- Store.historyHashes root <&> fmap (fmap fst)
  existing <- FileSystem.walk root
  pure
    Destination.Destination
      { root
      , freeBytes = free
      , history
      , existing
      }

gatherGeneration :: (FileSystem :> es) => OsPath -> Eff es Preflight.GenerationFacts
gatherGeneration folder = do
  tree <- FileSystem.walk folder
  freeBytes <- maybe (pure Nothing) (\_ -> FileSystem.freeSpaceOf folder) tree
  history <- Store.historyHashes folder
  pure Preflight.GenerationFacts {tree, freeBytes, history}
