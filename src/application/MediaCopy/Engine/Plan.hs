-- | Reads what a plan needs, and hands it to the domain. Every decision this module used to make
-- lives in 'MediaCopy.Domain.Preflight', so a plan can be checked without a disk.
module MediaCopy.Engine.Plan
  ( planJob
  ) where

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Vector qualified as V
import Effectful
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan)
import MediaCopy.Domain.Preflight
import MediaCopy.Effects.FileSystem (FileSystem, freeSpaceOf, walk)
import MediaCopy.Mhl.Store (historyHashes, resolveOriginals)

planJob :: (FileSystem :> es) => JobSpec -> Eff es JobPlan
planJob spec = case spec.job of
  Offload oj -> gatherOffload oj <&> decideOffload spec oj
  VerifyFolder vj -> gatherGeneration vj.folder <&> decideGeneration spec RequireHistory
  SealMediaSource sj -> gatherGeneration sj.folder <&> decideGeneration spec AllowFresh

-- | The reads an offload's decision needs, and no other. Each destination's three reads
-- sit together; every read answers a value or an Either, so the order changes no decision.
gatherOffload :: (FileSystem :> es) => OffloadJob -> Eff es OffloadFacts
gatherOffload job = do
  sourceTree <- walk job.source
  targets <- V.mapM (gatherTarget job.source) (V.fromList (toList job.destinations))
  originals <- resolveOriginals job.source
  sourceHistory <- historyHashes job.source
  pure OffloadFacts {sourceTree, targets, originals, sourceHistory}

-- | Three reads, because one folder cannot answer them all. The free bytes come from the parent,
-- which is there. The history and the walk of what a partial copy holds come from the root.
gatherTarget :: (FileSystem :> es) => OsPath -> OsPath -> Eff es TargetFacts
gatherTarget source parent = do
  let root = destinationPath parent source
  -- The reason a free-space call failed reaches nobody, so it stops here.
  free <- freeSpaceOf parent <&> either (const Nothing) Just
  -- The whole history, not the chain alone: a manifest the chain names and cannot be read is a
  -- destination fault too.
  history <- historyHashes root <&> fmap (fmap (\pair -> fst pair))
  existing <- walk root
  pure
    TargetFacts
      { root
      , freeBytes = free
      , history
      , existing
      }

-- | The reads a verify's or a seal's decision needs. A folder that is not there reports
-- no free space, because the plan never asked a missing folder for it.
gatherGeneration :: (FileSystem :> es) => OsPath -> Eff es GenerationFacts
gatherGeneration folder = do
  tree <- walk folder
  freeBytes <- maybe (pure Nothing) (\_ -> freeSpaceOf folder <&> either (const Nothing) Just) tree
  history <- historyHashes folder
  pure GenerationFacts {tree, freeBytes, history}
