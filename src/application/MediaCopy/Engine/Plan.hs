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

gatherOffload :: (FileSystem :> es) => OffloadJob -> Eff es OffloadFacts
gatherOffload job = do
  sourceTree <- walk job.source
  targets <- V.mapM (gatherTarget job.source) (V.fromList (toList job.destinations))
  originals <- resolveOriginals job.source
  sourceHistory <- historyHashes job.source
  pure OffloadFacts {sourceTree, targets, originals, sourceHistory}

gatherTarget :: (FileSystem :> es) => OsPath -> OsPath -> Eff es TargetFacts
gatherTarget source parent = do
  let root = destinationPath parent source
  free <- freeSpaceOf parent <&> either (const Nothing) Just
  history <- historyHashes root <&> fmap (fmap (\pair -> fst pair))
  existing <- walk root
  pure
    TargetFacts
      { root
      , freeBytes = free
      , history
      , existing
      }

gatherGeneration :: (FileSystem :> es) => OsPath -> Eff es GenerationFacts
gatherGeneration folder = do
  tree <- walk folder
  freeBytes <- maybe (pure Nothing) (\_ -> freeSpaceOf folder <&> either (const Nothing) Just) tree
  history <- historyHashes folder
  pure GenerationFacts {tree, freeBytes, history}
