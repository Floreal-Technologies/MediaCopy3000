module MediaCopy.Domain.Plan
  ( Plan (..)
  , Execution (..)
  , CopyPass (..)
  , RecordPass (..)
  , SealPass (..)
  , blocked
  , withPlugins
  , blockers
  , equivalent
  , plannedGenerations
  , raceChecks
  , originsDescription
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Hash (Hash)
import Ascmhl.RelPath (RelPath)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import MediaCopy.Domain.Finding (Finding)
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlannedGeneration (PlannedGeneration)
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Domain.PluginPlan (PluginPlan)
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Target (Target)
import MediaCopy.Domain.Target qualified as Target

data CopyPass = CopyPass
  { source :: OsPath
  , generations :: Vector PlannedGeneration
  , carried :: Int
  }
  deriving stock (Eq, Show)

data RecordPass = RecordPass
  { folder :: OsPath
  , generation :: PlannedGeneration
  }
  deriving stock (Eq, Show)

data Execution
  = CopyInto CopyPass
  | RecordAt RecordPass
  deriving stock (Eq, Show)

data SealPass = SealPass
  { steps :: Vector PlanStep
  , bytes :: Int64
  , onFailure :: Offload.OnSealFailure
  , generation :: PlannedGeneration
  }
  deriving stock (Eq, Show)

data Plan = Plan
  { spec :: JobSpec
  , execution :: Execution
  , targets :: Vector Target
  , format :: Maybe Algo
  , originsUsed :: Text
  , steps :: Vector PlanStep
  , totalBytes :: Int64
  , bytesToRead :: Int64
  , creates :: Vector OsPath
  , ignorePatterns :: Vector Text
  , findings :: Vector Finding
  , sealPass :: Maybe SealPass
  , generations :: Int
  , plugins :: PluginPlan
  }
  deriving stock (Eq, Show)

blockers :: Plan -> Vector Finding
blockers plan = V.filter (\finding -> finding.severity == Severity.Blocker) plan.findings

withPlugins :: PluginPlan -> Plan -> Plan
withPlugins pluginPlan plan = plan {plugins = pluginPlan}

blocked :: Plan -> Bool
blocked plan = not (V.null (blockers plan)) || not (V.null (PluginPlan.blockers plan.plugins))

equivalent :: Plan -> Plan -> Bool
equivalent a b = blankFree a == blankFree b
  where
    blankFree plan = plan {targets = V.map (\target -> target {Target.freeBytes = Nothing}) plan.targets}

raceChecks :: Plan -> Vector (OsPath, Int)
raceChecks plan = case plan.execution of
  RecordAt record -> V.singleton (record.folder, record.generation.number)
  CopyInto copy -> V.singleton (copy.source, maybe (copy.carried + 1) (.generation.number) plan.sealPass)

plannedGenerations :: Plan -> Vector PlannedGeneration
plannedGenerations plan = sealed <> executed
  where
    sealed = maybe V.empty (\pass -> V.singleton pass.generation) plan.sealPass
    executed = case plan.execution of
      CopyInto copy -> copy.generations
      RecordAt record -> V.singleton record.generation

originsDescription :: Map RelPath Hash -> Text
originsDescription resolved
  | Map.null resolved = "none found, so every file is recorded as original"
  | otherwise = "the media source's own history, " <> Prose.plural "file" (Map.size resolved)
