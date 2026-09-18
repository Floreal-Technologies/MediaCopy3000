module MediaCopy.Domain.PluginPlan
  ( PluginPlan (..)
  , empty
  , blockers
  ) where

import Data.Vector qualified as V

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.Severity qualified as Severity

data PluginPlan = PluginPlan
  { active :: Vector PluginRef
  , findings :: Vector PluginFinding
  , contributions :: Contributions
  }
  deriving stock (Eq, Show)

empty :: PluginPlan
empty = PluginPlan {active = V.empty, findings = V.empty, contributions = Contributions.empty}

blockers :: PluginPlan -> Vector PluginFinding
blockers pluginPlan = V.filter (\finding -> finding.severity == Severity.Blocker) pluginPlan.findings
