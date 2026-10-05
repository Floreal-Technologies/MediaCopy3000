module MediaCopy.Plugin
  ( PluginSetup (..)
  , loadPluginSetup
  , planWithPlugins
  ) where

import Data.Either (fromRight, partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Domain.Plan (JobPlan, withPluginPlan)
import MediaCopy.Plugin.Discovery
import MediaCopy.Plugin.Grants
import MediaCopy.Plugin.Session

data PluginSetup = PluginSetup
  { ready :: Vector Ready
  , inactive :: Vector Inactive
  , rejected :: Vector Rejected
  , grantsProblem :: Maybe Text
  }

loadPluginSetup :: IO PluginSetup
loadPluginSetup = do
  found <- pluginRoots >>= discover
  grantFile <- grantsPath >>= loadGrants
  let grants = fromRight Map.empty grantFile
      (inactive, ready) = partitionEithers (map (activate grants) (V.toList found.installed))
  pure
    PluginSetup
      { ready = V.fromList ready
      , inactive = V.fromList inactive
      , rejected = found.rejected
      , grantsProblem = either Just (const Nothing) grantFile
      }

planWithPlugins :: SessionConfig -> JobPlan -> IO JobPlan
planWithPlugins config plan = do
  pluginPlan <- withSession config PlanStage plan planHooks
  pure (withPluginPlan pluginPlan plan)
