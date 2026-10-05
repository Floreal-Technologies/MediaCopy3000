module MediaCopy.Plugin
  ( PluginSetup (..)
  , loadPluginSetup
  , planWithPlugins
  ) where

import Control.Monad (forM)
import Data.Aeson (Value (String))
import Data.Either (fromRight, lefts, partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (Field (..), FieldKind (..), PluginId (..), PluginManifest (..))

import MediaCopy.Domain.Plan (JobPlan, withPluginPlan)
import MediaCopy.Plugin.Discovery
import MediaCopy.Plugin.Grants
import MediaCopy.Plugin.Secrets (readSecret)
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
  withKeys <- traverse withSecrets ready
  pure
    PluginSetup
      { ready = V.fromList withKeys
      , inactive = V.fromList inactive
      , rejected = found.rejected
      , grantsProblem = either Just (const Nothing) grantFile
      }

withSecrets :: Ready -> IO Ready
withSecrets entry = do
  let PluginId pluginId = entry.installed.manifest.id
      secretKeys = [field.key | field <- V.toList entry.installed.manifest.settings, field.kind == SecretField]
  found <- forM secretKeys (readSecret pluginId)
  let stored = Map.fromList [(key, String value) | (key, Right (Just value)) <- zip secretKeys found]
  pure
    Ready
      { installed = entry.installed
      , granted = entry.granted
      , settings = Map.union stored entry.settings
      , keyring = listToMaybe (lefts found)
      , trace = entry.trace
      }

planWithPlugins :: SessionConfig -> JobPlan -> IO JobPlan
planWithPlugins config plan = do
  pluginPlan <- withSession config PlanStage plan planHooks
  pure (withPluginPlan pluginPlan plan)
