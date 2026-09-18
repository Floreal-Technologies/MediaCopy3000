module MediaCopy.Plugin.Setup
  ( Setup (..)
  , Inactive (..)
  , load
  , activate
  ) where

import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Floreal.Commons.Environment (Environment)
import Floreal.Commons.Xdg (Xdg)
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import MediaCopy.Plugin.Manifest qualified as Manifest

import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Plugin.Discovery qualified as Discovery
import MediaCopy.Plugin.Grant qualified as Grant
import MediaCopy.Plugin.Grant qualified as Plugin (Grant)
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Installed qualified as Plugin (Installed)
import MediaCopy.Plugin.Ready qualified as Plugin (Ready)
import MediaCopy.Plugin.Ready qualified as Ready

data Setup = Setup
  { installed :: Vector Plugin.Installed
  , grants :: Map Plugin.Id Plugin.Grant
  , ready :: Vector Plugin.Ready
  , inactive :: Vector Inactive
  , rejected :: Vector Discovery.Rejected
  , grantsProblem :: Maybe Text
  }

load :: (FileSystem :> es, Environment :> es, Xdg :> es) => Eff es Setup
load = do
  found <- Discovery.roots >>= Discovery.scan
  grantFile <- Grant.path >>= Grant.load
  let grants = fromRight Map.empty grantFile
      (inactive, ready) = partitionEithers (map (activate grants) (V.toList found.installed))
  pure
    Setup
      { installed = found.installed
      , grants
      , ready = V.fromList ready
      , inactive = V.fromList inactive
      , rejected = found.rejected
      , grantsProblem = either Just (const Nothing) grantFile
      }

data Inactive = Inactive
  { installed :: Plugin.Installed
  , reason :: Text
  }
  deriving stock (Eq, Show)

activate :: Map Plugin.Id Plugin.Grant -> Plugin.Installed -> Either Inactive Plugin.Ready
activate grantMap installed = case Map.lookup installed.manifest.id grantMap of
  Just grant
    | grant.enabled -> case unanswered grant of
        [] -> Right Ready.Ready {installed, granted = Set.intersection grant.grants declared, settings = grant.settings, trace = grant.trace}
        missing -> Left Inactive {installed, reason = "asks for " <> T.intercalate ", " (map Capability.toText missing) <> ", which is neither granted nor declined"}
  _ -> Left Inactive {installed, reason = "is not enabled"}
  where
    declared = Set.fromList (V.toList installed.manifest.capabilities)
    unanswered grant = Set.toList (declared `Set.difference` (grant.grants <> grant.declined))
