module MediaCopy.Plugin.Grants
  ( Grant (..)
  , grantsPath
  , loadGrants
  , Ready (..)
  , Inactive (..)
  , activate
  ) where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.Foldable (fold, foldlM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest
import System.Directory.OsPath (XdgDirectory (XdgConfig), doesFileExist, getHomeDirectory, getXdgDirectory)
import System.File.OsPath qualified as FileIO
import System.Info (os)
import System.OsPath (OsPath, encodeUtf, (</>))

import MediaCopy.Plugin.Discovery (Installed (..))

-- $setup
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Set qualified as Set
-- >>> import Data.Vector qualified as V
-- >>> import System.OsPath (unsafeEncodeUtf)
-- >>> let manifest = PluginManifest {id = PluginId "tech.floreal.probe", name = "Probe", version = "1.0.0", api = 1, namespace = Nothing, executable = Map.empty, roles = V.singleton Inspector, capabilities = V.fromList [FilesRead, Block], settings = V.empty, jobFields = V.empty}
-- >>> let probe = Installed {folder = unsafeEncodeUtf "/plugins/tech.floreal.probe", manifest, executable = unsafeEncodeUtf "/plugins/tech.floreal.probe/probe"}
-- >>> let reason = either (.reason) (const "active")

data Grant = Grant
  { enabled :: Bool
  , trace :: Bool
  , grants :: Set Capability
  , declined :: Set Capability
  , settings :: Map Text Value
  }
  deriving stock (Eq, Show)

instance FromJSON Grant where
  parseJSON = withObject "grant" $ \o -> do
    enabled <- fromMaybe False <$> o .:? "enabled"
    trace <- fromMaybe False <$> o .:? "trace"
    grants <- known <$> o .:? "grants"
    declined <- known <$> o .:? "declined"
    settings <- fromMaybe Map.empty <$> o .:? "settings"
    pure Grant {enabled, trace, grants, declined, settings}
    where
      known :: Maybe (Set Text) -> Set Capability
      known names = Set.fromList [capability | capability <- [minBound ..], Set.member (capabilityName capability) (fold names)]

grantsPath :: IO OsPath
grantsPath = do
  folder <- case os of
    "darwin" -> getHomeDirectory >>= \home -> foldlM (\path part -> (path </>) <$> encodeUtf part) home ["Library", "Application Support", "MediaCopy3000"]
    "mingw32" -> encodeUtf "MediaCopy3000" >>= getXdgDirectory XdgConfig
    _ -> encodeUtf "mediacopy3000" >>= getXdgDirectory XdgConfig
  (folder </>) <$> encodeUtf "plugins.json"

loadGrants :: OsPath -> IO (Either Text (Map PluginId Grant))
loadGrants path = do
  present <- doesFileExist path
  if not present
    then pure (Right Map.empty)
    else
      try @IOException (FileIO.readFile' path) >>= \case
        Left e -> pure (Left ("plugins.json cannot be read: " <> T.show e))
        Right bytes -> pure $ case eitherDecodeStrict bytes of
          Left problem -> Left ("plugins.json: " <> T.pack problem)
          Right (GrantFile entries) -> Right entries

newtype GrantFile = GrantFile (Map PluginId Grant)

instance FromJSON GrantFile where
  parseJSON = withObject "plugins.json" $ \o -> GrantFile . fromMaybe Map.empty <$> o .:? "plugins"

data Ready = Ready
  { installed :: Installed
  , granted :: Set Capability
  , settings :: Map Text Value
  , keyring :: Maybe Text
  , trace :: Bool
  }
  deriving stock (Eq, Show)

data Inactive = Inactive
  { installed :: Installed
  , reason :: Text
  }
  deriving stock (Eq, Show)

-- |
-- >>> reason (activate Map.empty probe)
-- "is not enabled"
-- >>> reason (activate (Map.singleton (PluginId "tech.floreal.probe") Grant {enabled = True, trace = False, grants = Set.singleton FilesRead, declined = Set.empty, settings = Map.empty}) probe)
-- "asks for block, which is neither granted nor declined"
-- >>> reason (activate (Map.singleton (PluginId "tech.floreal.probe") Grant {enabled = True, trace = False, grants = Set.singleton FilesRead, declined = Set.singleton Block, settings = Map.empty}) probe)
-- "active"
activate :: Map PluginId Grant -> Installed -> Either Inactive Ready
activate grantMap installed = case Map.lookup installed.manifest.id grantMap of
  Just grant
    | grant.enabled -> case unanswered grant of
        [] -> Right Ready {installed, granted = Set.intersection grant.grants declared, settings = grant.settings, keyring = Nothing, trace = grant.trace}
        missing -> Left Inactive {installed, reason = "asks for " <> T.intercalate ", " (map capabilityName missing) <> ", which is neither granted nor declined"}
  _ -> Left Inactive {installed, reason = "is not enabled"}
  where
    declared = Set.fromList (V.toList installed.manifest.capabilities)
    unanswered grant = Set.toList (declared `Set.difference` (grant.grants <> grant.declined))
