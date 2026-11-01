module MediaCopy.Plugin.Grant
  ( Grant (..)
  , path
  , readJson
  , load
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Foldable (fold, foldlM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.Exception qualified as Exception
import Floreal.Commons.Environment (Environment, getHomeDirectory)
import Floreal.Commons.Xdg (Xdg, XdgDirectory (XdgConfig), getXdgDirectory)
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import System.Info (os)
import System.OsPath (encodeUtf, (</>))

import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem

data Grant = Grant
  { enabled :: Bool
  , trace :: Bool
  , grants :: Set Plugin.Capability
  , declined :: Set Plugin.Capability
  , settings :: Map Text Value
  }
  deriving stock (Eq, Show)

-- |
-- >>> import Data.Aeson (eitherDecode)
-- >>> eitherDecode "{\"enabled\":true,\"roles\":{\"inspector\":true},\"grants\":[\"plan.inspect\",\"old.thing\"]}" :: Either String Grant
-- Right (Grant {enabled = True, trace = False, grants = fromList [PlanInspect], declined = fromList [], settings = fromList []})
instance FromJSON Grant where
  parseJSON = withObject "grant" $ \o -> do
    enabled <- fromMaybe False <$> o .:? "enabled"
    trace <- fromMaybe False <$> o .:? "trace"
    grants <- known <$> o .:? "grants"
    declined <- known <$> o .:? "declined"
    settings <- fromMaybe Map.empty <$> o .:? "settings"
    pure Grant {enabled, trace, grants, declined, settings}
    where
      known :: Maybe (Set Text) -> Set Plugin.Capability
      known names = Set.fromList [capability | capability <- [minBound ..], Set.member (display capability) (fold names)]

path :: (Environment :> es, Xdg :> es) => Eff es OsPath
path = do
  folder <- case os of
    "darwin" -> getHomeDirectory >>= \home -> foldlM (\parent part -> (parent </>) <$> encodeUtf part) home ["Library", "Application Support", "MediaCopy3000"]
    "mingw32" -> encodeUtf "MediaCopy3000" >>= getXdgDirectory XdgConfig
    _ -> encodeUtf "mediacopy3000" >>= getXdgDirectory XdgConfig
  (folder </>) <$> encodeUtf "plugins.json"

readJson :: (FileSystem :> es) => OsPath -> Eff es (Either Text Value)
readJson file = do
  present <- FileSystem.doesFileExist file
  if not present
    then pure (Right (object []))
    else
      Exception.try @IOException (FileSystem.readBytes file) >>= \case
        Left e -> pure (Left ("plugins.json cannot be read: " <> T.show e))
        Right bytes -> pure (first (\problem -> "plugins.json: " <> T.pack problem) (eitherDecodeStrict bytes))

load :: (FileSystem :> es) => OsPath -> Eff es (Either Text (Map Plugin.Id Grant))
load file = do
  json <- readJson file
  pure $ do
    value <- json
    first
      (\problem -> "plugins.json: " <> T.pack problem)
      (parseEither (withObject "plugins.json" (\o -> fromMaybe Map.empty <$> o .:? "plugins")) value)
