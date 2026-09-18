module MediaCopy.Plugin.Discovery
  ( Discovery (..)
  , Rejected (..)
  , roots
  , scan
  ) where

import Ascmhl.OsPath qualified as OsPath
import Control.Monad (filterM)
import Data.Aeson (eitherDecodeStrict)
import Data.Foldable (foldlM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Exception qualified as Exception
import Floreal.Commons.Environment (Environment, getExecutablePath, getHomeDirectory, lookupEnv)
import Floreal.Commons.Xdg (Xdg, XdgDirectory (..), XdgDirectoryList (..), getXdgDirectory, getXdgDirectoryList)
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Manifest qualified as Manifest
import System.Info (os)
import System.OsPath (encodeUtf, normalise, takeDirectory, takeFileName, (</>))

import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Installed qualified as Plugin (Installed)

data Rejected = Rejected
  { folder :: OsPath
  , reason :: Text
  }
  deriving stock (Eq, Show)

data Discovery = Discovery
  { installed :: Vector Plugin.Installed
  , rejected :: Vector Rejected
  }
  deriving stock (Eq, Show)

roots :: (Environment :> es, Xdg :> es) => Eff es (List OsPath)
roots = case os of
  "darwin" -> do
    home <- getHomeDirectory
    contents <- takeDirectory . takeDirectory <$> getExecutablePath
    sequence [below home ["Library", "Application Support", "MediaCopy3000", "Plugins"], below contents ["PlugIns"]]
  "mingw32" -> do
    user <- getXdgDirectory XdgData mempty >>= \appData -> below appData ["MediaCopy3000", "plugins"]
    system <- lookupEnv "ProgramData" >>= traverse (\raw -> encodeUtf raw >>= \root -> below root ["MediaCopy3000", "plugins"])
    exe <- getExecutablePath
    bundled <- below (takeDirectory exe) ["plugins"]
    pure (user : maybeToList system <> [bundled])
  _ -> do
    dataDirs <- (:) <$> getXdgDirectory XdgData mempty <*> getXdgDirectoryList XdgDataDirs
    shared <- traverse (\dir -> below dir ["mediacopy3000", "plugins"]) dataDirs
    exe <- getExecutablePath
    bundled <- below (takeDirectory (takeDirectory exe)) ["lib", "mediacopy3000", "plugins"]
    pure (shared <> [bundled])
  where
    below root parts = foldlM (\path part -> (path </>) <$> encodeUtf part) root parts

scan :: (FileSystem :> es) => List OsPath -> Eff es Discovery
scan places = do
  found <- concat <$> traverse folders places
  (_, discovery) <- foldlM step (Set.empty, Discovery {installed = V.empty, rejected = V.empty}) found
  pure discovery
  where
    folders root =
      FileSystem.listDirectory root >>= \case
        Nothing -> pure []
        Just names -> filterM FileSystem.doesDirectoryExist (map (root </>) names)
    step (seen, acc) dir = do
      outcome <- inspectFolder dir
      pure $ case outcome of
        Left reason -> (seen, acc {rejected = V.snoc acc.rejected Rejected {folder = dir, reason = Prose.oneLine reason}})
        Right plugin
          | Set.member plugin.manifest.id seen -> (seen, acc)
          | otherwise -> (Set.insert plugin.manifest.id seen, acc {installed = V.snoc acc.installed plugin})

inspectFolder :: (FileSystem :> es) => OsPath -> Eff es (Either Text Plugin.Installed)
inspectFolder dir = do
  jsonPath <- (dir </>) <$> encodeUtf "plugin.json"
  Exception.try @IOException (FileSystem.readBytes jsonPath) >>= \case
    Left _ -> pure (Left "has no readable plugin.json")
    Right bytes -> case eitherDecodeStrict bytes >>= either (Left . T.unpack) Right . Manifest.validate of
      Left problem -> pure (Left ("plugin.json " <> T.pack problem))
      Right manifest -> do
        let Id.Id pluginId = manifest.id
        let folderName = OsPath.toText (takeFileName dir)
        case Map.lookup Manifest.platformKey manifest.executable of
          _ | folderName /= pluginId -> pure (Left ("is in the folder " <> folderName <> ", not " <> pluginId))
          Nothing -> pure (Left ("has no executable for " <> Manifest.platformKey))
          Just relative -> do
            exe <- (\rel -> normalise (dir </> rel)) <$> encodeUtf relative
            present <- FileSystem.doesFileExist exe
            pure $
              if present
                then Right Installed.Installed {folder = dir, manifest, executable = exe}
                else Left ("has no file " <> OsPath.toText exe)
