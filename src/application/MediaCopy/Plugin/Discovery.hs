module MediaCopy.Plugin.Discovery
  ( Installed (..)
  , Rejected (..)
  , Discovery (..)
  , pluginRoots
  , discover
  ) where

import Ascmhl.Path (pathText)
import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Aeson (eitherDecodeStrict)
import Data.Foldable (foldlM)
import Data.List (List, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest
import System.Directory.OsPath
import System.Environment (getExecutablePath, lookupEnv)
import System.File.OsPath qualified as FileIO
import System.Info (os)
import System.OsPath (OsPath, encodeUtf, normalise, takeDirectory, takeFileName, (</>))

import MediaCopy.Domain.Plugin (oneLine)

data Installed = Installed
  { folder :: OsPath
  , manifest :: PluginManifest
  , executable :: OsPath
  }
  deriving stock (Eq, Show)

data Rejected = Rejected
  { folder :: OsPath
  , reason :: Text
  }
  deriving stock (Eq, Show)

data Discovery = Discovery
  { installed :: Vector Installed
  , rejected :: Vector Rejected
  }
  deriving stock (Eq, Show)

pluginRoots :: IO (List OsPath)
pluginRoots = case os of
  "darwin" -> do
    home <- getHomeDirectory
    contents <- takeDirectory . takeDirectory <$> (getExecutablePath >>= encodeUtf)
    sequence [below home ["Library", "Application Support", "MediaCopy3000", "Plugins"], below contents ["PlugIns"]]
  "mingw32" -> do
    user <- getXdgDirectory XdgData mempty >>= \appData -> below appData ["MediaCopy3000", "plugins"]
    system <- lookupEnv "ProgramData" >>= traverse (\raw -> encodeUtf raw >>= \root -> below root ["MediaCopy3000", "plugins"])
    exe <- getExecutablePath >>= encodeUtf
    bundled <- below (takeDirectory exe) ["plugins"]
    pure (user : maybeToList system <> [bundled])
  _ -> do
    dataDirs <- (:) <$> getXdgDirectory XdgData mempty <*> getXdgDirectoryList XdgDataDirs
    shared <- traverse (\dir -> below dir ["mediacopy3000", "plugins"]) dataDirs
    exe <- getExecutablePath >>= encodeUtf
    bundled <- below (takeDirectory (takeDirectory exe)) ["lib", "mediacopy3000", "plugins"]
    pure (shared <> [bundled])
  where
    below root parts = foldlM (\path part -> (path </>) <$> encodeUtf part) root parts

discover :: List OsPath -> IO Discovery
discover roots = do
  found <- concat <$> traverse folders roots
  (_, discovery) <- foldlM step (Set.empty, Discovery {installed = V.empty, rejected = V.empty}) found
  pure discovery
  where
    folders root =
      try @IOException (listDirectory root) >>= \case
        Left _ -> pure []
        Right names -> filterM doesDirectoryExist (map (root </>) (sort names))
    step (seen, acc) dir = do
      outcome <- inspectFolder dir
      pure $ case outcome of
        Left reason -> (seen, acc {rejected = V.snoc acc.rejected Rejected {folder = dir, reason = oneLine reason}})
        Right plugin
          | Set.member plugin.manifest.id seen -> (seen, acc)
          | otherwise -> (Set.insert plugin.manifest.id seen, acc {installed = V.snoc acc.installed plugin})

inspectFolder :: OsPath -> IO (Either Text Installed)
inspectFolder dir = do
  jsonPath <- (dir </>) <$> encodeUtf "plugin.json"
  try @IOException (FileIO.readFile' jsonPath) >>= \case
    Left _ -> pure (Left "has no readable plugin.json")
    Right bytes -> case eitherDecodeStrict bytes >>= either (Left . T.unpack) Right . validateManifest of
      Left problem -> pure (Left ("plugin.json " <> T.pack problem))
      Right manifest -> do
        let PluginId pluginId = manifest.id
        let folderName = pathText (takeFileName dir)
        case Map.lookup platformKey manifest.executable of
          _ | folderName /= pluginId -> pure (Left ("is in the folder " <> folderName <> ", not " <> pluginId))
          Nothing -> pure (Left ("has no executable for " <> platformKey))
          Just relative -> do
            exe <- (\rel -> normalise (dir </> rel)) <$> encodeUtf relative
            present <- doesFileExist exe
            pure $
              if present
                then Right Installed {folder = dir, manifest, executable = exe}
                else Left ("has no file " <> pathText exe)
