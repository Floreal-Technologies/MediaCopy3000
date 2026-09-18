module MediaCopy.Gtk.Assets
  ( resolveAsset
  ) where

import Control.Monad.Extra (findM)
import Data.List (intercalate)
import Data.Text qualified as T
import Effectful.Log (Log, logAttention_)
import Floreal.Commons.Environment qualified as Env
import System.FilePath (takeDirectory, (</>))
import System.OsPath qualified as OsPath

import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Gtk.Environment (Mode (..))
import Paths_mediacopy3000 (getDataFileName)

resolveAsset :: (Env.Environment :> es, FileSystem :> es, Log :> es, IOE :> es) => Mode -> FilePath -> Eff es FilePath
resolveAsset mode relative = do
  installed <- liftIO (getDataFileName relative)
  let sourceTree = [relative | mode == Development]
  fromPrefixes <- besideExecutable relative
  let candidates = sourceTree <> (installed : fromPrefixes)
  findM exists candidates >>= \case
    Just found -> pure found
    Nothing -> do
      logAttention_ (T.pack ("no " <> relative <> "; tried " <> intercalate ", " candidates))
      pure installed
  where
    exists candidate = liftIO (OsPath.encodeUtf candidate) >>= FileSystem.doesFileExist

besideExecutable :: (Env.Environment :> es, IOE :> es) => FilePath -> Eff es [FilePath]
besideExecutable relative = do
  exeDir <- Env.getExecutablePath >>= \path -> liftIO (OsPath.decodeUtf path) <&> takeDirectory
  pure [prefix </> "share" </> "mediacopy3000" </> relative | prefix <- [exeDir, takeDirectory exeDir]]
