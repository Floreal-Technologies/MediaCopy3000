module MediaCopy.Gtk.Assets
  ( resolveAsset
  ) where

import Control.Monad.Extra (findM)
import Data.List (intercalate)
import Data.Text qualified as T
import Effectful (Eff, liftIO)
import Effectful.Log (logAttention_)
import Effectful.Reader.Static (ask)
import System.Directory (doesPathExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import MediaCopy.Gtk.Environment (Environment (..), Mode (..), Ui)
import Paths_mediacopy3000 (getDataFileName)

resolveAsset :: (Ui es) => FilePath -> Eff es FilePath
resolveAsset relative = do
  environment <- ask @Environment
  installed <- liftIO (getDataFileName relative)
  let sourceTree = [relative | environment.mode == Development]
  fromPrefixes <- liftIO (besideExecutable relative)
  let candidates = sourceTree <> (installed : fromPrefixes)
  liftIO (findM doesPathExist candidates) >>= \case
    Just found -> pure found
    Nothing -> do
      logAttention_ (T.pack ("no " <> relative <> "; tried " <> intercalate ", " candidates))
      pure installed

besideExecutable :: FilePath -> IO [FilePath]
besideExecutable relative = do
  exeDir <- takeDirectory <$> getExecutablePath
  pure [prefix </> "share" </> "mediacopy3000" </> relative | prefix <- [exeDir, takeDirectory exeDir]]
