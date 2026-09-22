module MediaCopy.Gtk.Assets
  ( resolveAsset
  ) where

import Control.Monad.Extra (findM)
import Data.List (intercalate)
import Data.Text qualified as T
import Effectful.Log (logAttention_)
import System.Directory (doesPathExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import MediaCopy.Gtk.Environment (Environment (..), Mode (..), logWith)
import Paths_mediacopy3000 (getDataFileName)

resolveAsset :: Environment -> FilePath -> IO FilePath
resolveAsset environment relative = do
  installed <- getDataFileName relative
  let sourceTree = [relative | environment.mode == Development]
  fromPrefixes <- besideExecutable relative
  let candidates = sourceTree <> (installed : fromPrefixes)
  findM doesPathExist candidates >>= \case
    Just found -> pure found
    Nothing -> do
      logWith environment (logAttention_ (T.pack ("no " <> relative <> "; tried " <> intercalate ", " candidates)))
      pure installed

besideExecutable :: FilePath -> IO [FilePath]
besideExecutable relative = do
  exeDir <- takeDirectory <$> getExecutablePath
  pure [prefix </> "share" </> "mediacopy3000" </> relative | prefix <- [exeDir, takeDirectory exeDir]]
