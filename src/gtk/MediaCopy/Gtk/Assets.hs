module MediaCopy.Gtk.Assets
  ( resolveAsset
  ) where

import Control.Monad.Extra (findM)
import Data.List (intercalate)
import System.Directory (doesPathExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import MediaCopy.Gtk.Environment (Environment (..))
import MediaCopy.Gtk.Log (logLine)
import Paths_mediacopy3000 (getDataFileName)

resolveAsset :: Environment -> FilePath -> IO FilePath
resolveAsset environment relative = do
  installed <- getDataFileName relative
  let sourceTree = [relative | environment == Development]
  fromPrefixes <- besideExecutable relative
  let candidates = sourceTree <> (installed : fromPrefixes)
  findM doesPathExist candidates >>= \case
    Just found -> pure found
    Nothing -> do
      logLine ("no " <> relative <> "; tried " <> intercalate ", " candidates)
      pure installed

besideExecutable :: FilePath -> IO [FilePath]
besideExecutable relative = do
  exeDir <- takeDirectory <$> getExecutablePath
  pure [prefix </> "share" </> "mediacopy3000" </> relative | prefix <- [exeDir, takeDirectory exeDir]]
