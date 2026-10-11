module MediaCopy.Resources.Splice (compiledResources) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject, runIO)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openBinaryTempFile)
import System.Process (callProcess, readProcess)

compiledResources :: FilePath -> Q Exp
compiledResources relative = do
  manifest <- makeRelativeToProject relative
  let sourceDir = takeDirectory manifest
  addDependentFile manifest
  listed <- runIO (readProcess "glib-compile-resources" ["--sourcedir", sourceDir, "--generate-dependencies", manifest] "")
  forM_ (lines listed) addDependentFile
  bundle <- runIO $ do
    tmp <- getTemporaryDirectory
    (target, handle) <- openBinaryTempFile tmp "mediacopy3000.gresource"
    hClose handle
    callProcess "glib-compile-resources" ["--sourcedir", sourceDir, "--target", target, manifest]
    BS.readFile target <* removeFile target
  lift bundle
