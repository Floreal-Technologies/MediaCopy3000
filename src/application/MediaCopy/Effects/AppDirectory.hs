module MediaCopy.Effects.AppDirectory
  ( ensure
  ) where

import Data.Vector qualified as V
import Floreal.Commons.Xdg (Xdg, XdgDirectory, appPath)

import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem

ensure :: (Xdg :> es, FileSystem :> es) => XdgDirectory -> OsPath -> OsPath -> Eff es OsPath
ensure kind program path = do
  dir <- appPath kind program path
  FileSystem.makeDirectories (V.singleton dir)
  pure dir
