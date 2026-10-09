module MediaCopy.Effects.Run
  ( runApp
  ) where

import Effectful (Eff, IOE, runEff)
import Effectful.Time (Time, runTime)

import MediaCopy.Effects.FileSystem (FileSystem, defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (Hasher, runHasher)

runApp :: Eff '[FileSystem, Hasher, Time, IOE] a -> IO a
runApp = runEff . runTime . runHasher . runFileSystemIO defaultChunkSize
