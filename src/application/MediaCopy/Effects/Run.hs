module MediaCopy.Effects.Run
  ( AppEffects
  , runApp
  ) where

import Effectful (Eff, IOE, runEff)
import Effectful.Time (Time, runTime)

import MediaCopy.Effects.FileSystem (FileSystem, defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (Hasher, runHasher)

type AppEffects = '[FileSystem, Hasher, Time, IOE]

runApp :: Eff AppEffects a -> IO a
runApp = runEff . runTime . runHasher . runFileSystemIO defaultChunkSize
