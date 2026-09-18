module MediaCopy.Effects.AppStack
  ( AppStack
  , run
  ) where

import Effectful (runEff)
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Log (Log, Logger, defaultLogLevel, runLog)
import Effectful.Time (Time, runTime)
import Effectful.Timeout (Timeout, runTimeout)
import Floreal.Commons.Environment (Environment, runEnvironmentIO)
import Floreal.Commons.Process (Process, runProcessIO)
import Floreal.Commons.Xdg (Xdg, runXdgIO)

import MediaCopy.Effects.Console (Console)
import MediaCopy.Effects.Console qualified as Console
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Effects.Hasher (Hasher)
import MediaCopy.Effects.Hasher qualified as Hasher

type AppStack = '[FileSystem, Hasher, Process, Environment, Xdg, Console, Log, Time, Timeout, Concurrent, IOE]

run :: Logger -> Eff AppStack a -> IO a
run logger action =
  action
    & FileSystem.runIO FileSystem.defaultChunkSize
    & Hasher.run
    & runProcessIO
    & runEnvironmentIO
    & runXdgIO
    & Console.runIO
    & runLog "mediacopy3000" logger defaultLogLevel
    & runTime
    & runTimeout
    & runConcurrent
    & runEff
