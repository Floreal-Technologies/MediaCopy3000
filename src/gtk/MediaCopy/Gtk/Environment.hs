module MediaCopy.Gtk.Environment
  ( Environment (..)
  , Mode (..)
  , withEnvironment
  , runIn
  , logWith
  ) where

import Effectful.Log (Logger)
import Floreal.Commons.Environment (lookupEnv)
import Floreal.Commons.Log (withStderrLogger)

import MediaCopy.Effects.AppStack (AppStack)
import MediaCopy.Effects.AppStack qualified as AppStack
import MediaCopy.Runtime (Runner (..))

data Mode = Production | Development
  deriving stock (Eq, Show)

data Environment = Environment
  { mode :: Mode
  , logger :: Logger
  , runner :: Runner
  }

withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment use = withStderrLogger $ \logger -> do
  let runner = Runner (AppStack.run logger)
      environment mode = Environment {mode, logger, runner}
  mode <-
    runIn (environment Production) (lookupEnv "MC3K_ENV") <&> \case
      Just "dev" -> Development
      _ -> Production
  use (environment mode)

runIn :: Environment -> Eff AppStack a -> IO a
runIn Environment {runner = Runner run} = run

logWith :: Environment -> Eff AppStack a -> IO a
logWith = runIn
