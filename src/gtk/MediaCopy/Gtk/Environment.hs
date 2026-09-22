module MediaCopy.Gtk.Environment
  ( Environment (..)
  , Mode (..)
  , withEnvironment
  , logWith
  ) where

import Control.Exception (finally)
import Data.Text.IO qualified as T
import Effectful (Eff, IOE, runEff)
import Effectful.Log (Log, Logger, defaultLogLevel, mkLogger, runLog, showLogMessage, shutdownLogger, waitForLogger)
import System.Environment (lookupEnv)
import System.IO (stderr)

data Mode = Production | Development
  deriving stock (Eq, Show)

data Environment = Environment
  { mode :: Mode
  , logger :: Logger
  }

withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment use = do
  mode <-
    lookupEnv "MC3K_ENV" >>= \case
      Just "dev" -> pure Development
      _ -> pure Production
  logger <- mkLogger "stderr" (\message -> T.hPutStrLn stderr (showLogMessage Nothing message))
  use Environment {mode, logger} `finally` (waitForLogger logger >> shutdownLogger logger)

logWith :: Environment -> Eff '[Log, IOE] a -> IO a
logWith environment action = runEff (runLog "mediacopy3000" environment.logger defaultLogLevel action)
