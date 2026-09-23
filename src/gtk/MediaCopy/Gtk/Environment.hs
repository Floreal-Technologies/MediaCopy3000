module MediaCopy.Gtk.Environment
  ( Environment (..)
  , Mode (..)
  , Ui
  , UiStack
  , withEnvironment
  , runUi
  , logFault
  , abortApp
  , exitOnFault
  ) where

import Control.Exception (SomeException, displayException, finally)
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Log (Log, Logger, defaultLogLevel, logAttention_, mkLogger, runLog, showLogMessage, shutdownLogger, waitForLogger)
import Effectful.Reader.Static (Reader, runReader)
import GI.Gio qualified as Gio
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

import MediaCopy.Gtk.Eff (Gtk, runGtk)

data Mode = Production | Development
  deriving stock (Eq, Show)

data Environment = Environment
  { mode :: Mode
  , logger :: Logger
  }

type Ui es = (Gtk :> es, Log :> es, Reader Environment :> es, IOE :> es)

type UiStack = '[Gtk, Log, Reader Environment, IOE]

withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment use = do
  mode <-
    lookupEnv "MC3K_ENV" >>= \case
      Just "dev" -> pure Development
      _ -> pure Production
  logger <- mkLogger "stderr" (\message -> T.hPutStrLn stderr (showLogMessage Nothing message))
  use Environment {mode, logger} `finally` (waitForLogger logger >> shutdownLogger logger)

runUi :: Environment -> (SomeException -> Eff UiStack ()) -> Eff UiStack a -> IO a
runUi environment onFault action =
  runEff
    . runReader environment
    . runLog "mediacopy3000" environment.logger defaultLogLevel
    . runGtk onFault
    $ action

logFault :: (Log :> es) => SomeException -> Eff es Text
logFault err = do
  let message = T.pack (displayException err)
  logAttention_ message
  pure message

abortApp :: (Ui es, Gio.IsApplication app) => IORef Bool -> app -> SomeException -> Eff es ()
abortApp failed app err = do
  _ <- logFault err
  liftIO (writeIORef failed True)
  Gio.applicationQuit app

exitOnFault :: Int32 -> IORef Bool -> IO ()
exitOnFault status failed = do
  faulted <- readIORef failed
  when
    (status /= 0 || faulted)
    (exitWith (ExitFailure (if status /= 0 then fromIntegral status else 1)))
