module MediaCopy.Plugin.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  , childEnvironment
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (CPid)
import System.Process (ProcessHandle, getPid)

newtype Group = Group (Maybe CPid)

groupOf :: ProcessHandle -> IO Group
groupOf handle = Group <$> getPid handle

terminateGroup :: ProcessHandle -> Group -> IO ()
terminateGroup _ group = signal sigTERM group

killGroup :: ProcessHandle -> Group -> IO ()
killGroup _ group = signal sigKILL group

signal :: Signal -> Group -> IO ()
signal sig (Group leader) = forM_ leader (try @IOException . signalProcessGroup sig)

-- |
-- >>> childEnvironment [("PATH", "/bin"), ("LC_ALL", "C"), ("AWS_SECRET_ACCESS_KEY", "x"), ("LD_PRELOAD", "evil.so")]
-- [("PATH","/bin"),("LC_ALL","C")]
childEnvironment :: [(String, String)] -> [(String, String)]
childEnvironment = filter (allowed . fst)
  where
    allowed name = name `elem` kept || "LC_" `isPrefixOf` name
    kept = ["PATH", "HOME", "TMPDIR", "LANG", "TZ", "USER", "LOGNAME"]
