module MediaCopy.Plugin.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  ) where

import Control.Exception (IOException, try)
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
signal sig (Group leader) = mapM_ (try @IOException . signalProcessGroup sig) leader
