module MediaCopy.Plugin.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  ) where

import System.Process (ProcessHandle, terminateProcess)

data Group = Group

groupOf :: ProcessHandle -> IO Group
groupOf _ = pure Group

terminateGroup :: ProcessHandle -> Group -> IO ()
terminateGroup handle _ = terminateProcess handle

killGroup :: ProcessHandle -> Group -> IO ()
killGroup handle _ = terminateProcess handle
