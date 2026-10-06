module MediaCopy.Plugin.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  , childEnvironment
  ) where

import Data.Char (toUpper)
import System.Process (ProcessHandle, terminateProcess)

data Group = Group

groupOf :: ProcessHandle -> IO Group
groupOf _ = pure Group

terminateGroup :: ProcessHandle -> Group -> IO ()
terminateGroup handle _ = terminateProcess handle

killGroup :: ProcessHandle -> Group -> IO ()
killGroup handle _ = terminateProcess handle

-- |
-- >>> childEnvironment [("Path", "C:\\Windows"), ("SystemRoot", "C:\\Windows"), ("GITHUB_TOKEN", "x")]
-- [("Path","C:\\Windows"),("SystemRoot","C:\\Windows")]
childEnvironment :: [(String, String)] -> [(String, String)]
childEnvironment = filter ((`elem` kept) . map toUpper . fst)
  where
    kept =
      [ "SYSTEMROOT"
      , "WINDIR"
      , "SYSTEMDRIVE"
      , "PATH"
      , "PATHEXT"
      , "COMSPEC"
      , "TEMP"
      , "TMP"
      , "USERPROFILE"
      , "HOMEDRIVE"
      , "HOMEPATH"
      , "APPDATA"
      , "LOCALAPPDATA"
      , "PROGRAMDATA"
      , "USERNAME"
      ]
