module MediaCopy.Plugin.Connection.Native
  ( childEnvironment
  ) where

import Data.Char (toUpper)

-- |
-- >>> childEnvironment [("Path", "C:\\Windows"), ("SystemRoot", "C:\\Windows"), ("GITHUB_TOKEN", "x")]
-- [("Path","C:\\Windows"),("SystemRoot","C:\\Windows")]
childEnvironment :: [(String, String)] -> [(String, String)]
childEnvironment = filter (\entry -> entry & fst & map toUpper & (`elem` kept))
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
