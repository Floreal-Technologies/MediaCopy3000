module MediaCopy.Plugin.Connection.Native
  ( childEnvironment
  ) where

import Data.List (isPrefixOf)

-- |
-- >>> childEnvironment [("PATH", "/bin"), ("LC_ALL", "C"), ("AWS_SECRET_ACCESS_KEY", "x"), ("LD_PRELOAD", "evil.so")]
-- [("PATH","/bin"),("LC_ALL","C")]
childEnvironment :: [(String, String)] -> [(String, String)]
childEnvironment = filter (allowed . fst)
  where
    allowed name = name `elem` kept || "LC_" `isPrefixOf` name
    kept = ["PATH", "HOME", "TMPDIR", "LANG", "TZ", "USER", "LOGNAME"]
