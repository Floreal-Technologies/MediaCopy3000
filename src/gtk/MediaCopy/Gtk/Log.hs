-- | What the application says on the standard error.
module MediaCopy.Gtk.Log
  ( logLine
  ) where

import System.IO (hPutStrLn, stderr)

-- | One line, under the program's name.
logLine :: String -> IO ()
logLine message = hPutStrLn stderr ("mediacopy3000: " <> message)
