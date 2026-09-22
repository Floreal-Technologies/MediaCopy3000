module MediaCopy.Gtk.Log
  ( logLine
  ) where

import System.IO (hPutStrLn, stderr)

logLine :: String -> IO ()
logLine message = hPutStrLn stderr ("mediacopy3000: " <> message)
