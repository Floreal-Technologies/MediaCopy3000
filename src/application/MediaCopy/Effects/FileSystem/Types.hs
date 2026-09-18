-- | What both platform halves of the file system say, and the one loop that feeds a handle's bytes
-- to a hook. Written once, because a chunk loop written twice can drift in its last read.
module MediaCopy.Effects.FileSystem.Types
  ( Entry (..)
  , feedHandle
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import System.IO (Handle)

-- | What one call about one directory entry answers.
data Entry = Entry
  { isDirectory :: Bool
  , isRegularFile :: Bool
  , isSymbolicLink :: Bool
  , size :: Int64
  }

-- | A read shorter than nothing ends the loop, so the hook sees every byte exactly once.
feedHandle :: Int -> (ByteString -> IO ()) -> Handle -> IO ()
feedHandle chunkSize onChunk h = do
  bs <- BS.hGet h chunkSize
  unless (BS.null bs) (onChunk bs >> feedHandle chunkSize onChunk h)
