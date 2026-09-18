module MediaCopy.Effects.DirEntry
  ( DirEntry (..)
  , feedHandle
  ) where

import Data.ByteString qualified as BS
import System.IO (Handle)

data DirEntry = DirEntry
  { isDirectory :: Bool
  , isRegularFile :: Bool
  , isSymbolicLink :: Bool
  , size :: Int64
  }

feedHandle :: Int -> (ByteString -> IO ()) -> Handle -> IO ()
feedHandle chunkSize onChunk h = do
  bs <- BS.hGet h chunkSize
  unless (BS.null bs) (onChunk bs >> feedHandle chunkSize onChunk h)
