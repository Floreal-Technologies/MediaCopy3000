module MediaCopy.Effects.FileSystem.Types
  ( Entry (..)
  , feedHandle
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import System.IO (Handle)

data Entry = Entry
  { isDirectory :: Bool
  , isRegularFile :: Bool
  , isSymbolicLink :: Bool
  , size :: Int64
  }

feedHandle :: Int -> (ByteString -> IO ()) -> Handle -> IO ()
feedHandle chunkSize onChunk h = do
  bs <- BS.hGet h chunkSize
  unless (BS.null bs) (onChunk bs >> feedHandle chunkSize onChunk h)
