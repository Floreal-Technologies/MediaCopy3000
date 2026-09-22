module MediaCopy.Effects.FileSystem.Native
  ( Entry (..)
  , statEntry
  , syncAndClose
  , syncDirectory
  , readCold
  ) where

import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (castPtr)
import System.File.OsPath qualified as FileIO
import System.FilePath (takeDrive)
import System.IO (Handle, IOMode (ReadMode), hClose, hFlush)
import System.OsPath (OsPath, decodeFS)
import System.Win32.File qualified as File
import System.Win32.Types qualified as Types

import MediaCopy.Effects.FileSystem.Types (Entry (..), feedHandle)

statEntry :: OsPath -> IO Entry
statEntry path = do
  path' <- decodeFS path
  attrs <- File.getFileAttributesExStandard path'
  let word = File.fadFileAttributes attrs
      has flag = word .&. flag /= 0
      directory = has File.fILE_ATTRIBUTE_DIRECTORY
      reparse = has File.fILE_ATTRIBUTE_REPARSE_POINT
  pure
    Entry
      { isDirectory = directory
      , isRegularFile = not directory && not reparse
      , isSymbolicLink = reparse
      , size = fromIntegral (File.fadFileSize attrs)
      }

syncAndClose :: Handle -> IO ()
syncAndClose h =
  ( do
      hFlush h
      Types.withHandleToHANDLE h File.flushFileBuffers
  )
    `finally` hClose h

syncDirectory :: OsPath -> IO ()
syncDirectory dir = do
  dir' <- decodeFS dir
  h <-
    File.createFile
      dir'
      File.gENERIC_WRITE
      (File.fILE_SHARE_READ .|. File.fILE_SHARE_WRITE .|. File.fILE_SHARE_DELETE)
      Nothing
      File.oPEN_EXISTING
      File.fILE_FLAG_BACKUP_SEMANTICS
      Nothing
  File.flushFileBuffers h `finally` File.closeHandle h

readCold :: Int -> OsPath -> (ByteString -> IO ()) -> IO ()
readCold chunkSize path onChunk = do
  path' <- decodeFS path
  sector <- sectorSize path'
  attempt <- try @IOException (openUnbuffered path')
  case attempt of
    Left _ -> bracket (FileIO.openBinaryFile path ReadMode) hClose (feedHandle chunkSize onChunk)
    Right h -> feedUnbuffered sector h `finally` File.closeHandle h
  where
    openUnbuffered name =
      File.createFile
        name
        File.gENERIC_READ
        File.fILE_SHARE_READ
        Nothing
        File.oPEN_EXISTING
        (File.fILE_FLAG_NO_BUFFERING .|. File.fILE_FLAG_SEQUENTIAL_SCAN)
        Nothing
    feedUnbuffered sector h =
      let aligned = max sector (chunkSize - chunkSize `mod` sector)
      in allocaBytesAligned aligned sector $ \buf -> do
           let loop = do
                 n <- File.win32_ReadFile h buf (fromIntegral aligned) Nothing
                 unless (n == 0) $ do
                   bs <- BS.packCStringLen (castPtr buf, fromIntegral n)
                   onChunk bs
                 when (n == fromIntegral aligned) loop
           loop

sectorSize :: String -> IO Int
sectorSize path' = do
  answer <- try @IOException (File.getDiskFreeSpace (Just (takeDrive path')))
  pure $ case answer of
    Left _ -> 4096
    Right (_, bytesPerSector, _, _) -> max 4096 (fromIntegral bytesPerSector)
