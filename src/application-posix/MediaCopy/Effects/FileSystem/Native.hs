{-# LANGUAGE CPP #-}

module MediaCopy.Effects.FileSystem.Native
  ( Entry (..)
  , statEntry
  , syncAndClose
  , syncDirectory
  , readCold
  ) where

import Control.Exception (IOException, bracketOnError, finally, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import System.IO (Handle, hClose)
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString (getOsString))
import System.Posix.Files.PosixString qualified as PosixFiles
import System.Posix.IO qualified as PosixIO
import System.Posix.IO.PosixString qualified as PosixPathIO
import System.Posix.Types (Fd (..))
import System.Posix.Unistd qualified as PosixUnistd

#if defined(darwin_HOST_OS)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
#else
import System.Posix.Fcntl qualified as Fcntl
#endif

import MediaCopy.Effects.FileSystem.Types (Entry (..), feedHandle)

statEntry :: OsPath -> IO Entry
statEntry path = do
  status <- PosixFiles.getSymbolicLinkStatus (getOsString path)
  pure
    Entry
      { isDirectory = PosixFiles.isDirectory status
      , isRegularFile = PosixFiles.isRegularFile status
      , isSymbolicLink = PosixFiles.isSymbolicLink status
      , size = fromIntegral (PosixFiles.fileSize status)
      }

syncAndClose :: Handle -> IO ()
syncAndClose h = do
  fd <- PosixIO.handleToFd h `onException` hClose h
  PosixUnistd.fileSynchronise fd `finally` PosixIO.closeFd fd

syncDirectory :: OsPath -> IO ()
syncDirectory dir = do
  fd <- PosixPathIO.openFd (getOsString dir) PosixIO.ReadOnly PosixIO.defaultFileFlags
  PosixUnistd.fileSynchronise fd `finally` PosixIO.closeFd fd

readCold :: Int -> OsPath -> (ByteString -> IO ()) -> IO ()
readCold chunkSize path onChunk = do
  h <- bracketOnError open PosixIO.closeFd (\fd -> void (try @IOException (bypassCache fd)) >> PosixIO.fdToHandle fd)
  feedAll h `finally` hClose h
  where
    open = PosixPathIO.openFd (getOsString path) PosixIO.ReadOnly PosixIO.defaultFileFlags
    feedAll h = feedHandle chunkSize onChunk h

#if defined(darwin_HOST_OS)
foreign import ccall unsafe "fcntl" c_fcntl :: CInt -> CInt -> CInt -> IO CInt

bypassCache :: Fd -> IO ()
bypassCache (Fd fd) = throwErrnoIfMinus1_ "fcntl F_NOCACHE" (c_fcntl fd 48 1)
#else

bypassCache :: Fd -> IO ()
bypassCache fd = Fcntl.fileAdvise fd 0 0 Fcntl.AdviceDontNeed
#endif
