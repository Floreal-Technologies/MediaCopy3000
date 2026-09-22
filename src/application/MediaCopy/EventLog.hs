{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.EventLog
  ( withEventLog
  ) where

import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM (TBQueue, atomically, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (IOException, bracketOnError, finally, try)
import Control.Monad (unless, void)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory.OsPath (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.File.OsPath qualified as FileIO
import System.IO (BufferMode (LineBuffering), Handle, IOMode (WriteMode), hClose, hSetBuffering, hSetEncoding, utf8)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import splice System.OsPath (osp)
import System.Timeout (timeout)

import MediaCopy.Domain.Job

withEventLog
  :: JobSpec
  -> Text
  -> (Maybe OsPath -> (JobEvent -> IO ()) -> IO a)
  -> IO a
withEventLog spec header use = do
  opened <- try @IOException (openLog spec)
  case opened of
    Left _ -> use Nothing (\_ -> pure ())
    Right (path, h) -> do
      queue <- newTBQueueIO 1024
      writer <- async (drain h queue)
      let enqueue event = do
            now <- getCurrentTime
            let line = stamp now <> " " <> display event
            atomically (isFullTBQueue queue >>= \full -> unless full (writeTBQueue queue (Just line)))
      let closeLog = do
            finished <- timeout 5_000_000 (atomically (writeTBQueue queue Nothing) >> waitCatch writer)
            case finished of
              Just _ -> void (try @IOException (hClose h))
              Nothing -> pure ()
      (writeHeader h header >> use (Just path) enqueue) `finally` closeLog

drain :: Handle -> TBQueue (Maybe Text) -> IO ()
drain h queue =
  atomically (readTBQueue queue) >>= \case
    Nothing -> pure ()
    Just line -> void (try @IOException (TIO.hPutStrLn h line)) >> drain h queue

openLog :: JobSpec -> IO (OsPath, Handle)
openLog spec = do
  dir <- getXdgDirectory XdgState [osp|mediacopy3000|] <&> (</> [osp|jobs|])
  createDirectoryIfMissing True dir
  let path = dir </> unsafeEncodeUtf (T.unpack (logName spec))
  bracketOnError (FileIO.openFile path WriteMode) hClose $ \h -> do
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    pure (path, h)

logName :: JobSpec -> Text
logName spec =
  T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" spec.createdAt)
    <> "-"
    <> jobNumber spec.jobId
    <> "-"
    <> jobLabel spec.job
    <> "-"
    <> display (jobKind spec.job)
    <> ".log"

jobNumber :: JobId -> Text
jobNumber (JobId n) = T.pack (show n)

writeHeader :: Handle -> Text -> IO ()
writeHeader h header = void (try @IOException (TIO.hPutStrLn h ("MediaCopy 3000 event log\n" <> header)))

stamp :: UTCTime -> Text
stamp t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" t)
