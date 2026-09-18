{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The EventLog sends progress messages to the 'XdgState' directory,
-- with one log file per job
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

-- | Opens the job's log, writes the header, hands back the path and a line writer. The caller's
-- thread only enqueues a line, and one writer thread holds the handle, so a slow or hung disk
-- cannot stall the copy. The time is read at enqueueing. The close waits five seconds, then leaves
-- a stuck writer alone.
withEventLog
  :: JobSpec
  -> Text
  -> (Maybe OsPath -> (JobEvent -> IO ()) -> IO a)
  -> IO a
withEventLog spec header use = do
  opened <- try @IOException (openLog spec)
  case opened of
    -- File opening errors are silently discarded at the moment.
    Left _ -> use Nothing (\_ -> pure ())
    Right (path, h) -> do
      queue <- newTBQueueIO 1024
      writer <- async (drain h queue)
      -- A full queue drops the line rather than holding the thread that reports the event.
      let enqueue event = do
            now <- getCurrentTime
            let line = stamp now <> " " <> display event
            atomically (isFullTBQueue queue >>= \full -> unless full (writeTBQueue queue (Just line)))
      -- A writer that does not finish in five seconds is left behind with its
      -- handle; process exit reclaims the descriptor.
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
    -- 'file-io' hands back a byte handle, which would cut every character above U+00FF.
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

-- | A failed write is dropped. The engine's sink calls this, and a full disk must not fail a copy.
writeHeader :: Handle -> Text -> IO ()
writeHeader h header = void (try @IOException (TIO.hPutStrLn h ("MediaCopy 3000 event log\n" <> header)))

stamp :: UTCTime -> Text
stamp t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" t)
