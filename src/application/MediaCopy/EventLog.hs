{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.EventLog
  ( with
  , folder
  ) where

import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async, waitCatch)
import Effectful.Concurrent.STM (TBQueue, atomically, flushTBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Effectful.Exception qualified as E
import Effectful.Time (Time, currentTime)
import Effectful.Timeout (Timeout, timeout)
import Floreal.Commons.Xdg (Xdg, XdgDirectory (XdgState))
import System.OsPath (unsafeEncodeUtf, (</>))
import splice System.OsPath (osp)

import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Effects.AppDirectory qualified as AppDirectory
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem

-- | Open the log of a job in 'folder'. When the folder cannot be made or the file cannot be
-- opened, the job runs with no log.
with
  :: (FileSystem :> es, Xdg :> es, Time :> es, Timeout :> es, Concurrent :> es)
  => JobSpec
  -> Text
  -> (Maybe OsPath -> (JobEvent -> Eff es ()) -> Eff es a)
  -> Eff es a
with spec header use = do
  opened <- E.try @IOException (openLog spec header)
  case opened of
    Left _ -> use Nothing (\_ -> pure ())
    Right path -> do
      queue <- newTBQueueIO 1024
      writer <- async (drain path queue)
      let enqueue event = do
            now <- currentTime
            let line = stamp now <> " " <> display event
            atomically (isFullTBQueue queue >>= \full -> unless full (writeTBQueue queue (Just line)))
      let closeLog =
            void (timeout 5_000_000 (atomically (writeTBQueue queue Nothing) >> waitCatch writer))
      use (Just path) enqueue `E.finally` closeLog

drain :: (FileSystem :> es, Concurrent :> es) => OsPath -> TBQueue (Maybe Text) -> Eff es ()
drain path queue = do
  batch <- atomically ((:) <$> readTBQueue queue <*> flushTBQueue queue)
  let (waiting, rest) = span isJust batch
  unless (null waiting) $
    void (E.try @IOException (FileSystem.appendBytes path (encodeUtf8 (T.unlines (catMaybes waiting)))))
  when (null rest) (drain path queue)

-- | The folder of the job logs, @mediacopy3000/jobs@ in the XDG state directory. It is made when
-- it does not exist.
folder :: (Xdg :> es, FileSystem :> es) => Eff es OsPath
folder = AppDirectory.ensure XdgState [osp|mediacopy3000|] [osp|jobs|]

openLog :: (FileSystem :> es, Xdg :> es) => JobSpec -> Text -> Eff es OsPath
openLog spec header = do
  dir <- folder
  let path = dir </> unsafeEncodeUtf (T.unpack (jobStem spec <> ".log"))
  FileSystem.writeBytes path (encodeUtf8 ("MediaCopy 3000 event log\n" <> header <> "\n"))
  pure path

jobStem :: JobSpec -> Text
jobStem spec =
  T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" spec.createdAt)
    <> "-"
    <> T.show n
    <> "-"
    <> Job.label spec.job
    <> "-"
    <> display (Job.kind spec.job)
  where
    JobId.JobId n = spec.id

stamp :: UTCTime -> Text
stamp t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" t)
