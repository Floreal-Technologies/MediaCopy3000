--  | Our FileSystem abstraction. Different from "Effectful.FileSystem" because
-- we need multiple interpreters.
module MediaCopy.Effects.FileSystem
  ( defaultChunkSize
  , FileSystem (..)
  , ReadCache (..)
  , walk
  , freeSpaceOf
  , streamFile
  , writeTemps
  , publish
  , discard
  , mtimeOf
  , readText
  , listHistory
  , writeTextAtomically
  , makeDirectories
  , runFileSystemIO
  ) where

import Ascmhl.Layout (ascmhlDir)
import Ascmhl.Path (RelPath, pathText)
import Control.Exception hiding (displayException)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (forM_, traverse_)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (List, nub, sort, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Exception (displayException, trySync)
import System.Directory.OsPath qualified as Dir
import System.DiskSpace (getAvailSpace)
import System.File.OsPath qualified as FileIO
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, decodeFS, decodeUtf, takeDirectory, (</>))

import MediaCopy.Domain.FileSystem (Tree (..), ignorePatterns, partPath, relPathOf)
import MediaCopy.Domain.Job (FileSize)
import MediaCopy.Domain.Plan (PlannedWrite (..), WriteMode (..))
import MediaCopy.Effects.FileSystem.Native (readCold, statEntry, syncAndClose, syncDirectory)
import MediaCopy.Effects.FileSystem.Types (Entry (..), feedHandle)

-- | Where a read may come from. A read-back must come from the device, or it proves only the cache.
data ReadCache = FromCache | FromDevice
  deriving stock (Eq, Show)

data FileSystem :: Effect where
  -- | 'Nothing' when the folder does not exist.
  Walk :: OsPath -> FileSystem m (Maybe Tree)
  -- | The bytes free on the device that holds the path. 'Left' when the call fails. The path must exist.
  FreeSpaceOf :: OsPath -> FileSystem m (Either Text Int64)
  -- | Feeds every chunk to the hook, then answers the file's mtime.
  StreamFile :: ReadCache -> OsPath -> (ByteString -> m ()) -> FileSystem m UTCTime
  -- | Writes every chunk to each write's temp path, and feeds the same chunk to the second hook.
  -- A failure removes every temp it made, then rethrows. The first hook runs once: after the last
  -- chunk, before the writers close, with only the per-device synchronise left to follow. That
  -- moves no byte and can block for a long time, so the hook is the caller's one chance to say so.
  WriteTemps :: OsPath -> Vector PlannedWrite -> m () -> (ByteString -> m ()) -> FileSystem m UTCTime
  -- | Renames each temp to its final name and stamps the given mtime on the final name.
  Publish :: Vector PlannedWrite -> UTCTime -> FileSystem m ()
  -- | Removes the temp of each write, and the final of a 'WriteNew'. A name that is already gone is not an error.
  Discard :: Vector PlannedWrite -> FileSystem m ()
  MtimeOf :: OsPath -> FileSystem m UTCTime
  -- | 'Nothing' when the file is absent.
  ReadText :: OsPath -> FileSystem m (Maybe Text)
  -- | The file names under @\<folder\>\/ascmhl@. 'Nothing' when that directory is absent.
  ListHistory :: OsPath -> FileSystem m (Maybe (Vector OsPath))
  -- | Makes the parent directories, writes the part file beside the path, renames it over the path.
  WriteTextAtomically :: OsPath -> Text -> FileSystem m ()
  -- | Makes every directory given, parents included. One that exists is not an error.
  MakeDirectories :: Vector OsPath -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

walk :: (FileSystem :> es) => OsPath -> Eff es (Maybe Tree)
walk root = send (Walk root)

freeSpaceOf :: (FileSystem :> es) => OsPath -> Eff es (Either Text Int64)
freeSpaceOf p = send (FreeSpaceOf p)

streamFile :: (FileSystem :> es) => ReadCache -> OsPath -> (ByteString -> Eff es ()) -> Eff es UTCTime
streamFile mode path onChunk = send (StreamFile mode path onChunk)

writeTemps :: (FileSystem :> es) => OsPath -> Vector PlannedWrite -> Eff es () -> (ByteString -> Eff es ()) -> Eff es UTCTime
writeTemps source writes onFlush onChunk = send (WriteTemps source writes onFlush onChunk)

publish :: (FileSystem :> es) => Vector PlannedWrite -> UTCTime -> Eff es ()
publish writes mtime = send (Publish writes mtime)

discard :: (FileSystem :> es) => Vector PlannedWrite -> Eff es ()
discard writes = send (Discard writes)

mtimeOf :: (FileSystem :> es) => OsPath -> Eff es UTCTime
mtimeOf p = send (MtimeOf p)

readText :: (FileSystem :> es) => OsPath -> Eff es (Maybe Text)
readText p = send (ReadText p)

listHistory :: (FileSystem :> es) => OsPath -> Eff es (Maybe (Vector OsPath))
listHistory folder = send (ListHistory folder)

writeTextAtomically :: (FileSystem :> es) => OsPath -> Text -> Eff es ()
writeTextAtomically p t = send (WriteTextAtomically p t)

makeDirectories :: (FileSystem :> es) => Vector OsPath -> Eff es ()
makeDirectories dirs = send (MakeDirectories dirs)

-- | The chunk size is fixed for the life of one interpreter, so one job reads at one size.
runFileSystemIO :: (IOE :> es) => Int -> Eff (FileSystem : es) a -> Eff es a
runFileSystemIO chunkSize =
  interpret $ \env -> \case
    Walk root -> liftIO (walkIO root)
    -- The free-space call speaks to the device, so it is the one question here that can fail.
    FreeSpaceOf p -> trySync (liftIO (freeSpaceIO p)) <&> first (\e -> T.pack (displayException e))
    StreamFile mode path onChunk ->
      localSeqUnliftIO env (\unlift -> streamFileIO mode chunkSize path (\bs -> unlift (onChunk bs)))
    WriteTemps source writes onFlush onChunk ->
      localSeqUnliftIO env (\unlift -> writeTempsIO chunkSize source (V.toList writes) (unlift onFlush) (\bs -> unlift (onChunk bs)))
    Publish writes mtime -> liftIO $ do
      forM_ writes $ \w -> do
        Dir.renamePath w.temp w.final
        Dir.setModificationTime w.final mtime
      traverse_ syncDirectory (nub (map (\w -> takeDirectory w.final) (V.toList writes)))
    Discard writes -> liftIO $ forM_ writes $ \w -> do
      removeFileIfExists w.temp
      when (w.mode == WriteNew) (removeFileIfExists w.final)
    MtimeOf p -> liftIO (Dir.getModificationTime p)
    ReadText p -> liftIO (readTextIO p)
    ListHistory folder -> liftIO (listHistoryIO folder)
    WriteTextAtomically p t -> liftIO (writeTextAtomicallyIO p t)
    MakeDirectories dirs -> liftIO (V.mapM_ (\dir -> Dir.createDirectoryIfMissing True dir) dirs)

-- | The block size a writer buffers before it reaches the device.
writeBufferBytes :: Int
writeBufferBytes = 4 * 1024 * 1024

walkIO :: OsPath -> IO (Maybe Tree)
walkIO root = do
  exists <- Dir.doesDirectoryExist root
  if not exists
    then pure Nothing
    else do
      (files, dirs) <- descend root
      pure (Just Tree {files = V.fromList (sortOn fst files), dirs = V.fromList (sort dirs)})
  where
    descend dir = do
      -- A directory that vanished between the stat and the listing is an empty branch, not a fault.
      here <- Dir.doesDirectoryExist dir
      if not here
        then pure ([], [])
        else Dir.listDirectory dir >>= \names -> fmap mconcat (mapM (step dir) names)

    step dir name =
      classifyChild dir name >>= \case
        Nothing -> pure ([], [])
        Just (full, entry)
          | entry.isDirectory -> do
              (belowFiles, belowDirs) <- descend full
              rel <- relPathIO root full
              pure (belowFiles, rel : belowDirs)
          | otherwise -> do
              -- A FIFO, socket or device node is not copyable, so it must not reach the plan.
              unless entry.isRegularFile (ioError (userError ("not a regular file: " <> T.unpack (pathText full))))
              rel <- relPathIO root full
              pure ([(rel, entry.size :: FileSize)], [])

-- | A name that matches 'ignorePatterns' after a decode to 'Text'. A non-UTF-8 name is never ignored.
isIgnoredName :: OsPath -> Bool
isIgnoredName name = case decodeUtf name of
  Nothing -> False
  Just s -> T.pack s `V.elem` ignorePatterns

-- | A symlink is never followed and never skipped. It fails the whole job. 'Nothing' means the name is ignored.
-- One 'statEntry' per entry answers symlink, directory, regular file and size together.
classifyChild :: OsPath -> OsPath -> IO (Maybe (OsPath, Entry))
classifyChild dir name
  | isIgnoredName name = pure Nothing
  | otherwise = do
      let full = dir </> name
      entry <- statEntry full
      -- On Windows the flag also covers a junction, a mount point and a cloud placeholder, so the message names the class.
      when entry.isSymbolicLink (ioError (userError ("symbolic link or reparse point not supported: " <> T.unpack (pathText full))))
      pure (Just (full, entry))

relPathIO :: OsPath -> OsPath -> IO RelPath
relPathIO root full = either (\message -> ioError (userError (T.unpack message))) pure (relPathOf root full)

-- | 'getAvailSpace' speaks 'String'. The filesystem encoding is the round-trip that keeps the bytes.
freeSpaceIO :: OsPath -> IO Int64
freeSpaceIO p = do
  raw <- decodeFS p
  avail <- getAvailSpace raw
  pure (fromIntegral avail)

-- | How much one read asks for. Nothing in the engine chooses it; only an interpreter passes it.
defaultChunkSize :: Int
defaultChunkSize = 4 * 1024 * 1024

-- | 'FromDevice' reads past the cache where the volume allows it.
streamFileIO :: ReadCache -> Int -> OsPath -> (ByteString -> IO ()) -> IO UTCTime
streamFileIO mode chunkSize path onChunk = do
  case mode of
    FromCache -> bracket (FileIO.openBinaryFile path ReadMode) hClose feedAll
    FromDevice -> readCold chunkSize path onChunk
  -- The mtime is read after the bytes, so it describes the file the hook just saw.
  Dir.getModificationTime path
  where
    feedAll h = feedHandle chunkSize onChunk h

-- | A failure leaves no temp behind, so a retry never finds a half-written file.
writeTempsIO :: Int -> OsPath -> List PlannedWrite -> IO () -> (ByteString -> IO ()) -> IO UTCTime
writeTempsIO chunkSize source writes onFlush onChunk =
  withWriters temps fanOut `onException` traverse_ removeFileIfExists temps
  where
    temps = map (\w -> w.temp) writes
    -- The hook goes here and not after 'withWriters', because the close of every writer is what the
    -- unwind of this bracket does.
    fanOut handles = do
      mtime <- streamFileIO FromCache chunkSize source (\bs -> traverse_ (\h -> BS.hPut h bs) handles >> onChunk bs)
      onFlush
      pure mtime

-- | This code opens the paths in order. A failure on one path closes the handles that are already open.
withWriters :: List OsPath -> (List Handle -> IO a) -> IO a
withWriters paths use = go [] paths
  where
    go opened [] = use (reverse opened)
    go opened (p : ps) = withWriter p (\h -> go (h : opened) ps)

-- | A writer synchronises to the device before it closes, so a copy is on the disk before
-- anything reads it back.
withWriter :: OsPath -> (Handle -> IO a) -> IO a
withWriter path use = do
  Dir.createDirectoryIfMissing True (takeDirectory path)
  -- A writer that failed is about to be deleted, so it closes without a sync. Which platform owns
  -- the close on success differs; 'syncAndClose' holds that difference.
  bracketOnError open hClose $ \h -> do
    result <- use h
    syncAndClose h
    pure result
  where
    open = do
      h <- FileIO.openBinaryFile path WriteMode
      hSetBuffering h (BlockBuffering (Just writeBufferBytes))
      pure h

readTextIO :: OsPath -> IO (Maybe Text)
readTextIO p = do
  exists <- Dir.doesFileExist p
  if not exists
    then pure Nothing
    else do
      bytes <- FileIO.readFile' p
      case TE.decodeUtf8' bytes of
        Left err -> ioError (userError (T.unpack (pathText p) <> ": " <> show err))
        Right t -> pure (Just t)

listHistoryIO :: OsPath -> IO (Maybe (V.Vector OsPath))
listHistoryIO folder = do
  let dir = ascmhlDir folder
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then pure Nothing
    else Dir.listDirectory dir <&> sort <&> V.fromList <&> Just

-- | The rename puts the text in place, so no reader ever sees a half-written file.
writeTextAtomicallyIO :: OsPath -> Text -> IO ()
writeTextAtomicallyIO path text = do
  let temp = partPath path
  withWriter temp (\h -> BS.hPut h (TE.encodeUtf8 text))
  Dir.renamePath temp path
  syncDirectory (takeDirectory path)

removeFileIfExists :: OsPath -> IO ()
removeFileIfExists p =
  Dir.removeFile p `catch` \e ->
    if isDoesNotExistError e then pure () else throwIO e
