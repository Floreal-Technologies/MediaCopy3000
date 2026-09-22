module MediaCopy.Effects.FileSystem
  ( defaultChunkSize
  , FileSystem (..)
  , ReadCache (..)
  , walk
  , freeSpaceOf
  , streamFile
  , writeTemps
  , publish
  , removeTemps
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
import MediaCopy.Domain.Plan (PlannedWrite (..))
import MediaCopy.Effects.FileSystem.Native (readCold, statEntry, syncAndClose, syncDirectory)
import MediaCopy.Effects.FileSystem.Types (Entry (..), feedHandle)

data ReadCache = FromCache | FromDevice
  deriving stock (Eq, Show)

data FileSystem :: Effect where
  Walk :: OsPath -> FileSystem m (Maybe Tree)
  FreeSpaceOf :: OsPath -> FileSystem m (Either Text Int64)
  StreamFile :: ReadCache -> OsPath -> (ByteString -> m ()) -> FileSystem m UTCTime
  WriteTemps :: OsPath -> Vector PlannedWrite -> m () -> (ByteString -> m ()) -> FileSystem m UTCTime
  Publish :: Vector PlannedWrite -> UTCTime -> FileSystem m ()
  RemoveTemps :: Vector PlannedWrite -> FileSystem m ()
  MtimeOf :: OsPath -> FileSystem m UTCTime
  ReadText :: OsPath -> FileSystem m (Maybe Text)
  ListHistory :: OsPath -> FileSystem m (Maybe (Vector OsPath))
  WriteTextAtomically :: OsPath -> Text -> FileSystem m ()
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

removeTemps :: (FileSystem :> es) => Vector PlannedWrite -> Eff es ()
removeTemps writes = send (RemoveTemps writes)

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

runFileSystemIO :: (IOE :> es) => Int -> Eff (FileSystem : es) a -> Eff es a
runFileSystemIO chunkSize =
  interpret $ \env -> \case
    Walk root -> liftIO (walkIO root)
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
    RemoveTemps writes -> liftIO (forM_ writes (\w -> removeFileIfExists w.temp))
    MtimeOf p -> liftIO (Dir.getModificationTime p)
    ReadText p -> liftIO (readTextIO p)
    ListHistory folder -> liftIO (listHistoryIO folder)
    WriteTextAtomically p t -> liftIO (writeTextAtomicallyIO p t)
    MakeDirectories dirs -> liftIO (V.mapM_ (\dir -> Dir.createDirectoryIfMissing True dir) dirs)

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
              unless entry.isRegularFile (ioError (userError ("not a regular file: " <> T.unpack (pathText full))))
              rel <- relPathIO root full
              pure ([(rel, entry.size :: FileSize)], [])

isIgnoredName :: OsPath -> Bool
isIgnoredName name = case decodeUtf name of
  Nothing -> False
  Just s -> T.pack s `V.elem` ignorePatterns

classifyChild :: OsPath -> OsPath -> IO (Maybe (OsPath, Entry))
classifyChild dir name
  | isIgnoredName name = pure Nothing
  | otherwise = do
      let full = dir </> name
      entry <- statEntry full
      when entry.isSymbolicLink (ioError (userError ("symbolic link or reparse point not supported: " <> T.unpack (pathText full))))
      pure (Just (full, entry))

relPathIO :: OsPath -> OsPath -> IO RelPath
relPathIO root full = either (\message -> ioError (userError (T.unpack message))) pure (relPathOf root full)

freeSpaceIO :: OsPath -> IO Int64
freeSpaceIO p = do
  raw <- decodeFS p
  avail <- getAvailSpace raw
  pure (fromIntegral avail)

defaultChunkSize :: Int
defaultChunkSize = 4 * 1024 * 1024

streamFileIO :: ReadCache -> Int -> OsPath -> (ByteString -> IO ()) -> IO UTCTime
streamFileIO mode chunkSize path onChunk = do
  case mode of
    FromCache -> bracket (FileIO.openBinaryFile path ReadMode) hClose feedAll
    FromDevice -> readCold chunkSize path onChunk
  Dir.getModificationTime path
  where
    feedAll h = feedHandle chunkSize onChunk h

writeTempsIO :: Int -> OsPath -> List PlannedWrite -> IO () -> (ByteString -> IO ()) -> IO UTCTime
writeTempsIO chunkSize source writes onFlush onChunk =
  withWriters temps fanOut `onException` traverse_ removeFileIfExists temps
  where
    temps = map (\w -> w.temp) writes
    fanOut handles = do
      mtime <- streamFileIO FromCache chunkSize source (\bs -> traverse_ (\h -> BS.hPut h bs) handles >> onChunk bs)
      onFlush
      pure mtime

withWriters :: List OsPath -> (List Handle -> IO a) -> IO a
withWriters paths use = go [] paths
  where
    go opened [] = use (reverse opened)
    go opened (p : ps) = withWriter p (\h -> go (h : opened) ps)

withWriter :: OsPath -> (Handle -> IO a) -> IO a
withWriter path use = do
  Dir.createDirectoryIfMissing True (takeDirectory path)
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
