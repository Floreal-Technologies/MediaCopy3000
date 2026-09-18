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
  , listDirectory
  , doesFileExist
  , doesDirectoryExist
  , readBytes
  , appendBytes
  , writeBytes
  , runIO
  ) where

import Ascmhl.Layout qualified as Layout
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Control.Exception
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Exception (trySync)
import System.Directory.OsPath qualified as Dir
import System.DiskSpace (getAvailSpace)
import System.File.OsPath qualified as FileIO
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.OsPath (decodeFS, takeDirectory, (</>))

import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.Part qualified as Part
import MediaCopy.Domain.PlannedWrite (PlannedWrite)
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.Tree (Tree)
import MediaCopy.Domain.Tree qualified as Tree
import MediaCopy.Effects.DirEntry (DirEntry)
import MediaCopy.Effects.DirEntry qualified as DirEntry
import MediaCopy.Effects.FileSystem.Native qualified as Native

data ReadCache = FromCache | FromDevice
  deriving stock (Eq, Show)

data FileSystem :: Effect where
  Walk :: OsPath -> FileSystem m (Maybe Tree)
  FreeSpace :: OsPath -> FileSystem m (Maybe Int64)
  StreamFile :: ReadCache -> OsPath -> (ByteString -> m ()) -> FileSystem m UTCTime
  WriteTemps :: OsPath -> Vector PlannedWrite -> m () -> (ByteString -> m ()) -> FileSystem m UTCTime
  Publish :: Vector PlannedWrite -> UTCTime -> FileSystem m ()
  RemoveTemps :: Vector PlannedWrite -> FileSystem m ()
  Mtime :: OsPath -> FileSystem m UTCTime
  ReadText :: OsPath -> FileSystem m (Maybe Text)
  ListHistory :: OsPath -> FileSystem m (Maybe (Vector OsPath))
  WriteTextAtomically :: OsPath -> Text -> FileSystem m ()
  MakeDirectories :: Vector OsPath -> FileSystem m ()
  ListDirectory :: OsPath -> FileSystem m (Maybe (List OsPath))
  DoesFileExist :: OsPath -> FileSystem m Bool
  DoesDirectoryExist :: OsPath -> FileSystem m Bool
  ReadBytes :: OsPath -> FileSystem m ByteString
  AppendBytes :: OsPath -> ByteString -> FileSystem m ()
  WriteBytes :: OsPath -> ByteString -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

walk :: (FileSystem :> es) => OsPath -> Eff es (Maybe Tree)
walk root = send (Walk root)

freeSpaceOf :: (FileSystem :> es) => OsPath -> Eff es (Maybe Int64)
freeSpaceOf p = send (FreeSpace p)

streamFile :: (FileSystem :> es) => ReadCache -> OsPath -> (ByteString -> Eff es ()) -> Eff es UTCTime
streamFile mode path onChunk = send (StreamFile mode path onChunk)

writeTemps :: (FileSystem :> es) => OsPath -> Vector PlannedWrite -> Eff es () -> (ByteString -> Eff es ()) -> Eff es UTCTime
writeTemps source writes onFlush onChunk = send (WriteTemps source writes onFlush onChunk)

publish :: (FileSystem :> es) => Vector PlannedWrite -> UTCTime -> Eff es ()
publish writes mtime = send (Publish writes mtime)

removeTemps :: (FileSystem :> es) => Vector PlannedWrite -> Eff es ()
removeTemps writes = send (RemoveTemps writes)

mtimeOf :: (FileSystem :> es) => OsPath -> Eff es UTCTime
mtimeOf p = send (Mtime p)

readText :: (FileSystem :> es) => OsPath -> Eff es (Maybe Text)
readText p = send (ReadText p)

listHistory :: (FileSystem :> es) => OsPath -> Eff es (Maybe (Vector OsPath))
listHistory folder = send (ListHistory folder)

writeTextAtomically :: (FileSystem :> es) => OsPath -> Text -> Eff es ()
writeTextAtomically p t = send (WriteTextAtomically p t)

makeDirectories :: (FileSystem :> es) => Vector OsPath -> Eff es ()
makeDirectories dirs = send (MakeDirectories dirs)

listDirectory :: (FileSystem :> es) => OsPath -> Eff es (Maybe (List OsPath))
listDirectory dir = send (ListDirectory dir)

doesFileExist :: (FileSystem :> es) => OsPath -> Eff es Bool
doesFileExist p = send (DoesFileExist p)

doesDirectoryExist :: (FileSystem :> es) => OsPath -> Eff es Bool
doesDirectoryExist p = send (DoesDirectoryExist p)

readBytes :: (FileSystem :> es) => OsPath -> Eff es ByteString
readBytes p = send (ReadBytes p)

appendBytes :: (FileSystem :> es) => OsPath -> ByteString -> Eff es ()
appendBytes p bytes = send (AppendBytes p bytes)

writeBytes :: (FileSystem :> es) => OsPath -> ByteString -> Eff es ()
writeBytes p bytes = send (WriteBytes p bytes)

runIO :: (IOE :> es) => Int -> Eff (FileSystem : es) a -> Eff es a
runIO chunkSize =
  interpret $ \env -> \case
    Walk root -> liftIO (walkIO root)
    FreeSpace p -> trySync (liftIO (freeSpaceIO p)) <&> either (const Nothing) Just
    StreamFile mode path onChunk ->
      localSeqUnliftIO env (\unlift -> streamFileIO mode chunkSize path (unlift . onChunk))
    WriteTemps source writes onFlush onChunk ->
      localSeqUnliftIO env (\unlift -> writeTempsIO chunkSize source (V.toList writes) (unlift onFlush) (unlift . onChunk))
    Publish writes mtime -> liftIO $ do
      forM_ writes $ \w -> do
        Dir.renamePath w.temp w.final
        Dir.setModificationTime w.final mtime
      traverse_ Native.syncDirectory (nub (map (\w -> takeDirectory w.final) (V.toList writes)))
    RemoveTemps writes -> liftIO (forM_ writes (\w -> removeFileIfExists w.temp))
    Mtime p -> liftIO (Dir.getModificationTime p)
    ReadText p -> liftIO (readTextIO p)
    ListHistory folder -> liftIO (listHistoryIO folder)
    WriteTextAtomically p t -> liftIO (writeTextAtomicallyIO p t)
    MakeDirectories dirs -> liftIO (V.mapM_ (Dir.createDirectoryIfMissing True) dirs)
    ListDirectory dir -> liftIO (try @IOException (Dir.listDirectory dir)) <&> either (const Nothing) (Just . sort)
    DoesFileExist p -> liftIO (Dir.doesFileExist p)
    DoesDirectoryExist p -> liftIO (Dir.doesDirectoryExist p)
    ReadBytes p -> liftIO (FileIO.readFile' p)
    AppendBytes p bytes -> liftIO (FileIO.appendFile' p bytes)
    WriteBytes p bytes -> liftIO (FileIO.writeFile' p bytes)

walkIO :: OsPath -> IO (Maybe Tree)
walkIO root = do
  exists <- Dir.doesDirectoryExist root
  if not exists
    then pure Nothing
    else do
      (files, dirs) <- descend root
      pure (Just Tree.Tree {files = V.fromList (sortOn fst files), dirs = V.fromList (sort dirs)})
  where
    descend dir = Dir.listDirectory dir >>= \names -> fmap mconcat (forM names (step dir))

    step dir name =
      classifyChild dir name >>= \case
        Nothing -> pure ([], [])
        Just (full, entry)
          | entry.isDirectory -> do
              (belowFiles, belowDirs) <- descend full
              rel <- relPathIO root full
              pure (belowFiles, rel : belowDirs)
          | otherwise -> do
              unless entry.isRegularFile (ioError (userError ("not a regular file: " <> T.unpack (OsPath.toText full))))
              rel <- relPathIO root full
              pure ([(rel, entry.size :: FileSize)], [])

classifyChild :: OsPath -> OsPath -> IO (Maybe (OsPath, DirEntry))
classifyChild dir name
  | Tree.isIgnoredName name = pure Nothing
  | otherwise = do
      let full = dir </> name
      entry <- Native.statEntry full
      when entry.isSymbolicLink (ioError (userError ("symbolic link or reparse point not supported: " <> T.unpack (OsPath.toText full))))
      pure (Just (full, entry))

relPathIO :: OsPath -> OsPath -> IO RelPath
relPathIO root full = either (\message -> message & T.unpack & userError & ioError) pure (Tree.relPathOf root full)

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
    FromDevice -> Native.readCold chunkSize path onChunk
  Dir.getModificationTime path
  where
    feedAll h = DirEntry.feedHandle chunkSize onChunk h

writeTempsIO :: Int -> OsPath -> List PlannedWrite -> IO () -> (ByteString -> IO ()) -> IO UTCTime
writeTempsIO chunkSize source writes onFlush onChunk =
  withWriters temps fanOut `onException` traverse_ removeFileIfExists temps
  where
    temps = map (.temp) writes
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
    Native.syncAndClose h
    pure result
  where
    open = do
      h <- FileIO.openBinaryFile path WriteMode
      hSetBuffering h (BlockBuffering (Just defaultChunkSize))
      pure h

readTextIO :: OsPath -> IO (Maybe Text)
readTextIO p = do
  exists <- Dir.doesFileExist p
  if not exists
    then pure Nothing
    else do
      bytes <- FileIO.readFile' p
      case TE.decodeUtf8' bytes of
        Left err -> ioError (userError (T.unpack (OsPath.toText p) <> ": " <> show err))
        Right t -> pure (Just t)

listHistoryIO :: OsPath -> IO (Maybe (V.Vector OsPath))
listHistoryIO folder = do
  let dir = Layout.dir folder
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then pure Nothing
    else Dir.listDirectory dir <&> sort <&> V.fromList <&> Just

writeTextAtomicallyIO :: OsPath -> Text -> IO ()
writeTextAtomicallyIO path text = do
  let temp = Part.path path
  withWriter temp (\h -> BS.hPut h (TE.encodeUtf8 text))
  Dir.renamePath temp path
  Native.syncDirectory (takeDirectory path)

removeFileIfExists :: OsPath -> IO ()
removeFileIfExists p =
  Dir.removeFile p `catch` \e ->
    if isDoesNotExistError e then pure () else throwIO e
