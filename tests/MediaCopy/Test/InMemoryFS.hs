{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Test.InMemoryFS
  ( MemFS (..)
  , emptyMemFS
  , withFile
  , withTextFile
  , withDir
  , withCorruptRead
  , withFailOnOpen
  , withFailOnRename
  , withFailOnManifestWrite
  , withFreeBytes
  , withUnreadableFreeSpace
  , epoch
  , runFileSystemMem
  , slashedPath
  ) where

import Ascmhl.Layout (ascmhlDir)
import Ascmhl.Path (RelPath, mkRelPath, pathText)
import Control.Monad (when)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.IORef
import Data.Int (Int64)
import Data.List (List, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Exception (onException)
import System.OsPath (OsPath, decodeUtf, dropTrailingPathSeparator, makeRelative, splitDirectories)
import splice System.OsPath (osp)
import System.OsString (OsChar, isPrefixOf, unsafeFromChar)
import System.OsString qualified as OsString

import MediaCopy.Domain.FileSystem (ignorePatterns, relPathOf)
import MediaCopy.Domain.FileSystem qualified as FS
import MediaCopy.Domain.Job (FileSize)
import MediaCopy.Domain.Plan (PlannedWrite (..))
import MediaCopy.Effects.FileSystem (FileSystem (..))

data MemFS = MemFS
  { files :: Map OsPath (ByteString, UTCTime)
  , corruptOnRead :: Set OsPath
  , failOnOpen :: Set OsPath
  , failOnRename :: Set OsPath
  , failOnManifestWrite :: Bool
  , freeBytes :: Int64
  , freeSpaceFails :: Bool
  , dirs :: Set OsPath
  }

emptyMemFS :: MemFS
emptyMemFS =
  MemFS
    { files = Map.empty
    , dirs = Set.empty
    , corruptOnRead = Set.empty
    , failOnOpen = Set.empty
    , failOnRename = Set.empty
    , failOnManifestWrite = False
    , freeBytes = maxBound
    , freeSpaceFails = False
    }

withFile :: OsPath -> ByteString -> MemFS -> MemFS
withFile path content fs = fs {files = Map.insert (slashedPath path) (content, epoch) fs.files}

withTextFile :: OsPath -> Text -> MemFS -> MemFS
withTextFile path content = withFile path (TE.encodeUtf8 content)

withDir :: OsPath -> MemFS -> MemFS
withDir path fs = fs {dirs = Set.insert (slashedPath (dropTrailingPathSeparator path)) fs.dirs}

withCorruptRead :: OsPath -> MemFS -> MemFS
withCorruptRead path fs = fs {corruptOnRead = Set.insert (slashedPath path) fs.corruptOnRead}

withFailOnOpen :: OsPath -> MemFS -> MemFS
withFailOnOpen path fs = fs {failOnOpen = Set.insert (slashedPath path) fs.failOnOpen}

withFailOnRename :: OsPath -> MemFS -> MemFS
withFailOnRename path fs = fs {failOnRename = Set.insert (slashedPath path) fs.failOnRename}

withFailOnManifestWrite :: MemFS -> MemFS
withFailOnManifestWrite fs = fs {failOnManifestWrite = True}

slashedPath :: OsPath -> OsPath
slashedPath = OsString.map (\c -> if c == backslash then slash else c)
  where
    backslash = unsafeFromChar '\\' :: OsChar
    slash = unsafeFromChar '/'

slashedWrite :: PlannedWrite -> PlannedWrite
slashedWrite w = PlannedWrite {temp = slashedPath w.temp, final = slashedPath w.final, mode = w.mode}

withFiles :: Map OsPath (ByteString, UTCTime) -> MemFS -> MemFS
withFiles m fs = fs {files = m}

withFreeBytes :: Int64 -> MemFS -> MemFS
withFreeBytes n fs = fs {freeBytes = n}

withUnreadableFreeSpace :: MemFS -> MemFS
withUnreadableFreeSpace fs = fs {freeSpaceFails = True}

memChunkBytes :: Int
memChunkBytes = 4096

chunksOf :: ByteString -> List ByteString
chunksOf bs
  | BS.null bs = []
  | otherwise = let (piece, rest) = BS.splitAt memChunkBytes bs in piece : chunksOf rest

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

owner :: Text
owner = "MediaCopy.Test.InMemoryFS"

runFileSystemMem :: (IOE :> es) => IORef MemFS -> Eff (FileSystem : es) a -> Eff es a
runFileSystemMem fsRef =
  interpret $ \env -> \case
    Walk root -> liftIO (walkMem fsRef (slashedPath root))
    FreeSpaceOf _ -> liftIO $ do
      fs <- readIORef fsRef
      pure (if fs.freeSpaceFails then Left (owner <> ": free space unreadable") else Right fs.freeBytes)
    StreamFile _ (slashedPath -> path) onChunk -> localSeqUnlift env $ \unlift -> do
      content <- liftIO (readWholeMem fsRef path)
      forM_ (chunksOf content) (\chunk -> unlift (onChunk chunk))
      liftIO (mtimeMem fsRef path)
    WriteTemps (slashedPath -> source) (V.map slashedWrite -> writes) onFlush onChunk -> localSeqUnlift env $ \unlift -> do
      liftIO (forM_ writes (\w -> failIfInjected fsRef w.temp))
      content <- liftIO (readWholeMem fsRef source)
      liftIO (forM_ writes (\w -> modifyIORef' fsRef (withFile w.temp content)))
      let feedAndFlush = do
            forM_ (chunksOf content) (\chunk -> unlift (onChunk chunk))
            unlift onFlush
            liftIO (mtimeMem fsRef source)
      feedAndFlush `onException` liftIO (forM_ writes (\w -> modifyIORef' fsRef (\fs -> withFiles (Map.delete w.temp fs.files) fs)))
    Publish (V.map slashedWrite -> writes) mtime -> liftIO $ forM_ writes $ \w -> do
      failIfRenameInjected fsRef w.final
      renameMem fsRef w.temp w.final mtime
    RemoveTemps (V.map slashedWrite -> writes) -> liftIO $ forM_ writes $ \w ->
      modifyIORef' fsRef (\fs -> withFiles (Map.delete w.temp fs.files) fs)
    MtimeOf (slashedPath -> p) -> liftIO (mtimeMem fsRef p)
    ReadText (slashedPath -> p) -> liftIO $ do
      fs <- readIORef fsRef
      traverse (\entry -> decodeTextMem p (fst entry)) (Map.lookup p fs.files)
    ListHistory (slashedPath -> folder) -> liftIO (listHistoryMem folder <$> readIORef fsRef)
    WriteTextAtomically p t -> liftIO $ do
      fs <- readIORef fsRef
      when fs.failOnManifestWrite (ioError (userError "injected manifest write"))
      modifyIORef' fsRef (withTextFile p t)
    MakeDirectories dirs -> liftIO (V.mapM_ (\dir -> modifyIORef' fsRef (withDir dir)) dirs)

failIfInjected :: IORef MemFS -> OsPath -> IO ()
failIfInjected fsRef p = do
  fs <- readIORef fsRef
  when (p `Set.member` fs.failOnOpen) (ioError (userError "injected"))

failIfRenameInjected :: IORef MemFS -> OsPath -> IO ()
failIfRenameInjected fsRef p = do
  fs <- readIORef fsRef
  when (p `Set.member` fs.failOnRename) (ioError (userError "injected rename"))

decodeTextMem :: OsPath -> ByteString -> IO Text
decodeTextMem p bytes = case TE.decodeUtf8' bytes of
  Left err -> ioError (userError (T.unpack (pathText p) <> ": " <> show err))
  Right t -> pure t

readWholeMem :: IORef MemFS -> OsPath -> IO ByteString
readWholeMem fsRef p = do
  failIfInjected fsRef p
  fs <- readIORef fsRef
  case Map.lookup p fs.files of
    Nothing -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (pathText p)))
    Just (content, _) ->
      pure (if p `Set.member` fs.corruptOnRead then corruptFirstByte content else content)

corruptFirstByte :: ByteString -> ByteString
corruptFirstByte bs = case BS.uncons bs of
  Nothing -> bs
  Just (b, rest) -> BS.cons (xor b 0xFF) rest

renameMem :: IORef MemFS -> OsPath -> OsPath -> UTCTime -> IO ()
renameMem fsRef src dst mtime = do
  fs <- readIORef fsRef
  case Map.lookup src fs.files of
    Nothing -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (pathText src)))
    Just (content, _) -> writeIORef fsRef (withFiles (fs.files & Map.delete src & Map.insert dst (content, mtime)) fs)

mtimeMem :: IORef MemFS -> OsPath -> IO UTCTime
mtimeMem fsRef p = do
  fs <- readIORef fsRef
  case Map.lookup p fs.files of
    Just entry -> pure (snd entry)
    Nothing -> case belowMTimes fs of
      [] -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (pathText p)))
      times -> pure (maximum times)
  where
    belowMTimes fs =
      Map.toList fs.files
        & filter (\entry -> withTrailingSlash p `isPrefixOf` fst entry)
        & map (\entry -> snd (snd entry))

withTrailingSlash :: OsPath -> OsPath
withTrailingSlash p = dropTrailingPathSeparator p <> [osp|/|]

hasChildren :: OsPath -> MemFS -> Bool
hasChildren p fs = any (\k -> withTrailingSlash p `isPrefixOf` k) (Map.keys fs.files)

dirExists :: OsPath -> MemFS -> Bool
dirExists p fs = dropTrailingPathSeparator p `Set.member` fs.dirs || hasChildren p fs

ignoreNames :: Set Text
ignoreNames = Set.fromList (V.toList ignorePatterns)

isIgnoredComponent :: OsPath -> Bool
isIgnoredComponent component = case decodeUtf component of
  Nothing -> False
  Just s -> T.pack s `Set.member` ignoreNames

walkMem :: IORef MemFS -> OsPath -> IO (Maybe FS.Tree)
walkMem fsRef root = do
  fs <- readIORef fsRef
  if not (dirExists root fs)
    then pure Nothing
    else do
      files <- traverse toEntry (filter underRoot (Map.toList fs.files))
      let sorted = V.fromList (sortOn fst files)
      pure (Just FS.Tree {FS.files = sorted, FS.dirs = dirsOf sorted})
  where
    prefix = withTrailingSlash root
    underRoot (p, _) =
      prefix `isPrefixOf` p && not (any isIgnoredComponent (splitDirectories (makeRelative root p)))
    toEntry (p, (bs, _)) = do
      rel <- either (\message -> ioError (userError (T.unpack message))) pure (relPathOf root p)
      pure (rel, fromIntegral (BS.length bs) :: FileSize)

dirsOf :: Vector (RelPath, FileSize) -> Vector RelPath
dirsOf files =
  files
    & V.toList
    & concatMap (\entry -> ancestorsOf (display (fst entry)))
    & Set.fromList
    & Set.toList
    & mapMaybe mkRelPath
    & sort
    & V.fromList
  where
    ancestorsOf :: Text -> List Text
    ancestorsOf p =
      T.splitOn "/" p
        & init
        & scanl1 (\acc component -> acc <> "/" <> component)

listHistoryMem :: OsPath -> MemFS -> Maybe (Vector OsPath)
listHistoryMem folder fs
  | not (dirExists dir fs) = Nothing
  | otherwise = Just (V.fromList (Set.toList (Set.fromList names)))
  where
    dir = slashedPath (ascmhlDir folder)
    prefix = withTrailingSlash dir
    names =
      Map.keys fs.files
        & filter (\k -> prefix `isPrefixOf` k)
        & mapMaybe firstComponent
    firstComponent k = case splitDirectories (makeRelative dir k) of
      component : _ -> Just component
      [] -> Nothing
