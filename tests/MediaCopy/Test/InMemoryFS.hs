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
  , slashedPath
  , runFileSystemMem
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

-- The port's own records carry the names 'files' and 'freeBytes' too. The qualified import keeps
-- them out of this module's unqualified scope, so a 'MemFS' update names one record only.
import MediaCopy.Domain.FileSystem (ignorePatterns, relPathOf)
import MediaCopy.Domain.FileSystem qualified as FS
import MediaCopy.Domain.Job (FileSize)
import MediaCopy.Domain.Plan (PlannedWrite (..), WriteMode (..))
import MediaCopy.Effects.FileSystem (FileSystem (..))

-- | The 'files' map is the tree. A directory exists when some key has it as a prefix, or when 'dirs' names it.
data MemFS = MemFS
  { files :: Map OsPath (ByteString, UTCTime)
  , corruptOnRead :: Set OsPath
  -- ^ A read of these paths flips the first byte of the first chunk.
  , failOnOpen :: Set OsPath
  -- ^ A read of these paths, or a temp write to them, raises an 'IOError'.
  , failOnRename :: Set OsPath
  -- ^ A rename onto these final paths raises an 'IOError' before it moves anything.
  , failOnManifestWrite :: Bool
  -- ^ When set, every atomic text write raises, which fails the manifest and the carried history.
  , freeBytes :: Int64
  , freeSpaceFails :: Bool
  -- ^ When set, the free-space call answers 'Left', as a device that will not say does.
  , dirs :: Set OsPath
  -- ^ Directories that exist with no file below them. A directory with files needs no entry.
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

-- | Adds a file with the given content. The mtime is the Unix epoch.
withFile :: OsPath -> ByteString -> MemFS -> MemFS
withFile path content fs = fs {files = Map.insert (slashedPath path) (content, epoch) fs.files}

-- | Adds a UTF-8 encoded text file. The mtime is the Unix epoch.
withTextFile :: OsPath -> Text -> MemFS -> MemFS
withTextFile path content = withFile path (TE.encodeUtf8 content)

-- | Adds a directory that holds nothing, the one state the files map cannot imply.
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

-- | Every path the double sees is written with @/@, whatever the host separator is. Production
-- joins with the host's separator, so on Windows a path arrives as @/ssd\\media-source\\A@ while
-- the test seeded @/ssd/media-source/A@. One spelling for both, or no key matches there.
slashedPath :: OsPath -> OsPath
slashedPath = OsString.map (\c -> if c == backslash then slash else c)
  where
    backslash = unsafeFromChar '\\' :: OsChar
    slash = unsafeFromChar '/'

-- | The same spelling rule applied to both names of a planned write.
slashedWrite :: PlannedWrite -> PlannedWrite
slashedWrite w = PlannedWrite {temp = slashedPath w.temp, final = slashedPath w.final, mode = w.mode}

-- | This function sets the map. 'Tree' carries a field of the same name, so an update
-- written where the record's type is not yet known is ambiguous. This signature names it once.
withFiles :: Map OsPath (ByteString, UTCTime) -> MemFS -> MemFS
withFiles m fs = fs {files = m}

-- | This function sets the field. Here 'MemFS' is the only record in scope that carries it.
withFreeBytes :: Int64 -> MemFS -> MemFS
withFreeBytes n fs = fs {freeBytes = n}

withUnreadableFreeSpace :: MemFS -> MemFS
withUnreadableFreeSpace fs = fs {freeSpaceFails = True}

-- | The double feeds a file in pieces of this size, so a hook sees more than one chunk for a file past it.
memChunkBytes :: Int
memChunkBytes = 4096

chunksOf :: ByteString -> List ByteString
chunksOf bs
  | BS.null bs = []
  | otherwise = let (piece, rest) = BS.splitAt memChunkBytes bs in piece : chunksOf rest

-- | The Unix epoch, the mtime every in-memory file carries.
epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

owner :: Text
owner = "MediaCopy.Test.InMemoryFS"

-- | Every path reaches the map through 'slashedPath', so one spelling serves both hosts.
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
      -- The port promises that a failure removes every temp the call made, so the double keeps that
      -- promise. The flush hook runs where the real interpreter runs it: after the last chunk,
      -- inside the same guard, between the last write and the close. The mtime is read inside the
      -- guard for the same reason, because 'streamFileIO' reads it inside the bracket that removes
      -- the temps.
      let feedAndFlush = do
            forM_ (chunksOf content) (\chunk -> unlift (onChunk chunk))
            unlift onFlush
            liftIO (mtimeMem fsRef source)
      feedAndFlush `onException` liftIO (forM_ writes (\w -> modifyIORef' fsRef (\fs -> withFiles (Map.delete w.temp fs.files) fs)))
    Publish (V.map slashedWrite -> writes) mtime -> liftIO $ forM_ writes $ \w -> do
      failIfRenameInjected fsRef w.final
      renameMem fsRef w.temp w.final mtime
    Discard (V.map slashedWrite -> writes) -> liftIO $ forM_ writes $ \w ->
      modifyIORef' fsRef (\fs -> withFiles (fs.files & Map.delete w.temp & (if w.mode == WriteNew then Map.delete w.final else id)) fs)
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

-- | The real interpreter fails a read that is not UTF-8 and names the path; the double does the same.
decodeTextMem :: OsPath -> ByteString -> IO Text
decodeTextMem p bytes = case TE.decodeUtf8' bytes of
  Left err -> ioError (userError (T.unpack (pathText p) <> ": " <> show err))
  Right t -> pure t

-- | The whole file. A corrupt path loses its first byte to a flip, which 'chunksOf' then hands the
-- hook as a corrupt first chunk.
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

-- | The double has no directory entries, so a directory's mtime is the newest mtime below it. The real interpreter returns the directory's own mtime.
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

-- | The keys of the double are written with @/@, whatever the host separator is. The prefix that
-- selects the files under a directory must end with the same character. Otherwise, on Windows,
-- @/vol\\@ never matches @/vol/a.mxf@ and every directory reads as missing.
withTrailingSlash :: OsPath -> OsPath
withTrailingSlash p = dropTrailingPathSeparator p <> [osp|/|]

hasChildren :: OsPath -> MemFS -> Bool
hasChildren p fs = any (\k -> withTrailingSlash p `isPrefixOf` k) (Map.keys fs.files)

dirExists :: OsPath -> MemFS -> Bool
dirExists p fs = dropTrailingPathSeparator p `Set.member` fs.dirs || hasChildren p fs

-- | This set must match 'ignorePatterns'. The in-memory walk then skips exactly what the real one skips.
ignoreNames :: Set Text
ignoreNames = Set.fromList (V.toList ignorePatterns)

isIgnoredComponent :: OsPath -> Bool
isIgnoredComponent component = case decodeUtf component of
  Nothing -> False
  Just s -> T.pack s `Set.member` ignoreNames

-- | 'Nothing' when the root neither holds a key nor sits in 'dirs'. A root in 'dirs' alone walks to an empty tree.
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

-- | 'Nothing' when @\<folder\>\/ascmhl@ neither holds a key nor sits in 'dirs'; an empty one lists as empty, as on disk.
listHistoryMem :: OsPath -> MemFS -> Maybe (Vector OsPath)
listHistoryMem folder fs
  | not (dirExists dir fs) = Nothing
  | otherwise = Just (V.fromList (Set.toList (Set.fromList names)))
  where
    -- 'ascmhlDir' joins with the host separator, so the result is respelled before it selects keys.
    dir = slashedPath (ascmhlDir folder)
    prefix = withTrailingSlash dir
    names =
      Map.keys fs.files
        & filter (\k -> prefix `isPrefixOf` k)
        & mapMaybe firstComponent
    firstComponent k = case splitDirectories (makeRelative dir k) of
      component : _ -> Just component
      [] -> Nothing
