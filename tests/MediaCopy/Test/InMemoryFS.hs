{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Test.InMemoryFS
  ( MemFS (..)
  , emptyMemFS
  , withFile
  , withTextFile
  , withFiles
  , withDir
  , withCorruptRead
  , withFailOnOpen
  , withFailOnRename
  , withFailOnManifestWrite
  , withFreeSpace
  , runFileSystemMem
  , alter
  , slashedPath
  , corruptFirstByte
  , sampleHash
  , offloadJob
  , specOf
  , sampleSpec
  , isReplaced
  , newestManifestIn
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.Layout qualified as Layout
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector qualified as V
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Exception (onException)
import System.OsPath (dropTrailingPathSeparator, makeRelative, splitDirectories)
import splice System.OsPath (osp)
import System.OsString (OsChar, isPrefixOf, isSuffixOf, unsafeFromChar)
import System.OsString qualified as OsString

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload (Offload)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.PlannedWrite (PlannedWrite)
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.Tree (Tree)
import MediaCopy.Domain.Tree qualified as Tree
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem

data MemFS = MemFS
  { files :: Map OsPath (ByteString, UTCTime)
  , corruptOnRead :: Set OsPath
  , failOnOpen :: Set OsPath
  , failOnRename :: Set OsPath
  , failOnManifestWrite :: Bool
  , dirs :: Set OsPath
  , freeSpace :: Map OsPath Int64
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
    , freeSpace = Map.empty
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

withFreeSpace :: OsPath -> Int64 -> MemFS -> MemFS
withFreeSpace path bytes fs = fs {freeSpace = Map.insert (slashedPath path) bytes fs.freeSpace}

slashedPath :: OsPath -> OsPath
slashedPath = OsString.map (\c -> if c == backslash then slash else c)
  where
    backslash = unsafeFromChar '\\' :: OsChar
    slash = unsafeFromChar '/'

slashedWrite :: PlannedWrite -> PlannedWrite
slashedWrite w = PlannedWrite.PlannedWrite {temp = slashedPath w.temp, final = slashedPath w.final, mode = w.mode}

withFiles :: Map OsPath (ByteString, UTCTime) -> MemFS -> MemFS
withFiles m fs = fs {files = m}

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
    FileSystem.Walk root -> liftIO (walkMem fsRef (slashedPath root))
    FileSystem.FreeSpace (slashedPath -> p) -> liftIO (readIORef fsRef <&> \fs -> Just (Map.findWithDefault maxBound p fs.freeSpace))
    FileSystem.StreamFile _ (slashedPath -> path) onChunk -> localSeqUnlift env $ \unlift -> do
      content <- liftIO (readWholeMem fsRef path)
      forM_ (chunksOf content) (\chunk -> unlift (onChunk chunk))
      liftIO (mtimeMem fsRef path)
    FileSystem.WriteTemps (slashedPath -> source) (V.map slashedWrite -> writes) onFlush onChunk -> localSeqUnlift env $ \unlift -> do
      liftIO (forM_ writes (\w -> failIfInjected fsRef w.temp))
      content <- liftIO (readWholeMem fsRef source)
      liftIO (forM_ writes (\w -> alter fsRef (withFile w.temp content)))
      let feedAndFlush = do
            forM_ (chunksOf content) (\chunk -> unlift (onChunk chunk))
            unlift onFlush
            liftIO (mtimeMem fsRef source)
      feedAndFlush `onException` liftIO (forM_ writes (\w -> alter fsRef (\fs -> withFiles (Map.delete w.temp fs.files) fs)))
    FileSystem.Publish (V.map slashedWrite -> writes) mtime -> liftIO $ forM_ writes $ \w -> do
      failIfRenameInjected fsRef w.final
      renameMem fsRef w.temp w.final mtime
    FileSystem.RemoveTemps (V.map slashedWrite -> writes) -> liftIO $ forM_ writes $ \w ->
      alter fsRef (\fs -> withFiles (Map.delete w.temp fs.files) fs)
    FileSystem.Mtime (slashedPath -> p) -> liftIO (mtimeMem fsRef p)
    FileSystem.ReadText (slashedPath -> p) -> liftIO $ do
      fs <- readIORef fsRef
      traverse (\entry -> decodeTextMem p (fst entry)) (Map.lookup p fs.files)
    FileSystem.ListHistory (slashedPath -> folder) -> liftIO (listHistoryMem folder <$> readIORef fsRef)
    FileSystem.WriteTextAtomically p t -> liftIO $ do
      fs <- readIORef fsRef
      when fs.failOnManifestWrite (ioError (userError "injected manifest write"))
      alter fsRef (withTextFile p t)
    FileSystem.MakeDirectories dirs -> liftIO (V.mapM_ (\dir -> alter fsRef (withDir dir)) dirs)
    FileSystem.ListDirectory (slashedPath -> dir) -> liftIO (readIORef fsRef <&> listDirectoryMem dir)
    FileSystem.DoesFileExist (slashedPath -> p) -> liftIO (readIORef fsRef <&> \fs -> Map.member p fs.files)
    FileSystem.DoesDirectoryExist (slashedPath -> p) -> liftIO (readIORef fsRef <&> dirExists p)
    FileSystem.ReadBytes (slashedPath -> p) -> liftIO (readWholeMem fsRef p)
    FileSystem.AppendBytes (slashedPath -> p) bytes -> liftIO (failIfInjected fsRef p >> alter fsRef (appendMem p bytes))
    FileSystem.WriteBytes p bytes -> liftIO (alter fsRef (withFile p bytes))

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
  Left err -> ioError (userError (T.unpack (OsPath.toText p) <> ": " <> show err))
  Right t -> pure t

readWholeMem :: IORef MemFS -> OsPath -> IO ByteString
readWholeMem fsRef p = do
  failIfInjected fsRef p
  fs <- readIORef fsRef
  case Map.lookup p fs.files of
    Nothing -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (OsPath.toText p)))
    Just (content, _) ->
      pure (if p `Set.member` fs.corruptOnRead then corruptFirstByte content else content)

corruptFirstByte :: ByteString -> ByteString
corruptFirstByte bs = case BS.uncons bs of
  Nothing -> bs
  Just (b, rest) -> BS.cons (xor b 0xFF) rest

renameMem :: IORef MemFS -> OsPath -> OsPath -> UTCTime -> IO ()
renameMem fsRef src dst mtime =
  atomicModifyIORef' fsRef renamed >>= \case
    False -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (OsPath.toText src)))
    True -> pure ()
  where
    renamed fs = case Map.lookup src fs.files of
      Nothing -> (fs, False)
      Just (content, _) -> (withFiles (fs.files & Map.delete src & Map.insert dst (content, mtime)) fs, True)

alter :: IORef MemFS -> (MemFS -> MemFS) -> IO ()
alter fsRef change = atomicModifyIORef' fsRef (\fs -> (change fs, ()))

mtimeMem :: IORef MemFS -> OsPath -> IO UTCTime
mtimeMem fsRef p = do
  fs <- readIORef fsRef
  case Map.lookup p fs.files of
    Just entry -> pure (snd entry)
    Nothing -> case belowMTimes fs of
      [] -> ioError (userError (T.unpack owner <> ": no such file " <> T.unpack (OsPath.toText p)))
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

walkMem :: IORef MemFS -> OsPath -> IO (Maybe Tree)
walkMem fsRef root = do
  fs <- readIORef fsRef
  if not (dirExists root fs)
    then pure Nothing
    else do
      files <- traverse toEntry (filter underRoot (Map.toList fs.files))
      let sorted = V.fromList (sortOn fst files)
      pure (Just Tree.Tree {files = sorted, dirs = dirsBelow root fs})
  where
    prefix = withTrailingSlash root
    underRoot (p, _) =
      prefix `isPrefixOf` p && not (any Tree.isIgnoredName (splitDirectories (makeRelative root p)))
    toEntry (p, (bs, _)) = do
      rel <- either (\message -> ioError (userError (T.unpack message))) pure (Tree.relPathOf root p)
      pure (rel, fromIntegral (BS.length bs) :: FileSize)

-- | Every directory below the root, as the disk walk lists it: the folders that hold files, the
-- empty ones made with 'withDir' or 'FileSystem.MakeDirectories', and their parents. A walk does not go into
-- a folder with an ignored name, and does not list it.
dirsBelow :: OsPath -> MemFS -> Vector RelPath
dirsBelow root fs =
  (fromFiles <> fromDirs)
    & concatMap (\components -> components & takeWhile (\component -> not (V.elem component Tree.ignorePatterns)) & scanl1 joinPath)
    & Set.fromList
    & Set.toList
    & mapMaybe RelPath.parse
    & sort
    & V.fromList
  where
    prefix = withTrailingSlash root
    below = filter (isPrefixOf prefix)
    componentsOf p = either (const []) (T.splitOn "/" . display) (Tree.relPathOf root p)
    fromFiles = Map.keys fs.files & below & fmap (\p -> componentsOf p & reverse & drop 1 & reverse)
    fromDirs = Set.toList fs.dirs & below & fmap componentsOf
    joinPath acc component = acc <> "/" <> component

appendMem :: OsPath -> ByteString -> MemFS -> MemFS
appendMem p bytes fs = withFiles (Map.insertWith (\new old -> (fst old <> fst new, snd old)) p (bytes, epoch) fs.files) fs

entriesBelow :: OsPath -> MemFS -> List OsPath
entriesBelow dir fs = filter (isPrefixOf (withTrailingSlash dir)) (Map.keys fs.files <> Set.toList fs.dirs)

listDirectoryMem :: OsPath -> MemFS -> Maybe (List OsPath)
listDirectoryMem dir fs
  | not (dirExists dir fs) = Nothing
  | otherwise = Just (Set.toList (Set.fromList (mapMaybe firstComponent (entriesBelow dir fs))))
  where
    firstComponent k = case splitDirectories (makeRelative dir k) of
      component : _ -> Just component
      [] -> Nothing

listHistoryMem :: OsPath -> MemFS -> Maybe (Vector OsPath)
listHistoryMem folder fs
  | not (dirExists dir fs) = Nothing
  | otherwise = Just (V.fromList (Set.toList (Set.fromList names)))
  where
    dir = slashedPath (Layout.dir folder)
    prefix = withTrailingSlash dir
    names =
      Map.keys fs.files
        & filter (\k -> prefix `isPrefixOf` k)
        & mapMaybe firstComponent
    firstComponent k = case splitDirectories (makeRelative dir k) of
      component : _ -> Just component
      [] -> Nothing

sampleHash :: Hash
sampleHash = Hash.Hash Algo.XXH64 "ef46db3751d8e999"

offloadJob :: OsPath -> List OsPath -> Offload.SealFirst -> Offload
offloadJob source dests sealFirst =
  Offload.Offload {source, destinations = NE.fromList dests, sealFirst, existingCopy = Nothing}

specOf :: Job -> JobSpec
specOf job = JobSpec.JobSpec {id = JobId.JobId 1, job, createdAt = epoch, pluginFields = mempty}

sampleSpec :: JobSpec
sampleSpec = specOf (Job.Offload (offloadJob [osp|/src|] [[osp|/dst|]] Offload.UseHistory))

isReplaced :: FileStatus -> Bool
isReplaced (FileStatus.Done (FileOutcome.Replaced _)) = True
isReplaced _ = False

isManifestIn :: OsPath -> OsPath -> Bool
isManifestIn root p = withTrailingSlash (slashedPath (Layout.dir root)) `isPrefixOf` p && [osp|.mhl|] `isSuffixOf` p

newestManifestIn :: OsPath -> MemFS -> Maybe Manifest
newestManifestIn root fs = do
  (_, (bytes, _)) <- Map.lookupMax (Map.filterWithKey (\k _ -> isManifestIn root k) fs.files)
  either (const Nothing) Just (Manifest.parse (TE.decodeUtf8 bytes))
