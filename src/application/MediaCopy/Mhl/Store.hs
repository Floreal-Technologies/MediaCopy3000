module MediaCopy.Mhl.Store
  ( loadChain
  , readHistory
  , historyHashes
  , resolveOriginals
  , carryHistory
  , backfillChainEntry
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Chain (Chain)
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry (ChainEntry)
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.Hash (Hash)
import Ascmhl.History (History)
import Ascmhl.History qualified as History
import Ascmhl.Layout qualified as Layout
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.RelPath (RelPath)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful.Exception (displayException, trySync)

import MediaCopy.Domain.HistoryError (HistoryError)
import MediaCopy.Domain.HistoryError qualified as HistoryError
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Effects.Hasher (Hasher)
import MediaCopy.Effects.Hasher qualified as Hasher

data LoadedHistory = LoadedHistory
  { chain :: Chain
  , manifests :: Vector (Int, Manifest)
  , chainFilePresent :: Bool
  }

loadHistory :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe LoadedHistory))
loadHistory folder =
  FileSystem.listHistory folder >>= \case
    Nothing -> pure (Right Nothing)
    Just names ->
      readChain folder names >>= \case
        (_, Left e) -> pure (Left e)
        (present, Right chain) ->
          loadManifests folder (Chain.inOrder chain)
            <&> fmap (\ms -> Just LoadedHistory {chain, manifests = ms, chainFilePresent = present})

loadManifests :: (FileSystem :> es) => OsPath -> List ChainEntry -> Eff es (Either HistoryError (Vector (Int, Manifest)))
loadManifests folder entries = go [] entries
  where
    go acc [] = pure (Right (V.fromList (reverse acc)))
    go acc (entry : rest) = do
      let path = Layout.manifestFile folder entry.path
      FileSystem.readText path >>= \case
        Nothing -> pure (Left (HistoryError.ManifestNotFound path))
        Just txt -> case Manifest.parse txt of
          Left e -> pure (Left (HistoryError.ManifestNotParsed path e))
          Right m -> go ((entry.sequenceNr, m) : acc) rest

readChain :: (FileSystem :> es) => OsPath -> Vector OsPath -> Eff es (Bool, Either HistoryError Chain)
readChain folder names =
  FileSystem.readText (Layout.chainFile folder) <&> \case
    Just txt -> (True, first (HistoryError.ChainNotParsed (Layout.chainFile folder)) (Chain.parse txt))
    Nothing -> (False, Right (Chain.fromListing (Layout.manifestNames names)))

loadChain :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe Chain))
loadChain folder =
  FileSystem.listHistory folder >>= \case
    Nothing -> pure (Right Nothing)
    Just names -> readChain folder names <&> \pair -> fmap Just (snd pair)

readHistory :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe History))
readHistory folder = do
  attempt <- trySync (loadHistory folder)
  pure $ case attempt of
    Left e -> Left (HistoryError.HistoryUnreadable folder (T.pack (displayException e)))
    Right result -> fmap (fmap (\loaded -> History.fromManifests loaded.manifests)) result

historyHashes :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Chain, Map RelPath Hash)))
historyHashes folder =
  loadHistory folder <&> fmap (fmap (\loaded -> (loaded.chain, hashesOf loaded)))

hashesOf :: LoadedHistory -> Map RelPath Hash
hashesOf loaded = Manifest.latestHashes (V.map snd loaded.manifests)

resolveOriginals :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Map RelPath Hash)))
resolveOriginals source =
  loadHistory source <&> \case
    Left e -> Left e
    Right Nothing -> Right Nothing
    Right (Just loaded)
      | V.null loaded.chain.entries ->
          if loaded.chainFilePresent then Left (HistoryError.ChainEmpty (Layout.chainFile source)) else Right Nothing
      | otherwise -> Right (Just (hashesOf loaded))

carryHistory :: (FileSystem :> es) => OsPath -> OsPath -> Eff es (Either HistoryError (Maybe (Vector OsPath)))
carryHistory source dest =
  FileSystem.listHistory source >>= \case
    Nothing -> pure (Right Nothing)
    Just names -> do
      outcome <- V.foldM step (Right ()) names
      pure (fmap (\_ -> Just names) outcome)
  where
    step acc name = case acc of
      Left e -> pure (Left e)
      Right () -> carryOne name
    carryOne name = do
      let from = Layout.inHistory source name
      FileSystem.readText from >>= \case
        Nothing -> pure (Left (HistoryError.HistoryUnreadable from "the history names a file that is gone"))
        Just txt -> FileSystem.writeTextAtomically (Layout.inHistory dest name) txt <&> Right

backfillChainEntry :: (FileSystem :> es, Hasher :> es) => OsPath -> ChainEntry -> Eff es (Either HistoryError ChainEntry)
backfillChainEntry folder entry = case entry.c4 of
  Just _ -> pure (Right entry)
  Nothing -> do
    let path = Layout.manifestFile folder entry.path
    FileSystem.readText path >>= \case
      Nothing -> pure (Left (HistoryError.ManifestNotFound path))
      Just txt -> do
        h <- Hasher.hashBytes Algo.C4 (TE.encodeUtf8 txt)
        pure (Right (ChainEntry.new entry.sequenceNr entry.path (Just h)))
