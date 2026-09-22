module MediaCopy.Mhl.Store
  ( loadChain
  , readHistory
  , historyHashes
  , resolveOriginals
  , carryHistory
  , backfillChainEntry
  ) where

import Ascmhl.Build (chainEntry, chainFromListing)
import Ascmhl.Hash
import Ascmhl.Layout
import Ascmhl.Path (RelPath)
import Ascmhl.Read (parseChain, parseManifest)
import Ascmhl.Types
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Exception (displayException, trySync)
import System.OsPath (OsPath)

import MediaCopy.Domain.History
import MediaCopy.Domain.JobFormat (chainFormat)
import MediaCopy.Effects.FileSystem
import MediaCopy.Effects.Hasher

data LoadedHistory = LoadedHistory
  { chain :: Chain
  , manifests :: Vector (Int, Manifest)
  , chainFilePresent :: Bool
  }

loadHistory :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe LoadedHistory))
loadHistory folder =
  listHistory folder >>= \case
    Nothing -> pure (Right Nothing)
    Just names ->
      readChain folder names >>= \case
        (_, Left e) -> pure (Left e)
        (present, Right chain) ->
          loadManifests folder (orderedChainEntries chain)
            <&> fmap (\ms -> Just LoadedHistory {chain, manifests = ms, chainFilePresent = present})

loadManifests :: (FileSystem :> es) => OsPath -> List ChainEntry -> Eff es (Either HistoryError (Vector (Int, Manifest)))
loadManifests folder entries = go [] entries
  where
    go acc [] = pure (Right (V.fromList (reverse acc)))
    go acc (entry : rest) = do
      let path = manifestPathOf folder entry.path
      readText path >>= \case
        Nothing -> pure (Left (ManifestNotFound path))
        Just txt -> case parseManifest txt of
          Left e -> pure (Left (ManifestNotParsed path e))
          Right m -> go ((entry.sequenceNr, m) : acc) rest

readChain :: (FileSystem :> es) => OsPath -> Vector OsPath -> Eff es (Bool, Either HistoryError Chain)
readChain folder names =
  readText (chainPath folder) <&> \case
    Just txt -> (True, first (\e -> ChainNotParsed (chainPath folder) e) (parseChain txt))
    Nothing -> (False, Right (chainFromListing (mhlFileNames names)))

loadChain :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe Chain))
loadChain folder =
  listHistory folder >>= \case
    Nothing -> pure (Right Nothing)
    Just names -> readChain folder names <&> \pair -> fmap Just (snd pair)

readHistory :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe MhlHistory))
readHistory folder = do
  attempt <- trySync (loadHistory folder)
  pure $ case attempt of
    Left e -> Left (HistoryUnreadable folder (T.pack (displayException e)))
    Right result -> fmap (fmap (\loaded -> historyOf loaded.manifests)) result

historyHashes :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Chain, Map RelPath Hash)))
historyHashes folder =
  loadHistory folder <&> fmap (fmap (\loaded -> (loaded.chain, hashesOf loaded)))

hashesOf :: LoadedHistory -> Map RelPath Hash
hashesOf loaded = latestHashes (V.map (\pair -> snd pair) loaded.manifests)

resolveOriginals :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Map RelPath Hash)))
resolveOriginals source =
  loadHistory source <&> \case
    Left e -> Left e
    Right Nothing -> Right Nothing
    Right (Just loaded)
      | V.null loaded.chain.entries ->
          if loaded.chainFilePresent then Left (ChainEmpty (chainPath source)) else Right Nothing
      | otherwise -> Right (Just (hashesOf loaded))

carryHistory :: (FileSystem :> es) => OsPath -> OsPath -> Eff es (Either HistoryError (Maybe (Vector OsPath)))
carryHistory source dest =
  listHistory source >>= \case
    Nothing -> pure (Right Nothing)
    Just names -> do
      outcome <- V.foldM step (Right ()) names
      pure (fmap (\_ -> Just names) outcome)
  where
    step acc name = case acc of
      Left e -> pure (Left e)
      Right () -> carryOne name
    carryOne name = do
      let from = inHistory source name
      readText from >>= \case
        Nothing -> pure (Left (HistoryUnreadable from "the history names a file that is gone"))
        Just txt -> writeTextAtomically (inHistory dest name) txt <&> Right

backfillChainEntry :: (FileSystem :> es, Hasher :> es) => OsPath -> ChainEntry -> Eff es (Either HistoryError ChainEntry)
backfillChainEntry folder entry = case entry.c4 of
  Just _ -> pure (Right entry)
  Nothing -> do
    let path = manifestPathOf folder entry.path
    readText path >>= \case
      Nothing -> pure (Left (ManifestNotFound path))
      Just txt -> do
        h <- hashBytes chainFormat (TE.encodeUtf8 txt)
        pure (Right (chainEntry entry.sequenceNr entry.path (Just h)))
