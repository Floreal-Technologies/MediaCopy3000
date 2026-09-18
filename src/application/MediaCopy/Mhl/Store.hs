-- | This module reads an ASC MHL history off a disk with one walk over the chain. Each caller makes its own use of the result.
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

-- | A folder's history as the chain describes it. The history holds the chain and every manifest
-- it names, each with the generation number the chain gave it.
data LoadedHistory = LoadedHistory
  { chain :: Chain
  , manifests :: Vector (Int, Manifest)
  , chainFilePresent :: Bool
  -- ^ Whether the folder holds a chain file. A lost seal and no seal differ only by this.
  }

-- | This is the only walk over a history. A folder with no @ascmhl\/@ answers
-- 'Nothing', because what an absent history means is the caller's to say.
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

-- | This function parses a manifest. It never compares the manifest against the @c4@
-- that the chain holds for it.
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

-- | The chain a folder holds, and whether a chain file was there to read. 'Left' only on a
-- malformed chain file, and an I/O error throws. With manifests but no chain file, the listing's
-- manifest names synthesise one.
readChain :: (FileSystem :> es) => OsPath -> Vector OsPath -> Eff es (Bool, Either HistoryError Chain)
readChain folder names =
  readText (chainPath folder) <&> \case
    Just txt -> (True, first (\e -> ChainNotParsed (chainPath folder) e) (parseChain txt))
    -- A folder with manifests and no chain file: the listing says what the history holds.
    Nothing -> (False, Right (chainFromListing (mhlFileNames names)))

-- | The chain alone. Reads no manifest.
loadChain :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe Chain))
loadChain folder =
  listHistory folder >>= \case
    Nothing -> pure (Right Nothing)
    Just names -> readChain folder names <&> \pair -> fmap Just (snd pair)

-- | The only read that catches an I/O fault, because its caller runs it detached and
-- cannot catch one itself.
readHistory :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe MhlHistory))
readHistory folder = do
  attempt <- trySync (loadHistory folder)
  pure $ case attempt of
    Left e -> Left (HistoryUnreadable folder (T.pack (displayException e)))
    Right result -> fmap (fmap (\loaded -> historyOf loaded.manifests)) result

-- | The chain, not the directory listing, says what the history holds.
historyHashes :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Chain, Map RelPath Hash)))
historyHashes folder =
  loadHistory folder <&> fmap (fmap (\loaded -> (loaded.chain, hashesOf loaded)))

-- | The hashes a loaded history records; a later generation overrides an earlier one.
hashesOf :: LoadedHistory -> Map RelPath Hash
hashesOf loaded = latestHashes (V.map (\pair -> snd pair) loaded.manifests)

-- | A chain file that names no manifest is a failure, so a lost seal is never mistaken
-- for no seal. An @ascmhl\/@ with no chain file and no manifest is not a seal at all.
resolveOriginals :: (FileSystem :> es) => OsPath -> Eff es (Either HistoryError (Maybe (Map RelPath Hash)))
resolveOriginals source =
  loadHistory source <&> \case
    Left e -> Left e
    Right Nothing -> Right Nothing
    Right (Just loaded)
      | V.null loaded.chain.entries ->
          if loaded.chainFilePresent then Left (ChainEmpty (chainPath source)) else Right Nothing
      | otherwise -> Right (Just (hashesOf loaded))

-- | Copies the media source's @ascmhl\/@ into the destination's, so the destination continues that
-- history and numbers its next generation after it. The files go as they are, so every chain
-- entry's c4 still matches. Answers the names copied.
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

-- | Recomputes a missing c4 from the manifest the entry names. Every chain entry this project
-- writes carries one. 'HashlistType' is the sequence path, c4, so a legacy non-c4 child goes and
-- never sits beside the c4.
backfillChainEntry :: (FileSystem :> es, Hasher :> es) => OsPath -> ChainEntry -> Eff es (Either HistoryError ChainEntry)
backfillChainEntry folder entry = case entry.c4 of
  Just _ -> pure (Right entry)
  Nothing -> do
    let path = manifestPathOf folder entry.path
    readText path >>= \case
      Nothing -> pure (Left (ManifestNotFound path))
      Just txt -> do
        h <- hashBytes chainFormat (TE.encodeUtf8 txt)
        -- 'HashlistType' is the sequence path, c4. A legacy entry's other children go, because
        -- beside the c4 they make the chain invalid.
        pure (Right (chainEntry entry.sequenceNr entry.path (Just h)))
