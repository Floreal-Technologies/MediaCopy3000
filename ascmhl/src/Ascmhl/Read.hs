module Ascmhl.Read
  ( parseManifest
  , parseChain
  , parseMhlTime
  ) where

import Control.Exception (SomeException, displayException)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (List, find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Lazy qualified as TL
import Data.Text.Read qualified as TR
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM, zonedTimeToUTC)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML
import Text.XML.Cursor

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..), mkRelPath)
import Ascmhl.Schema qualified as Schema
import Ascmhl.Types

parseMhlTime :: Text -> Maybe UTCTime
parseMhlTime t =
  zonedTimeToUTC <$> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" (T.unpack (fixZ t))
  where
    fixZ s = if "Z" `T.isSuffixOf` s then T.dropEnd 1 s <> "+00:00" else s

parseDoc :: Text -> Either Text Cursor
parseDoc t =
  first
    (\e -> displayException @SomeException e & T.pack & (\msg -> "ASC MHL: " <> msg))
    (fromDocument <$> parseText def (TL.fromStrict t))

childText :: Text -> Cursor -> Maybe Text
childText local c = listToMaybe (c $/ laxElement local &/ content)

attr :: Text -> Cursor -> Maybe Text
attr a c = listToMaybe (attribute (Name a Nothing Nothing) c)

localName :: Cursor -> Maybe Text
localName c = case node c of
  NodeElement e -> Just (nameLocalName e.elementName)
  _ -> Nothing

angled :: Text -> Text
angled name = "<" <> name <> ">"

require :: Text -> Maybe a -> Either Text a
require what value = case value of
  Nothing -> Left ("ASC MHL: missing " <> what)
  Just found -> Right found

readIntegral :: (Integral a) => Text -> Maybe a
readIntegral t = either (const Nothing) (\(n, rest) -> if T.null rest then Just n else Nothing) (TR.decimal t)

requireRoot :: Text -> Text -> Cursor -> Either Text ()
requireRoot ns name c = case node c of
  NodeElement e
    | nameLocalName e.elementName /= name -> Left notTheRoot
    | otherwise -> case nameNamespace e.elementName of
        Nothing -> Right ()
        Just found | found == ns -> Right ()
        Just found -> Left ("ASC MHL: <" <> name <> "> is in namespace " <> found <> ", not " <> ns)
  _ -> Left notTheRoot
  where
    notTheRoot = "ASC MHL: root element is not <" <> name <> ">"

hashElement :: Cursor -> Maybe Hash
hashElement hc = do
  local <- localName hc
  algo <- algoFromMhlElement local
  let raw = T.strip (T.concat (hc $/ content))
  pure (Hash algo ((algoSpec algo).readValue raw))

attributesExcept :: List Text -> Cursor -> Map Name Text
attributesExcept known hc = case node hc of
  NodeElement e -> Map.filterWithKey (\name _ -> nameLocalName name `notElem` known) e.elementAttributes
  _ -> Map.empty

childrenExcept :: (Text -> Bool) -> Cursor -> Vector Node
childrenExcept known c =
  kids
    & filter (\child -> maybe True (\local -> not (known local)) (localName child))
    & map (\child -> node child)
    & V.fromList
  where
    kids = c $/ anyElement

parseManifest :: Text -> Either Text Manifest
parseManifest txt = do
  root <- parseDoc txt
  requireRoot Schema.manifestNs Schema.hashlist root
  case attr Schema.version root of
    Just v | not (Schema.isSupportedVersion v) -> Left ("ASC MHL: unsupported manifest version " <> v)
    _ -> Right ()
  ci <- require (angled Schema.creatorinfo) (listToMaybe (root $/ laxElement Schema.creatorinfo))
  creationDate <- require (angled Schema.creationdate) (childText Schema.creationdate ci >>= parseMhlTime)
  let hostname = fromMaybe "" (childText Schema.hostname ci)
      toolC = listToMaybe (ci $/ laxElement Schema.tool)
      toolName = maybe "" (\tc -> T.concat (tc $/ content)) toolC
      toolVersion = toolC >>= attr Schema.version
      process =
        fromMaybe
          ProcessInPlace
          (listToMaybe (root $/ laxElement Schema.processinfo) >>= childText Schema.process >>= processFromName)
      patternList = root $/ laxElement Schema.processinfo &/ laxElement Schema.ignore &/ laxElement Schema.ignorePattern &/ content
      ignorePatterns = V.fromList patternList
      creatorUnknown =
        childrenExcept (\local -> local `elem` [Schema.creationdate, Schema.hostname, Schema.tool]) ci
  rootHash <- case listToMaybe (root $/ laxElement Schema.processinfo &/ laxElement Schema.roothash) of
    Nothing -> Right V.empty
    Just pc -> dirHashesOf (angled Schema.roothash) pc
  entriesList <- traverse parseEntry (root $/ laxElement Schema.hashes &/ anyElement)
  let entries = V.fromList (catMaybes entriesList)
  pure
    Manifest
      { creator = CreatorInfo {creationDate, hostname, toolName, toolVersion, unknown = creatorUnknown}
      , process
      , rootHash
      , ignorePatterns
      , entries
      , unknown = childrenExcept (\local -> local `elem` [Schema.creatorinfo, Schema.processinfo, Schema.hashes]) root
      }

parseEntry :: Cursor -> Either Text (Maybe ManifestEntry)
parseEntry c = case localName c of
  Just local
    | local == Schema.hash -> fmap (\e -> Just (ManifestFile e)) (parseFileEntry c)
    | local == Schema.directoryhash -> fmap (\d -> Just (ManifestDir d)) (parseDirEntry c)
  _ -> Right Nothing

parseFileEntry :: Cursor -> Either Text HashEntry
parseFileEntry c = do
  pathC <- require (angled Schema.path) (listToMaybe (c $/ laxElement Schema.path))
  let pathValue = T.concat (pathC $/ content)
  path <- maybe (Left ("ASC MHL: path is not relative: " <> pathValue)) Right (mkRelPath pathValue)
  let size = fromMaybe 0 (attr Schema.size pathC >>= readIntegral)
  lastModified <- require Schema.lastmodificationdate (attr Schema.lastmodificationdate pathC >>= parseMhlTime)
  let hashes = V.fromList (mapMaybe (\hc -> manifestHashOf hc) (c $/ anyElement))
      known local = local == Schema.path || isJust (algoFromMhlElement local)
  pure
    HashEntry
      { path
      , size
      , lastModified
      , hashes
      , pathAttrs = attributesExcept [Schema.size, Schema.lastmodificationdate] pathC
      , unknown = childrenExcept known c
      }

manifestHashOf :: Cursor -> Maybe ManifestHash
manifestHashOf hc = do
  h <- hashElement hc
  let action = attr Schema.action hc >>= actionFromName
      hashDate = attr Schema.hashdate hc >>= parseMhlTime
  Just
    ManifestHash
      { hash = h
      , action = fromMaybe Original action
      , hashDate
      , extraAttrs = attributesExcept [Schema.action, Schema.hashdate] hc
      }

parseDirEntry :: Cursor -> Either Text DirectoryEntry
parseDirEntry c = do
  pathC <- require (angled Schema.path) (listToMaybe (c $/ laxElement Schema.path))
  let pathValue = T.concat (pathC $/ content)
  path <- maybe (Left ("ASC MHL: path is not relative: " <> pathValue)) Right (mkRelPath pathValue)
  lastModified <- require Schema.lastmodificationdate (attr Schema.lastmodificationdate pathC >>= parseMhlTime)
  hashes <- dirHashesOf (display path) c
  pure
    DirectoryEntry
      { path
      , lastModified
      , hashes
      , pathAttrs = attributesExcept [Schema.lastmodificationdate] pathC
      , unknown = childrenExcept (\local -> local `elem` [Schema.path, Schema.content, Schema.structure]) c
      }

dirHashesOf :: Text -> Cursor -> Either Text (Vector DirHash)
dirHashesOf label c = do
  paired <- traverse (\pair -> withStructure pair) contents
  traverse_ (\s -> requireContent s) structures
  Right (V.fromList paired)
  where
    contents = mapMaybe (\hc -> fmap (\h -> (h, hc)) (hashElement hc)) (c $/ laxElement Schema.content &/ anyElement)
    structures = mapMaybe (\hc -> hashElement hc) (c $/ laxElement Schema.structure &/ anyElement)
    unpaired what algo = "ASC MHL: " <> label <> ": " <> display algo <> " " <> what
    withStructure (h, hc) = case find (\candidate -> candidate.algo == h.algo) structures of
      Nothing -> Left (unpaired "content has no matching structure" h.algo)
      Just s ->
        Right
          DirHash
            { content = h
            , structure = s
            , hashDate = attr Schema.hashdate hc >>= parseMhlTime
            , extraAttrs = attributesExcept [Schema.hashdate] hc
            }
    requireContent s = case find (\candidate -> (fst candidate).algo == s.algo) contents of
      Nothing -> Left (unpaired "structure has no matching content" s.algo)
      Just _ -> Right ()

parseChain :: Text -> Either Text Chain
parseChain txt = do
  root <- parseDoc txt
  requireRoot Schema.chainNs Schema.ascmhldirectory root
  entriesList <- traverse entry (root $/ laxElement Schema.hashlist)
  pure (Chain (V.fromList entriesList))
  where
    entry c = do
      sequenceNr <- require Schema.sequencenr (attr Schema.sequencenr c >>= readIntegral)
      pathC <- require (angled Schema.path) (listToMaybe (c $/ laxElement Schema.path))
      let pathValue = T.concat (pathC $/ content)
      path <- maybe (Left ("ASC MHL: chain path is not relative: " <> pathValue)) Right (mkRelPath pathValue)
      let kids = c $/ anyElement
          c4 = kids & mapMaybe (\hc -> hashElement hc) & find (\h -> h.algo == C4)
          isC4 local = algoFromMhlElement local == Just C4
      pure ChainEntry {sequenceNr, path, c4, unknown = childrenExcept (\local -> local == Schema.path || isC4 local) c}
