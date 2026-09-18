module Ascmhl.Manifest
  ( Manifest (..)
  , new
  , withMetadata
  , latestHashes
  , parse
  , render
  , fileName
  ) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List, find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Element (..), Name (..), Node (..))
import Text.XML.Cursor (Cursor, anyElement, content, laxElement, node, ($/), (&/))

import Ascmhl.Algo qualified as Algo
import Ascmhl.Author (Author)
import Ascmhl.Author qualified as Author
import Ascmhl.Creator (Creator)
import Ascmhl.Creator qualified as Creator
import Ascmhl.DirHash (DirHash)
import Ascmhl.DirHash qualified as DirHash
import Ascmhl.DirectoryEntry (DirectoryEntry)
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.FileEntry (FileEntry)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Fragment (Fragment)
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.ManifestEntry (ManifestEntry)
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.Process (Process)
import Ascmhl.Process qualified as Process
import Ascmhl.RelPath (RelPath)
import Ascmhl.Schema qualified as Schema
import Ascmhl.Time qualified as Time
import Ascmhl.Xml qualified as Xml

data Manifest = Manifest
  { creator :: Creator
  , process :: Process
  , rootHash :: Vector DirHash
  , ignorePatterns :: Vector Text
  , entries :: Vector ManifestEntry
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

new :: Creator -> Process -> DirHash -> Vector Text -> Vector ManifestEntry -> Manifest
new creator process rootPair ignorePatterns entries =
  Manifest
    { creator
    , process
    , rootHash = V.singleton rootPair
    , ignorePatterns
    , entries
    , unknown = V.empty
    }

withMetadata :: Fragment -> Manifest -> Manifest
withMetadata fragment manifest =
  Manifest
    { creator = manifest.creator
    , process = manifest.process
    , rootHash = manifest.rootHash
    , ignorePatterns = manifest.ignorePatterns
    , entries = manifest.entries
    , unknown = V.snoc manifest.unknown (Xml.metadataNode fragment)
    }

-- | The hash to check each file against.
latestHashes :: Vector Manifest -> Map RelPath Hash
latestHashes manifests = fmap fst (foldl step Map.empty manifests)
  where
    step acc m = foldl (\a e -> maybe a (\picked -> Map.insertWith keep e.path picked a) (pick e.hashes)) acc (ManifestEntry.files m.entries)
    keep newer older = if snd newer then older else newer
    pick hs = fmap (\mh -> (mh.hash, mh.action == FileEntry.Failed)) $ case V.find (\mh -> mh.hash.algo == Algo.preferred) hs of
      Just mh -> Just mh
      Nothing -> hs V.!? 0

-- |
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> fileName 1 "A002R2EC" (UTCTime (fromGregorian 2026 5 9) 61320)
-- "0001_A002R2EC_2026-05-09_170200.mhl"
fileName :: Int -> Text -> UTCTime -> Text
fileName n folder t =
  T.justifyRight 4 '0' (T.show n)
    <> "_"
    <> folder
    <> "_"
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" t)
    <> ".mhl"

childText :: Text -> Cursor -> Maybe Text
childText local c = listToMaybe (c $/ laxElement local &/ content)

attributesExcept :: List Text -> Cursor -> Map Name Text
attributesExcept known hc = case node hc of
  NodeElement e -> Map.filterWithKey (\name _ -> nameLocalName name `notElem` known) e.elementAttributes
  _ -> Map.empty

-- |
-- >>> let creator = "<creatorinfo><creationdate>2026-01-01T00:00:00Z</creationdate><hostname>h</hostname><tool>t</tool><author email=\"jane@example.com\" role=\"DIT\">Jane Doe</author><author>Sam Roe</author><comment>c</comment></creatorinfo>"
-- >>> fmap (\m -> (m.creator.authors, length m.creator.unknown)) (parse ("<hashlist xmlns=\"urn:ASC:MHL:v2.0\" version=\"2.0\">" <> creator <> "</hashlist>"))
-- Right ([Author {name = "Jane Doe", email = Just "jane@example.com", phone = Nothing, role = Just "DIT"},Author {name = "Sam Roe", email = Nothing, phone = Nothing, role = Nothing}],1)
parse :: Text -> Either Text Manifest
parse document = do
  root <- Xml.parseDoc document
  Xml.requireRoot Schema.manifestNs Schema.hashlist root
  case Xml.attr Schema.version root of
    Just v | not (Schema.isSupportedVersion v) -> Left ("ASC MHL: unsupported manifest version " <> v)
    _ -> Right ()
  ci <- Xml.require (Xml.angled Schema.creatorinfo) (listToMaybe (root $/ laxElement Schema.creatorinfo))
  creationDate <- Xml.require (Xml.angled Schema.creationdate) (childText Schema.creationdate ci >>= Time.parse)
  let hostname = fromMaybe "" (childText Schema.hostname ci)
      toolC = listToMaybe (ci $/ laxElement Schema.tool)
      toolName = maybe "" (\tc -> T.concat (tc $/ content)) toolC
      toolVersion = toolC >>= Xml.attr Schema.version
      process =
        fromMaybe
          Process.InPlace
          (listToMaybe (root $/ laxElement Schema.processinfo) >>= childText Schema.process >>= processFromName)
      patternList = root $/ laxElement Schema.processinfo &/ laxElement Schema.ignore &/ laxElement Schema.ignorePattern &/ content
      ignorePatterns = V.fromList patternList
      authors = V.fromList (map authorOf (ci $/ laxElement Schema.author))
      creatorUnknown =
        Xml.childrenExcept (\local -> local `elem` [Schema.creationdate, Schema.hostname, Schema.tool, Schema.author]) ci
  rootHash <- case listToMaybe (root $/ laxElement Schema.processinfo &/ laxElement Schema.roothash) of
    Nothing -> Right V.empty
    Just pc -> dirHashesOf (Xml.angled Schema.roothash) pc
  entriesList <- traverse parseEntry (root $/ laxElement Schema.hashes &/ anyElement)
  let entries = V.fromList (catMaybes entriesList)
  pure
    Manifest
      { creator = Creator.Creator {creationDate, hostname, toolName, toolVersion, authors, unknown = creatorUnknown}
      , process
      , rootHash
      , ignorePatterns
      , entries
      , unknown = Xml.childrenExcept (\local -> local `elem` [Schema.creatorinfo, Schema.processinfo, Schema.hashes]) root
      }

processFromName :: Text -> Maybe Process
processFromName = \case
  "transfer" -> Just Process.Transfer
  ("in-place"; "verify") -> Just Process.InPlace
  "flatten" -> Just Process.Flatten
  _ -> Nothing

authorOf :: Cursor -> Author
authorOf c =
  Author.Author
    { name = T.concat (c $/ content)
    , email = Xml.attr Schema.email c
    , phone = Xml.attr Schema.phone c
    , role = Xml.attr Schema.role c
    }

parseEntry :: Cursor -> Either Text (Maybe ManifestEntry)
parseEntry c = case Xml.localName c of
  Just local
    | local == Schema.hash -> fmap (Just . ManifestEntry.File) (parseFileEntry c)
    | local == Schema.directoryhash -> fmap (Just . ManifestEntry.Directory) (parseDirEntry c)
  _ -> Right Nothing

parseFileEntry :: Cursor -> Either Text FileEntry
parseFileEntry c = do
  (pathC, path) <- Xml.pathElement "path" c
  let size = fromMaybe 0 (Xml.attr Schema.size pathC >>= Xml.readIntegral)
  lastModified <- Xml.require Schema.lastmodificationdate (Xml.attr Schema.lastmodificationdate pathC >>= Time.parse)
  let hashes = V.fromList (mapMaybe manifestHashOf (c $/ anyElement))
      known local = local == Schema.path || isJust (Algo.fromMhlElement local)
  pure
    FileEntry.FileEntry
      { path
      , size
      , lastModified
      , hashes
      , pathAttrs = attributesExcept [Schema.size, Schema.lastmodificationdate] pathC
      , unknown = Xml.childrenExcept known c
      }

manifestHashOf :: Cursor -> Maybe FileEntry.HashRecord
manifestHashOf hc = do
  h <- Xml.hashElement hc
  let action = Xml.attr Schema.action hc >>= actionFromName
      hashDate = Xml.attr Schema.hashdate hc >>= Time.parse
  Just
    FileEntry.HashRecord
      { hash = h
      , action = fromMaybe FileEntry.Original action
      , hashDate
      , extraAttrs = attributesExcept [Schema.action, Schema.hashdate] hc
      }

actionFromName :: Text -> Maybe FileEntry.Action
actionFromName = \case
  "original" -> Just FileEntry.Original
  ("verified"; "new") -> Just FileEntry.Verified
  "failed" -> Just FileEntry.Failed
  _ -> Nothing

parseDirEntry :: Cursor -> Either Text DirectoryEntry
parseDirEntry c = do
  (pathC, path) <- Xml.pathElement "path" c
  lastModified <- Xml.require Schema.lastmodificationdate (Xml.attr Schema.lastmodificationdate pathC >>= Time.parse)
  hashes <- dirHashesOf (display path) c
  pure
    DirectoryEntry.DirectoryEntry
      { path
      , lastModified
      , hashes
      , pathAttrs = attributesExcept [Schema.lastmodificationdate] pathC
      , unknown = Xml.childrenExcept (\local -> local `elem` [Schema.path, Schema.content, Schema.structure]) c
      }

dirHashesOf :: Text -> Cursor -> Either Text (Vector DirHash)
dirHashesOf label c = do
  paired <- traverse withStructure contents
  traverse_ requireContent structures
  Right (V.fromList paired)
  where
    contents = mapMaybe (\hc -> fmap (\h -> (h, hc)) (Xml.hashElement hc)) (c $/ laxElement Schema.content &/ anyElement)
    structures = mapMaybe Xml.hashElement (c $/ laxElement Schema.structure &/ anyElement)
    unpaired what algo = "ASC MHL: " <> label <> ": " <> display algo <> " " <> what
    withStructure (h, hc) = case find (\candidate -> candidate.algo == h.algo) structures of
      Nothing -> Left (unpaired "content has no matching structure" h.algo)
      Just s ->
        Right
          DirHash.DirHash
            { content = h
            , structure = s
            , hashDate = Xml.attr Schema.hashdate hc >>= Time.parse
            , extraAttrs = attributesExcept [Schema.hashdate] hc
            }
    requireContent s = case find (\candidate -> (fst candidate).algo == s.algo) contents of
      Nothing -> Left (unpaired "structure has no matching content" s.algo)
      Just _ -> Right ()

render :: Manifest -> Text
render m = Xml.renderDoc root
  where
    root =
      Element
        (Xml.nsName Schema.manifestNs Schema.hashlist)
        (Map.fromList [(Name Schema.version Nothing Nothing, Schema.supportedVersion)])
        ( [ creatorInfoEl m.creator
          , manifestEl Schema.processinfo [] (processInfoChildren m)
          ]
            <> hashesEl m.entries
            <> Xml.kept m.unknown
        )

nonEmptyVector :: Vector a -> Maybe (Vector a)
nonEmptyVector v = if V.null v then Nothing else Just v

keptLocalName :: Node -> Text
keptLocalName = \case
  NodeElement e -> nameLocalName e.elementName
  _ -> ""

inSchemaOrder :: List (Text, Node) -> List Node
inSchemaOrder named = named & sortOn (position . fst) & map snd
  where
    position name = fromMaybe (V.length Schema.hashFormatOrder) (V.elemIndex name Schema.hashFormatOrder)

manifestEl :: Text -> List (Text, Text) -> List Node -> Node
manifestEl = Xml.el Schema.manifestNs

manifestElWith :: Text -> Map Name Text -> List Node -> Node
manifestElWith = Xml.elWith Schema.manifestNs

creatorInfoEl :: Creator -> Node
creatorInfoEl creator =
  manifestEl
    Schema.creatorinfo
    []
    ( [ manifestEl Schema.creationdate [] [Xml.txt (Time.render creator.creationDate)]
      , manifestEl Schema.hostname [] [Xml.txt creator.hostname]
      , manifestEl Schema.tool (optAttr Schema.version creator.toolVersion) [Xml.txt creator.toolName]
      ]
        <> map authorEl (V.toList creator.authors)
        <> Xml.kept creator.unknown
    )

authorEl :: Author -> Node
authorEl author =
  manifestEl
    Schema.author
    (optAttr Schema.email author.email <> optAttr Schema.phone author.phone <> optAttr Schema.role author.role)
    [Xml.txt author.name]

optAttr :: Text -> Maybe Text -> List (Text, Text)
optAttr key = maybe [] (\v -> [(key, v)])

processInfoChildren :: Manifest -> List Node
processInfoChildren m =
  [manifestEl Schema.process [] [Xml.txt (display m.process)]]
    <> rootHashEl m.rootHash
    <> ignoreEl m.ignorePatterns

rootHashEl :: Vector DirHash -> List Node
rootHashEl hashes =
  maybe [] (\present -> [manifestEl Schema.roothash [] (dirHashChildren present)]) (nonEmptyVector hashes)

ignoreEl :: Vector Text -> List Node
ignoreEl patterns =
  maybe
    []
    (\present -> [manifestEl Schema.ignore [] (V.toList (V.map ignorePatternEl present))])
    (nonEmptyVector patterns)

ignorePatternEl :: Text -> Node
ignorePatternEl p = manifestEl Schema.ignorePattern [] [Xml.txt p]

hashesEl :: Vector ManifestEntry -> List Node
hashesEl entries =
  maybe
    []
    (\rendered -> [manifestEl Schema.hashes [] (V.toList rendered)])
    (nonEmptyVector (V.mapMaybe entryEl entries))

entryEl :: ManifestEntry -> Maybe Node
entryEl = \case
  ManifestEntry.File e -> Just (fileEntryEl e)
  ManifestEntry.Directory d
    | V.null d.hashes && V.null d.unknown -> Nothing
    | otherwise -> Just (dirEntryEl d)

fileEntryEl :: FileEntry -> Node
fileEntryEl e =
  manifestEl Schema.hash [] $
    manifestElWith
      Schema.path
      (Xml.ownAttrs (sizeAttr e.size <> [(Schema.lastmodificationdate, Time.render e.lastModified)]) <> e.pathAttrs)
      [Xml.txt (display e.path)]
      : inSchemaOrder (fileHashes e <> keptNamed e.unknown)

dirEntryEl :: DirectoryEntry -> Node
dirEntryEl d =
  manifestEl Schema.directoryhash [] $
    manifestElWith
      Schema.path
      (Xml.ownAttrs [(Schema.lastmodificationdate, Time.render d.lastModified)] <> d.pathAttrs)
      [Xml.txt (display d.path)]
      : (maybe [] dirHashChildren (nonEmptyVector d.hashes) <> Xml.kept d.unknown)

dirHashChildren :: Vector DirHash -> List Node
dirHashChildren hashes =
  [ manifestEl Schema.content [] (V.toList (V.map (\d -> dirFormatEl d.content d.hashDate d.extraAttrs) hashes))
  , manifestEl Schema.structure [] (V.toList (V.map (\d -> dirFormatEl d.structure d.hashDate Map.empty) hashes))
  ]

dirFormatEl :: Hash -> Maybe UTCTime -> Map Name Text -> Node
dirFormatEl h hashDate extra =
  manifestElWith (display h.algo) (Xml.ownAttrs (hashDateAttr hashDate) <> extra) [Xml.txt h.value]

fileHashes :: FileEntry -> List (Text, Node)
fileHashes e = e.hashes & V.toList & map (\mh -> (display mh.hash.algo, fileFormatEl mh))

keptNamed :: Vector Node -> List (Text, Node)
keptNamed nodes = nodes & V.toList & map (\nd -> (keptLocalName nd, nd))

fileFormatEl :: FileEntry.HashRecord -> Node
fileFormatEl mh =
  manifestElWith
    (display mh.hash.algo)
    (Xml.ownAttrs ([(Schema.action, display mh.action)] <> hashDateAttr mh.hashDate) <> mh.extraAttrs)
    [Xml.txt mh.hash.value]

hashDateAttr :: Maybe UTCTime -> List (Text, Text)
hashDateAttr hashDate = maybe [] (\t -> [(Schema.hashdate, Time.render t)]) hashDate

sizeAttr :: Int64 -> List (Text, Text)
sizeAttr = \case
  0 -> []
  bytes -> [(Schema.size, T.show bytes)]
