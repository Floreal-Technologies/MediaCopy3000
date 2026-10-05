module Ascmhl.Read
  ( parseManifest
  , parseChain
  , parseMhlTime
  , parseFragment
  , isXmlChar
  ) where

import Control.Exception (SomeException, displayException)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
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
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML
import Text.XML.Cursor

import Ascmhl.Hash
import Ascmhl.Path (RelPath, mkRelPath)
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
    (\e -> displayException @SomeException e & T.pack & ("ASC MHL: " <>))
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
    & filter (maybe True (not . known) . localName)
    & map node
    & V.fromList
  where
    kids = c $/ anyElement

-- |
-- >>> let creator = "<creatorinfo><creationdate>2026-01-01T00:00:00Z</creationdate><hostname>h</hostname><tool>t</tool><author email=\"jane@example.com\" role=\"DIT\">Jane Doe</author><author>Sam Roe</author><comment>c</comment></creatorinfo>"
-- >>> fmap (\m -> (m.creator.authors, length m.creator.unknown)) (parseManifest ("<hashlist xmlns=\"urn:ASC:MHL:v2.0\" version=\"2.0\">" <> creator <> "</hashlist>"))
-- Right ([Author {name = "Jane Doe", email = Just "jane@example.com", phone = Nothing, role = Just "DIT"},Author {name = "Sam Roe", email = Nothing, phone = Nothing, role = Nothing}],1)
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
      authors = V.fromList (map authorOf (ci $/ laxElement Schema.author))
      creatorUnknown =
        childrenExcept (\local -> local `elem` [Schema.creationdate, Schema.hostname, Schema.tool, Schema.author]) ci
  rootHash <- case listToMaybe (root $/ laxElement Schema.processinfo &/ laxElement Schema.roothash) of
    Nothing -> Right V.empty
    Just pc -> dirHashesOf (angled Schema.roothash) pc
  entriesList <- traverse parseEntry (root $/ laxElement Schema.hashes &/ anyElement)
  let entries = V.fromList (catMaybes entriesList)
  pure
    Manifest
      { creator = CreatorInfo {creationDate, hostname, toolName, toolVersion, authors, unknown = creatorUnknown}
      , process
      , rootHash
      , ignorePatterns
      , entries
      , unknown = childrenExcept (\local -> local `elem` [Schema.creatorinfo, Schema.processinfo, Schema.hashes]) root
      }

pathElement :: Text -> Cursor -> Either Text (Cursor, RelPath)
pathElement what c = do
  pathC <- require (angled Schema.path) (listToMaybe (c $/ laxElement Schema.path))
  let pathValue = T.concat (pathC $/ content)
  path <- maybe (Left ("ASC MHL: " <> what <> " is not relative: " <> pathValue)) Right (mkRelPath pathValue)
  Right (pathC, path)

authorOf :: Cursor -> Author
authorOf c =
  Author
    { name = T.concat (c $/ content)
    , email = attr Schema.email c
    , phone = attr Schema.phone c
    , role = attr Schema.role c
    }

parseEntry :: Cursor -> Either Text (Maybe ManifestEntry)
parseEntry c = case localName c of
  Just local
    | local == Schema.hash -> fmap (Just . ManifestFile) (parseFileEntry c)
    | local == Schema.directoryhash -> fmap (Just . ManifestDir) (parseDirEntry c)
  _ -> Right Nothing

parseFileEntry :: Cursor -> Either Text HashEntry
parseFileEntry c = do
  (pathC, path) <- pathElement "path" c
  let size = fromMaybe 0 (attr Schema.size pathC >>= readIntegral)
  lastModified <- require Schema.lastmodificationdate (attr Schema.lastmodificationdate pathC >>= parseMhlTime)
  let hashes = V.fromList (mapMaybe manifestHashOf (c $/ anyElement))
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
  (pathC, path) <- pathElement "path" c
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
  paired <- traverse withStructure contents
  traverse_ requireContent structures
  Right (V.fromList paired)
  where
    contents = mapMaybe (\hc -> fmap (\h -> (h, hc)) (hashElement hc)) (c $/ laxElement Schema.content &/ anyElement)
    structures = mapMaybe hashElement (c $/ laxElement Schema.structure &/ anyElement)
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
      (_, path) <- pathElement "chain path" c
      let kids = c $/ anyElement
          c4 = kids & mapMaybe hashElement & find (\h -> h.algo == C4)
          isC4 local = algoFromMhlElement local == Just C4
      pure ChainEntry {sequenceNr, path, c4, unknown = childrenExcept (\local -> local == Schema.path || isC4 local) c}

-- |
-- >>> fmap (\(Fragment nodes) -> length nodes) (parseFragment "urn:x" "<a xmlns=\"urn:x\">1</a> <b xmlns=\"urn:x\"/>")
-- Right 2
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:y\"/>"
-- Left "metadata: <a> is in namespace urn:y, not urn:x"
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:x\"><b xmlns=\"\"/></a>"
-- Left "metadata: <b> is in no namespace, not urn:x"
-- >>> parseFragment "urn:x" "loose text"
-- Left "metadata: text outside an element"
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:x\"><!-- v2 -- beta --><?pi x?>1</a>" == parseFragment "urn:x" "<a xmlns=\"urn:x\">1</a>"
-- True
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:x\" xmlns:m=\"urn:ASC:MHL:v2.0\" m:size=\"1\"/>"
-- Left "metadata: <a> has the attribute size in namespace urn:ASC:MHL:v2.0, not urn:x"
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:x\">a\SOHb</a>"
-- Left "metadata: <a> holds a character that XML does not allow"
-- >>> parseFragment "urn:x" "<a xmlns=\"urn:x\" b=\"\SOH\"/>"
-- Left "metadata: <a> holds a character that XML does not allow"
-- >>> either id (const "accepted") (parseFragment "urn:x" (T.replicate 33 "<a xmlns=\"urn:x\">" <> T.replicate 33 "</a>"))
-- "metadata: more than 32 levels of elements"
parseFragment :: Text -> Text -> Either Text Fragment
parseFragment ns txt = do
  doc <-
    first
      (\e -> "metadata: " <> T.pack (displayException @SomeException e))
      (parseText def (TL.fromStrict ("<fragment>" <> txt <> "</fragment>")))
  kids <- fmap catMaybes . for (elementNodes (documentRoot doc)) $ \case
    NodeContent loose
      | T.null (T.strip loose) -> Right Nothing
      | otherwise -> Left "metadata: text outside an element"
    NodeElement e -> Just . NodeElement <$> inNamespace ns 1 e
    _ -> Right Nothing
  Right (Fragment (V.fromList kids))

maxDepth :: Int
maxDepth = 32

inNamespace :: Text -> Int -> Element -> Either Text Element
inNamespace ns depth e = case nameNamespace e.elementName of
  _ | depth > maxDepth -> Left ("metadata: more than " <> T.show maxDepth <> " levels of elements")
  Just found | found == ns -> do
    for_ (Map.toList e.elementAttributes) $ \(name, value) -> case nameNamespace name of
      Just other | other /= ns -> Left ("metadata: <" <> nameLocalName e.elementName <> "> has the attribute " <> nameLocalName name <> " in namespace " <> other <> ", not " <> ns)
      _ -> xmlText value
    kids <- fmap catMaybes . for e.elementNodes $ \case
      NodeElement child -> Just . NodeElement <$> inNamespace ns (depth + 1) child
      NodeContent text -> xmlText text >> Right (Just (NodeContent text))
      _ -> Right Nothing
    Right e {elementNodes = kids}
  Just found -> Left (outside ("namespace " <> found))
  Nothing -> Left (outside "no namespace")
  where
    outside found = "metadata: <" <> nameLocalName e.elementName <> "> is in " <> found <> ", not " <> ns
    xmlText text
      | T.all isXmlChar text = Right ()
      | otherwise = Left ("metadata: <" <> nameLocalName e.elementName <> "> holds a character that XML does not allow")

-- |
-- >>> map isXmlChar ['a', '\t', '\SOH', '\xFFFE', '\x1F600']
-- [True,True,False,False,True]
isXmlChar :: Char -> Bool
isXmlChar c =
  c == '\t' || c == '\n' || c == '\r' || (c >= ' ' && c <= '\xD7FF') || (c >= '\xE000' && c <= '\xFFFD') || c >= '\x10000'
