module Ascmhl.Write
  ( renderManifest
  , renderChain
  , manifestFileName
  , formatMhlTime
  ) where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML

import Ascmhl.Hash
import Ascmhl.Schema qualified as Schema
import Ascmhl.Types

formatMhlTime :: UTCTime -> Text
formatMhlTime t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" t)

manifestFileName :: Int -> Text -> UTCTime -> Text
manifestFileName n folder t =
  T.justifyRight 4 '0' (T.pack (show n))
    <> "_"
    <> folder
    <> "_"
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" t)
    <> ".mhl"

nsName :: Text -> Text -> Name
nsName ns local = Name local (Just ns) Nothing

ownAttrs :: List (Text, Text) -> Map Name Text
ownAttrs attrs = attrs & map (first (\k -> Name k Nothing Nothing)) & Map.fromList

el :: Text -> Text -> List (Text, Text) -> List Node -> Node
el ns local attrs kids = elWith ns local (ownAttrs attrs) kids

elWith :: Text -> Text -> Map Name Text -> List Node -> Node
elWith ns local attrs kids = NodeElement (Element (nsName ns local) attrs kids)

txt :: Text -> Node
txt content = NodeContent content

renderDoc :: Element -> Text
renderDoc root = TL.toStrict (renderText def (Document (Prologue [] Nothing []) root []))

nonEmptyVector :: Vector a -> Maybe (Vector a)
nonEmptyVector v = if V.null v then Nothing else Just v

keptLocalName :: Node -> Text
keptLocalName = \case
  NodeElement e -> nameLocalName e.elementName
  _ -> ""

inSchemaOrder :: List (Text, Node) -> List Node
inSchemaOrder named = named & sortOn (\pair -> position (fst pair)) & map (\pair -> snd pair)
  where
    position name = fromMaybe (V.length Schema.hashFormatOrder) (V.elemIndex name Schema.hashFormatOrder)

manifestEl :: Text -> List (Text, Text) -> List Node -> Node
manifestEl = el Schema.manifestNs

manifestElWith :: Text -> Map Name Text -> List Node -> Node
manifestElWith = elWith Schema.manifestNs

kept :: Vector Node -> List Node
kept nodes = V.toList nodes

renderManifest :: Manifest -> Text
renderManifest m = renderDoc root
  where
    root =
      Element
        (nsName Schema.manifestNs Schema.hashlist)
        (Map.fromList [(Name Schema.version Nothing Nothing, Schema.supportedVersion)])
        ( [ creatorInfoEl m.creator
          , manifestEl Schema.processinfo [] (processInfoChildren m)
          ]
            <> hashesEl m.entries
            <> kept m.unknown
        )

creatorInfoEl :: CreatorInfo -> Node
creatorInfoEl creator =
  manifestEl
    Schema.creatorinfo
    []
    ( [ manifestEl Schema.creationdate [] [txt (formatMhlTime creator.creationDate)]
      , manifestEl Schema.hostname [] [txt creator.hostname]
      , manifestEl Schema.tool (toolVersionAttr creator.toolVersion) [txt creator.toolName]
      ]
        <> kept creator.unknown
    )

toolVersionAttr :: Maybe Text -> List (Text, Text)
toolVersionAttr = maybe [] (\v -> [(Schema.version, v)])

processInfoChildren :: Manifest -> List Node
processInfoChildren m =
  [manifestEl Schema.process [] [txt (display m.process)]]
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
ignorePatternEl p = manifestEl Schema.ignorePattern [] [txt p]

hashesEl :: Vector ManifestEntry -> List Node
hashesEl entries =
  maybe
    []
    (\rendered -> [manifestEl Schema.hashes [] (V.toList rendered)])
    (nonEmptyVector (V.mapMaybe entryEl entries))

entryEl :: ManifestEntry -> Maybe Node
entryEl = \case
  ManifestFile e -> Just (fileEntryEl e)
  ManifestDir d
    | V.null d.hashes && V.null d.unknown -> Nothing
    | otherwise -> Just (dirEntryEl d)

fileEntryEl :: HashEntry -> Node
fileEntryEl e =
  manifestEl Schema.hash [] $
    manifestElWith
      Schema.path
      (ownAttrs (sizeAttr e.size <> [(Schema.lastmodificationdate, formatMhlTime e.lastModified)]) <> e.pathAttrs)
      [txt (display e.path)]
      : inSchemaOrder (fileHashes e <> keptNamed e.unknown)

dirEntryEl :: DirectoryEntry -> Node
dirEntryEl d =
  manifestEl Schema.directoryhash [] $
    manifestElWith
      Schema.path
      (ownAttrs [(Schema.lastmodificationdate, formatMhlTime d.lastModified)] <> d.pathAttrs)
      [txt (display d.path)]
      : (maybe [] dirHashChildren (nonEmptyVector d.hashes) <> kept d.unknown)

dirHashChildren :: Vector DirHash -> List Node
dirHashChildren hashes =
  [ manifestEl Schema.content [] (V.toList (V.map (\d -> dirFormatEl d.content d.hashDate d.extraAttrs) hashes))
  , manifestEl Schema.structure [] (V.toList (V.map (\d -> dirFormatEl d.structure d.hashDate Map.empty) hashes))
  ]

dirFormatEl :: Hash -> Maybe UTCTime -> Map Name Text -> Node
dirFormatEl h hashDate extra =
  manifestElWith (display h.algo) (ownAttrs (hashDateAttr hashDate) <> extra) [txt h.value]

fileHashes :: HashEntry -> List (Text, Node)
fileHashes e = e.hashes & V.toList & map (\mh -> (display mh.hash.algo, fileFormatEl mh))

keptNamed :: Vector Node -> List (Text, Node)
keptNamed nodes = nodes & V.toList & map (\nd -> (keptLocalName nd, nd))

fileFormatEl :: ManifestHash -> Node
fileFormatEl mh =
  manifestElWith
    (display mh.hash.algo)
    (ownAttrs ([(Schema.action, display mh.action)] <> hashDateAttr mh.hashDate) <> mh.extraAttrs)
    [txt mh.hash.value]

hashDateAttr :: Maybe UTCTime -> List (Text, Text)
hashDateAttr hashDate = maybe [] (\t -> [(Schema.hashdate, formatMhlTime t)]) hashDate

sizeAttr :: Int64 -> List (Text, Text)
sizeAttr = \case
  0 -> []
  bytes -> [(Schema.size, T.pack (show bytes))]

renderChain :: Chain -> Text
renderChain c = renderDoc root
  where
    n = Schema.chainNs
    root = Element (nsName n Schema.ascmhldirectory) Map.empty (V.toList (V.map entry c.entries))
    entry e =
      el n Schema.hashlist [(Schema.sequencenr, T.pack (show e.sequenceNr))] $
        el n Schema.path [] [txt (display e.path)]
          : (maybe [] (\h -> [el n (display h.algo) [] [txt h.value]]) e.c4 <> kept e.unknown)
