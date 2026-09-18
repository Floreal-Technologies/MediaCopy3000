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

-- | Format a time for MHL output in ISO 8601 format with UTC offset.
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

-- | This project's own attributes, which carry no namespace of their own.
ownAttrs :: List (Text, Text) -> Map Name Text
ownAttrs attrs = attrs & map (first (\k -> Name k Nothing Nothing)) & Map.fromList

el :: Text -> Text -> List (Text, Text) -> List Node -> Node
el ns local attrs kids = elWith ns local (ownAttrs attrs) kids

-- | An attribute this project writes wins over one it kept. A kept attribute of the same
-- name is one this project has since parsed into a field.
elWith :: Text -> Text -> Map Name Text -> List Node -> Node
elWith ns local attrs kids = NodeElement (Element (nsName ns local) attrs kids)

txt :: Text -> Node
txt content = NodeContent content

renderDoc :: Element -> Text
renderDoc root = TL.toStrict (renderText def (Document (Prologue [] Nothing []) root []))

-- | 'Nothing' for an empty vector, so a caller writes no element at all for an absent block.
nonEmptyVector :: Vector a -> Maybe (Vector a)
nonEmptyVector v = if V.null v then Nothing else Just v

-- | The local name of a kept node, for the one place that must sort kept hash formats.
keptLocalName :: Node -> Text
keptLocalName = \case
  NodeElement e -> nameLocalName e.elementName
  _ -> ""

-- | 'HashType' declares its hash formats as an ordered sequence. As a result, a kept
-- format sorts back into its place and does not trail the ones this project understands.
inSchemaOrder :: List (Text, Node) -> List Node
inSchemaOrder named = named & sortOn (\pair -> position (fst pair)) & map (\pair -> snd pair)
  where
    position name = fromMaybe (V.length Schema.hashFormatOrder) (V.elemIndex name Schema.hashFormatOrder)

-- | Every element of a manifest sits in the manifest namespace. The writer names it here, rather
-- than at each of the twenty places that build one.
manifestEl :: Text -> List (Text, Text) -> List Node -> Node
manifestEl = el Schema.manifestNs

manifestElWith :: Text -> Map Name Text -> List Node -> Node
manifestElWith = elWith Schema.manifestNs

-- | The children a reader kept as it found them, put back where they were.
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

-- | @\<creatorinfo\>@: what wrote the manifest, and when.
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

-- | The children of @\<processinfo\>@: what was done, the tree's own hashes, and what it left out.
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

-- | 'HashesType' requires a child, so a manifest with no rendered entry omits the element.
hashesEl :: Vector ManifestEntry -> List Node
hashesEl entries =
  maybe
    []
    (\rendered -> [manifestEl Schema.hashes [] (V.toList rendered)])
    (nonEmptyVector (V.mapMaybe entryEl entries))

-- | 'Nothing' for a whole entry that has nothing left to say. This code omits @\<roothash\>@ the same way.
entryEl :: ManifestEntry -> Maybe Node
entryEl = \case
  ManifestFile e -> Just (fileEntryEl e)
  ManifestDir d
    | V.null d.hashes && V.null d.unknown -> Nothing
    | otherwise -> Just (dirEntryEl d)

-- | @\<hash\>@: one file, its path, and every hash format recorded for it.
fileEntryEl :: HashEntry -> Node
fileEntryEl e =
  manifestEl Schema.hash [] $
    manifestElWith
      Schema.path
      (ownAttrs (sizeAttr e.size <> [(Schema.lastmodificationdate, formatMhlTime e.lastModified)]) <> e.pathAttrs)
      [txt (display e.path)]
      : inSchemaOrder (fileHashes e <> keptNamed e.unknown)

-- | @\<directoryhash\>@: one directory, its path, and its content and structure hashes.
dirEntryEl :: DirectoryEntry -> Node
dirEntryEl d =
  manifestEl Schema.directoryhash [] $
    manifestElWith
      Schema.path
      (ownAttrs [(Schema.lastmodificationdate, formatMhlTime d.lastModified)] <> d.pathAttrs)
      [txt (display d.path)]
      : (maybe [] dirHashChildren (nonEmptyVector d.hashes) <> kept d.unknown)

-- | The reference parser needs <content> before <structure>.
dirHashChildren :: Vector DirHash -> List Node
dirHashChildren hashes =
  [ manifestEl Schema.content [] (V.toList (V.map (\d -> dirFormatEl d.content d.hashDate d.extraAttrs) hashes))
  , manifestEl Schema.structure [] (V.toList (V.map (\d -> dirFormatEl d.structure d.hashDate Map.empty) hashes))
  ]

dirFormatEl :: Hash -> Maybe UTCTime -> Map Name Text -> Node
dirFormatEl h hashDate extra =
  manifestElWith (display h.algo) (ownAttrs (hashDateAttr hashDate) <> extra) [txt h.value]

-- | A file's own hash elements, each under the name 'inSchemaOrder' sorts on.
fileHashes :: HashEntry -> List (Text, Node)
fileHashes e = e.hashes & V.toList & map (\mh -> (display mh.hash.algo, fileFormatEl mh))

-- | The same, for the formats this project kept without understanding them.
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

-- | The reference omits size for a zero-byte file because it writes the attribute under a truthiness test.
sizeAttr :: Int64 -> List (Text, Text)
sizeAttr = \case
  0 -> []
  bytes -> [(Schema.size, T.pack (show bytes))]

-- | The chain root has no version attribute and the chain path has no size: neither is in the schema.
renderChain :: Chain -> Text
renderChain c = renderDoc root
  where
    n = Schema.chainNs
    root = Element (nsName n Schema.ascmhldirectory) Map.empty (V.toList (V.map entry c.entries))
    entry e =
      el n Schema.hashlist [(Schema.sequencenr, T.pack (show e.sequenceNr))] $
        el n Schema.path [] [txt (display e.path)]
          : (maybe [] (\h -> [el n (display h.algo) [] [txt h.value]]) e.c4 <> kept e.unknown)
