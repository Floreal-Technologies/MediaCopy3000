module Ascmhl.Xml
  ( nsName
  , metadataNode
  , parseDoc
  , requireRoot
  , require
  , angled
  , attr
  , localName
  , readIntegral
  , hashElement
  , pathElement
  , childrenExcept
  , renderDoc
  , el
  , elWith
  , ownAttrs
  , txt
  , kept
  ) where

import Control.Exception (SomeException, displayException)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Document (..), Element (..), Name (..), Node (..), Prologue (..), def, parseText, renderText)
import Text.XML.Cursor (Cursor, anyElement, attribute, content, fromDocument, laxElement, node, ($/))

import Ascmhl.Algo qualified as Algo
import Ascmhl.Fragment (Fragment)
import Ascmhl.Fragment qualified as Fragment
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Ascmhl.Schema qualified as Schema

nsName :: Text -> Text -> Name
nsName ns local = Name local (Just ns) Nothing

metadataNode :: Fragment -> Node
metadataNode (Fragment.Fragment nodes) = NodeElement (Element (nsName Schema.manifestNs Schema.metadata) Map.empty (V.toList nodes))

parseDoc :: Text -> Either Text Cursor
parseDoc t =
  first
    (\e -> displayException @SomeException e & T.pack & ("ASC MHL: " <>))
    (fromDocument <$> parseText def (TL.fromStrict t))

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
  algo <- Algo.fromMhlElement local
  let raw = T.strip (T.concat (hc $/ content))
  pure (Hash.Hash algo ((Algo.spec algo).readValue raw))

pathElement :: Text -> Cursor -> Either Text (Cursor, RelPath)
pathElement what c = do
  pathC <- require (angled Schema.path) (listToMaybe (c $/ laxElement Schema.path))
  let pathValue = T.concat (pathC $/ content)
  path <- maybe (Left ("ASC MHL: " <> what <> " is not relative: " <> pathValue)) Right (RelPath.parse pathValue)
  Right (pathC, path)

childrenExcept :: (Text -> Bool) -> Cursor -> Vector Node
childrenExcept known c =
  kids
    & filter (maybe True (not . known) . localName)
    & map node
    & V.fromList
  where
    kids = c $/ anyElement

renderDoc :: Element -> Text
renderDoc root = TL.toStrict (renderText def (Document (Prologue [] Nothing []) root []))

ownAttrs :: List (Text, Text) -> Map Name Text
ownAttrs attrs = attrs & map (first (\k -> Name k Nothing Nothing)) & Map.fromList

el :: Text -> Text -> List (Text, Text) -> List Node -> Node
el ns local attrs kids = elWith ns local (ownAttrs attrs) kids

elWith :: Text -> Text -> Map Name Text -> List Node -> Node
elWith ns local attrs kids = NodeElement (Element (nsName ns local) attrs kids)

txt :: Text -> Node
txt value = NodeContent value

kept :: Vector Node -> List Node
kept nodes = V.toList nodes
