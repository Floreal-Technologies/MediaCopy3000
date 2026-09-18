module Ascmhl.Fragment
  ( Fragment (..)
  , parse
  , isXmlChar
  ) where

import Control.Exception (SomeException, displayException)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Document (..), Element (..), Name (..), Node (..), def, parseText)

newtype Fragment = Fragment (Vector Node)
  deriving stock (Eq, Show)

parse :: Text -> Text -> Either Text Fragment
parse ns txt = do
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
