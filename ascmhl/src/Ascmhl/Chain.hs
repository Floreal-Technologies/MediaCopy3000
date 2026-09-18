module Ascmhl.Chain
  ( Chain (..)
  , inOrder
  , highestGeneration
  , append
  , fromListing
  , parse
  , render
  ) where

import Data.Function ((&))
import Data.List (List, find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath)
import Text.XML (Element (..))
import Text.XML.Cursor (anyElement, laxElement, ($/))

import Ascmhl.Algo qualified as Algo
import Ascmhl.ChainEntry (ChainEntry)
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Ascmhl.Schema qualified as Schema
import Ascmhl.Xml qualified as Xml

newtype Chain = Chain {entries :: Vector ChainEntry}
  deriving stock (Eq, Show)

inOrder :: Chain -> List ChainEntry
inOrder chain = chain.entries & V.toList & sortOn (.sequenceNr)

highestGeneration :: Chain -> Int
highestGeneration chain = chain.entries & V.map (.sequenceNr) & V.toList & foldr max 0

append :: Chain -> Int -> RelPath -> Hash -> Chain
append chain number name c4 =
  Chain {entries = V.fromList (inOrder chain <> [ChainEntry.new number name (Just c4)])}

fromListing :: Vector OsPath -> Chain
fromListing names =
  names
    & V.toList
    & mapMaybe (RelPath.parse . OsPath.toText)
    & zipWith entryFor [1 ..]
    & V.fromList
    & Chain
  where
    entryFor index p = ChainEntry.new (sequenceOf index p) p Nothing

-- |
-- >>> sequenceOf 7 (RelPath.RelPath "0003_card_a.mhl")
-- 3
-- >>> sequenceOf 7 (RelPath.RelPath "legacy.mhl")
-- 7
sequenceOf :: Int -> RelPath -> Int
sequenceOf index p = case TR.decimal (display p) of
  Right (parsed, _rest) -> parsed
  Left _notANumber -> index

parse :: Text -> Either Text Chain
parse document = do
  root <- Xml.parseDoc document
  Xml.requireRoot Schema.chainNs Schema.ascmhldirectory root
  entriesList <- traverse entry (root $/ laxElement Schema.hashlist)
  pure (Chain (V.fromList entriesList))
  where
    entry c = do
      sequenceNr <- Xml.require Schema.sequencenr (Xml.attr Schema.sequencenr c >>= Xml.readIntegral)
      (_, path) <- Xml.pathElement "chain path" c
      let kids = c $/ anyElement
          c4 = kids & mapMaybe Xml.hashElement & find (\h -> h.algo == Algo.C4)
          isC4 local = Algo.fromMhlElement local == Just Algo.C4
      pure ChainEntry.ChainEntry {sequenceNr, path, c4, unknown = Xml.childrenExcept (\local -> local == Schema.path || isC4 local) c}

render :: Chain -> Text
render chain = Xml.renderDoc root
  where
    n = Schema.chainNs
    root = Element (Xml.nsName n Schema.ascmhldirectory) Map.empty (V.toList (V.map entry chain.entries))
    entry e =
      Xml.el n Schema.hashlist [(Schema.sequencenr, T.show e.sequenceNr)] $
        Xml.el n Schema.path [] [Xml.txt (display e.path)]
          : (maybe [] (\h -> [Xml.el n (display h.algo) [] [Xml.txt h.value]]) e.c4 <> Xml.kept e.unknown)
