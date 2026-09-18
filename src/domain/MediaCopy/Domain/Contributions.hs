module MediaCopy.Domain.Contributions
  ( Contributions (..)
  , empty
  , merge
  , joinFragments
  ) where

import Ascmhl.Author (Author)
import Ascmhl.Fragment (Fragment)
import Ascmhl.Fragment qualified as Fragment
import Ascmhl.RelPath (RelPath)
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import MediaCopy.Domain.PluginRef (PluginRef)

data Contributions = Contributions
  { authors :: Vector Author
  , fileMetadata :: Map RelPath Fragment
  , manifestMetadata :: Maybe Fragment
  , contributors :: Vector PluginRef
  }
  deriving stock (Eq, Show)

empty :: Contributions
empty = Contributions {authors = V.empty, fileMetadata = Map.empty, manifestMetadata = Nothing, contributors = V.empty}

merge :: Vector Contributions -> Contributions
merge = foldl' combine empty
  where
    combine acc next =
      Contributions
        { authors = acc.authors <> next.authors
        , fileMetadata = Map.unionWith joinFragments acc.fileMetadata next.fileMetadata
        , manifestMetadata = case (acc.manifestMetadata, next.manifestMetadata) of
            (Just earlier, Just later) -> Just (joinFragments earlier later)
            (earlier, later) -> earlier <|> later
        , contributors = acc.contributors <> next.contributors
        }

joinFragments :: Fragment -> Fragment -> Fragment
joinFragments (Fragment.Fragment earlier) (Fragment.Fragment later) = Fragment.Fragment (earlier <> later)
