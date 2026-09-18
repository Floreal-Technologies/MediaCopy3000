module Ascmhl.History
  ( History (..)
  , Generation (..)
  , fromManifests
  ) where

import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V

import Ascmhl.Algo (Algo)
import Ascmhl.Creator (Creator)
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Hash qualified as Hash
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.Process (Process)

data Generation = Generation
  { number :: Int
  , creator :: Creator
  , algos :: Set Algo
  , process :: Process
  , failures :: Int
  }
  deriving stock (Eq, Show)

newtype History = History {generations :: Vector Generation}
  deriving stock (Eq, Show)

fromManifests :: Vector (Int, Manifest) -> History
fromManifests ms = History (V.map gen ms)
  where
    gen (n, m) =
      Generation
        { number = n
        , creator = m.creator
        , algos = allHashes & V.toList & map (.hash.algo) & Set.fromList
        , process = m.process
        , failures = V.length (V.filter (\mh -> mh.action == FileEntry.Failed) allHashes)
        }
      where
        allHashes = foldMap (.hashes) (ManifestEntry.files m.entries)
