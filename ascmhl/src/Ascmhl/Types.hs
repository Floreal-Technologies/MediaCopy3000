module Ascmhl.Types
  ( HashAction (..)
  , ManifestHash (..)
  , DirHash (..)
  , HashEntry (..)
  , DirectoryEntry (..)
  , ManifestEntry (..)
  , CreatorInfo (..)
  , ProcessKind (..)
  , Manifest (..)
  , ChainEntry (..)
  , Chain (..)
  , Generation (..)
  , MhlHistory (..)
  , actionFromName
  , algosText
  , processFromName
  , fileEntries
  , latestHashes
  , historyOf
  , orderedChainEntries
  , highestGeneration
  ) where

import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.XML (Name, Node)

import Ascmhl.Hash
import Ascmhl.Path (RelPath)

data HashAction = Original | Verified | FailedAction
  deriving stock (Eq, Show)

-- |
-- >>> map display [Original, Verified, FailedAction]
-- ["original","verified","failed"]
instance Display HashAction where
  displayBuilder = \case
    Original -> "original"
    Verified -> "verified"
    FailedAction -> "failed"

-- |
-- >>> actionFromName "original"
-- Just Original
-- >>> actionFromName "new"
-- Just Verified
-- >>> actionFromName "renamed"
-- Nothing
actionFromName :: Text -> Maybe HashAction
actionFromName = \case
  "original" -> Just Original
  ("verified"; "new") -> Just Verified
  "failed" -> Just FailedAction
  _ -> Nothing

data ManifestHash = ManifestHash
  { hash :: Hash
  , action :: HashAction
  , hashDate :: Maybe UTCTime
  , extraAttrs :: Map Name Text
  }
  deriving stock (Eq, Show)

data DirHash = DirHash
  { content :: Hash
  , structure :: Hash
  , hashDate :: Maybe UTCTime
  , extraAttrs :: Map Name Text
  }
  deriving stock (Eq, Show)

data HashEntry = HashEntry
  { path :: RelPath
  , size :: Int64
  , lastModified :: UTCTime
  , hashes :: Vector ManifestHash
  , pathAttrs :: Map Name Text
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

data DirectoryEntry = DirectoryEntry
  { path :: RelPath
  , lastModified :: UTCTime
  , hashes :: Vector DirHash
  , pathAttrs :: Map Name Text
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

data ManifestEntry = ManifestFile HashEntry | ManifestDir DirectoryEntry
  deriving stock (Eq, Show)

data CreatorInfo = CreatorInfo
  { creationDate :: UTCTime
  , hostname :: Text
  , toolName :: Text
  , toolVersion :: Maybe Text
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

data ProcessKind = ProcessTransfer | ProcessInPlace | ProcessFlatten
  deriving stock (Eq, Show)

-- |
-- >>> map display [ProcessTransfer, ProcessInPlace, ProcessFlatten]
-- ["transfer","in-place","flatten"]
instance Display ProcessKind where
  displayBuilder = \case
    ProcessTransfer -> "transfer"
    ProcessInPlace -> "in-place"
    ProcessFlatten -> "flatten"

-- |
-- >>> processFromName "transfer"
-- Just ProcessTransfer
-- >>> processFromName "verify"
-- Just ProcessInPlace
-- >>> processFromName "copy"
-- Nothing
processFromName :: Text -> Maybe ProcessKind
processFromName = \case
  "transfer" -> Just ProcessTransfer
  ("in-place"; "verify") -> Just ProcessInPlace
  "flatten" -> Just ProcessFlatten
  _ -> Nothing

data Manifest = Manifest
  { creator :: CreatorInfo
  , process :: ProcessKind
  , rootHash :: Vector DirHash
  , ignorePatterns :: Vector Text
  , entries :: Vector ManifestEntry
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

fileEntries :: Vector ManifestEntry -> Vector HashEntry
fileEntries entries =
  V.mapMaybe
    ( \case
        ManifestFile fe -> Just fe
        ManifestDir _ -> Nothing
    )
    entries

data ChainEntry = ChainEntry
  { sequenceNr :: Int
  , path :: RelPath
  , c4 :: Maybe Hash
  , unknown :: Vector Node
  }
  deriving stock (Eq, Show)

newtype Chain = Chain {entries :: Vector ChainEntry}
  deriving stock (Eq, Show)

data Generation = Generation
  { number :: Int
  , creator :: CreatorInfo
  , algos :: Set HashAlgo
  , process :: ProcessKind
  , failures :: Int
  }
  deriving stock (Eq, Show)

newtype MhlHistory = MhlHistory {generations :: Vector Generation}
  deriving stock (Eq, Show)

-- |
-- >>> algosText (Set.fromList [SHA1, XXH64])
-- "xxh64/sha1"
algosText :: Set HashAlgo -> Text
algosText algos = algos & Set.toList & map (\algo -> display algo) & T.intercalate "/"

orderedChainEntries :: Chain -> List ChainEntry
orderedChainEntries chain = chain.entries & V.toList & sortOn (\entry -> entry.sequenceNr)

highestGeneration :: Chain -> Int
highestGeneration chain = chain.entries & V.map (\entry -> entry.sequenceNr) & V.toList & foldr max 0

latestHashes :: Vector Manifest -> Map RelPath Hash
latestHashes manifests = foldl step Map.empty manifests
  where
    step acc m = foldl (\a e -> maybe a (\h -> Map.insert e.path h a) (pick e.hashes)) acc (fileEntries m.entries)
    pick hs = case V.find (\mh -> mh.hash.algo == preferredAlgo) hs of
      Just mh -> Just mh.hash
      Nothing -> fmap (\mh -> mh.hash) (hs V.!? 0)

historyOf :: Vector (Int, Manifest) -> MhlHistory
historyOf ms = MhlHistory (V.map gen ms)
  where
    gen (n, m) =
      Generation
        { number = n
        , creator = m.creator
        , algos = allHashes & V.toList & map (\mh -> mh.hash.algo) & Set.fromList
        , process = m.process
        , failures = V.length (V.filter (\mh -> mh.action == FailedAction) allHashes)
        }
      where
        allHashes = foldMap (\e -> e.hashes) (fileEntries m.entries)
