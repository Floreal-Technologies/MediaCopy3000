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

-- | The three actions a manifest can record, and the one name a reader also accepts.
data HashAction = Original | Verified | FailedAction
  deriving stock (Eq, Show)

-- | The name a manifest records the action under.
--
-- >>> map display [Original, Verified, FailedAction]
-- ["original","verified","failed"]
instance Display HashAction where
  displayBuilder = \case
    Original -> "original"
    Verified -> "verified"
    FailedAction -> "failed"

-- | The reference rewrites "new" to "verified" before serialising, so it never reaches a file.
--
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
  -- ^ Attributes of the hash element other than @action@ and @hashdate@.
  }
  deriving stock (Eq, Show)

-- | A directory hash never carries an action. The reference never writes one.
data DirHash = DirHash
  { content :: Hash
  , structure :: Hash
  , hashDate :: Maybe UTCTime
  , extraAttrs :: Map Name Text
  -- ^ Attributes of the @\<content\>@ hash element other than @hashdate@.
  }
  deriving stock (Eq, Show)

data HashEntry = HashEntry
  { path :: RelPath
  , size :: Int64
  , lastModified :: UTCTime
  , hashes :: Vector ManifestHash
  , pathAttrs :: Map Name Text
  -- ^ Attributes of @\<path\>@ other than @size@ and @lastmodificationdate@.
  , unknown :: Vector Node
  -- ^ Children of @\<hash\>@ that are neither @\<path\>@ nor a hash format this project implements.
  }
  deriving stock (Eq, Show)

data DirectoryEntry = DirectoryEntry
  { path :: RelPath
  , lastModified :: UTCTime
  , hashes :: Vector DirHash
  , pathAttrs :: Map Name Text
  -- ^ Attributes of @\<path\>@ other than @lastmodificationdate@.
  , unknown :: Vector Node
  -- ^ Children of @\<directoryhash\>@ other than @\<path\>@, @\<content\>@ and @\<structure\>@.
  }
  deriving stock (Eq, Show)

data ManifestEntry = ManifestFile HashEntry | ManifestDir DirectoryEntry
  deriving stock (Eq, Show)

data CreatorInfo = CreatorInfo
  { creationDate :: UTCTime
  , hostname :: Text
  , toolName :: Text
  , toolVersion :: Maybe Text
  -- ^ @\<tool\>@'s @version@ attribute, which the schema leaves optional.
  , unknown :: Vector Node
  -- ^ Children of @\<creatorinfo\>@ other than @\<creationdate\>@, @\<hostname\>@ and @\<tool\>@.
  }
  deriving stock (Eq, Show)

data ProcessKind = ProcessTransfer | ProcessInPlace | ProcessFlatten
  deriving stock (Eq, Show)

-- | The ASC MHL schema allows only `in-place`, `transfer` and `flatten`. A verify generation is in-place.
--
-- >>> map display [ProcessTransfer, ProcessInPlace, ProcessFlatten]
-- ["transfer","in-place","flatten"]
instance Display ProcessKind where
  displayBuilder = \case
    ProcessTransfer -> "transfer"
    ProcessInPlace -> "in-place"
    ProcessFlatten -> "flatten"

-- | The legacy name @verify@ reads as in-place, because this project wrote it before anyone
-- checked the schema.
--
-- >>> processFromName "transfer"
-- Just ProcessTransfer
-- >>> processFromName "verify"
-- Just ProcessInPlace
-- >>> processFromName "copy"
-- Nothing
processFromName :: Text -> Maybe ProcessKind
processFromName = \case
  "transfer" -> Just ProcessTransfer
  -- Legacy: this project wrote "verify" before anyone checked the schema's enumeration, so that
  -- name reads as "in-place" too.
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
  -- ^ Children of @\<hashlist\>@ other than @\<creatorinfo\>@, @\<processinfo\>@ and @\<hashes\>@.
  }
  deriving stock (Eq, Show)

-- | The file entries of a manifest, in document order.
fileEntries :: Vector ManifestEntry -> Vector HashEntry
fileEntries entries =
  V.mapMaybe
    ( \case
        ManifestFile fe -> Just fe
        ManifestDir _ -> Nothing
    )
    entries

-- | @HashlistType@ is the sequence path, c4, so a chain entry has one c4 and nothing
-- else. 'Nothing' is a legacy entry that carried another format or none. A writer recomputes it
-- before it writes the chain again.
data ChainEntry = ChainEntry
  { sequenceNr :: Int
  , path :: RelPath
  , c4 :: Maybe Hash
  , unknown :: Vector Node
  -- ^ Children other than @\<path\>@ and @\<c4\>@, kept so a reader's own additions survive a read.
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

-- | A generation's hash formats as one cell, joined by @\/@. The order is the 'HashAlgo' order,
-- never the order the manifest happened to record them in.
--
-- >>> algosText (Set.fromList [SHA1, XXH64])
-- "xxh64/sha1"
algosText :: Set HashAlgo -> Text
algosText algos = algos & Set.toList & map (\algo -> display algo) & T.intercalate "/"

-- | The chain's own order is its sequence numbers, never the order the entries happen to sit in.
orderedChainEntries :: Chain -> List ChainEntry
orderedChainEntries chain = chain.entries & V.toList & sortOn (\entry -> entry.sequenceNr)

highestGeneration :: Chain -> Int
highestGeneration chain = chain.entries & V.map (\entry -> entry.sequenceNr) & V.toList & foldr max 0

-- | Later manifests override earlier ones. Within one entry 'preferredAlgo' wins, then the first hash.
latestHashes :: Vector Manifest -> Map RelPath Hash
latestHashes manifests = foldl step Map.empty manifests
  where
    step acc m = foldl (\a e -> maybe a (\h -> Map.insert e.path h a) (pick e.hashes)) acc (fileEntries m.entries)
    pick hs = case V.find (\mh -> mh.hash.algo == preferredAlgo) hs of
      Just mh -> Just mh.hash
      Nothing -> fmap (\mh -> mh.hash) (hs V.!? 0)

-- | The 'Int' is the chain's `sequencenr`.
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
