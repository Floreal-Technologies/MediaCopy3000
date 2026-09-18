-- | The ASC MHL v2.0 vocabulary: every element and attribute name the format uses, and the two
-- namespaces it uses them in.
--
-- The reader and the writer name a thing from here and nowhere else, so a spelling cannot drift.
-- Import it qualified.
module Ascmhl.Schema
  ( -- * Namespaces
    manifestNs
  , chainNs
  , supportedVersion
  , isSupportedVersion

    -- * Manifest elements
  , hashlist
  , creatorinfo
  , creationdate
  , hostname
  , tool
  , processinfo
  , process
  , roothash
  , ignore
  , ignorePattern
  , hashes
  , hash
  , directoryhash
  , path
  , content
  , structure
  , hashFormatOrder

    -- * Chain elements
  , ascmhldirectory

    -- * Attributes
  , version
  , size
  , lastmodificationdate
  , hashdate
  , action
  , sequencenr
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Ascmhl.Hash (HashAlgo (..))

manifestNs :: Text
manifestNs = "urn:ASC:MHL:v2.0"

chainNs :: Text
chainNs = "urn:ASC:MHL:DIRECTORY:v2.0"

-- | What a manifest this project writes puts in its @version@ attribute.
supportedVersion :: Text
supportedVersion = "2.0"

-- | This project reads the whole 2.x line and refuses everything else, because a later
-- major revision can spell the same element differently.
isSupportedVersion :: Text -> Bool
isSupportedVersion v = "2." `T.isPrefixOf` v

hashlist :: Text
hashlist = "hashlist"

creatorinfo :: Text
creatorinfo = "creatorinfo"

creationdate :: Text
creationdate = "creationdate"

hostname :: Text
hostname = "hostname"

tool :: Text
tool = "tool"

processinfo :: Text
processinfo = "processinfo"

process :: Text
process = "process"

roothash :: Text
roothash = "roothash"

ignore :: Text
ignore = "ignore"

-- | The child of @\<ignore\>@. It carries its parent's name because @pattern@ is a keyword under
-- @PatternSynonyms@.
ignorePattern :: Text
ignorePattern = "pattern"

hashes :: Text
hashes = "hashes"

hash :: Text
hash = "hash"

directoryhash :: Text
directoryhash = "directoryhash"

path :: Text
path = "path"

content :: Text
content = "content"

structure :: Text
structure = "structure"

-- | The order @HashType@ declares its hash formats in. It is a sequence, so a manifest that writes
-- them in another order is not schema-valid. The four formats this project implements come from
-- 'Ascmhl.Hash'; the two it only carries through a rewrite are literals.
hashFormatOrder :: Vector Text
hashFormatOrder = V.fromList [display C4, display MD5, display SHA1, "xxh128", "xxh3", display XXH64]

ascmhldirectory :: Text
ascmhldirectory = "ascmhldirectory"

version :: Text
version = "version"

size :: Text
size = "size"

lastmodificationdate :: Text
lastmodificationdate = "lastmodificationdate"

hashdate :: Text
hashdate = "hashdate"

action :: Text
action = "action"

sequencenr :: Text
sequencenr = "sequencenr"
