module Ascmhl.Schema
  ( manifestNs
  , chainNs
  , supportedVersion
  , isSupportedVersion
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
  , ascmhldirectory
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

supportedVersion :: Text
supportedVersion = "2.0"

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
