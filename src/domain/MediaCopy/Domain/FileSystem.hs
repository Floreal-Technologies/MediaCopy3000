module MediaCopy.Domain.FileSystem
  ( Tree (..)
  , ignorePatterns
  , relPathOf
  , partSuffix
  , partPath
  , stripPart
  ) where

import Ascmhl.Path (RelPath (..), mkRelPath, pathText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath, decodeUtf, makeRelative, splitDirectories, unsafeEncodeUtf)

import MediaCopy.Domain.Job (FileSize)

data Tree = Tree
  { files :: Vector (RelPath, FileSize)
  , dirs :: Vector RelPath
  }
  deriving stock (Eq, Show)

partSuffix :: Text
partSuffix = ".mc3k-part"

-- |
-- >>> pathText (partPath (unsafeEncodeUtf "a.mxf"))
-- "a.mxf.mc3k-part"
partPath :: OsPath -> OsPath
partPath target = target <> unsafeEncodeUtf (T.unpack partSuffix)

-- |
-- >>> stripPart (RelPath "card/a.mxf.mc3k-part")
-- Just (RelPath "card/a.mxf")
-- >>> stripPart (RelPath "card/a.mxf")
-- Nothing
-- >>> stripPart (RelPath ".mc3k-part")
-- Nothing
stripPart :: RelPath -> Maybe RelPath
stripPart (RelPath t) = T.stripSuffix partSuffix t >>= mkRelPath

-- |
-- >>> ignorePatterns
-- [".DS_Store","ascmhl"]
ignorePatterns :: Vector Text
ignorePatterns = V.fromList [".DS_Store", "ascmhl"]

-- |
-- >>> relPathOf (unsafeEncodeUtf "/media/card") (unsafeEncodeUtf "/media/card/day1/a.mxf")
-- Right (RelPath "day1/a.mxf")
relPathOf :: OsPath -> OsPath -> Either Text RelPath
relPathOf root full = do
  components <- traverse decodeComponent (splitDirectories (makeRelative root full))
  case mkRelPath (T.intercalate "/" components) of
    Nothing -> Left ("file name is not a usable relative path: " <> pathText full)
    Just rel -> Right rel
  where
    decodeComponent component = case decodeUtf component of
      Nothing -> Left ("file name is not valid UTF-8: " <> pathText full)
      Just component' -> Right (T.pack component')
