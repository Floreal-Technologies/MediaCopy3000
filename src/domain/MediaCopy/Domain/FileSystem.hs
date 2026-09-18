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

-- | One walk of one folder. Both lists carry the ignore rules and are sorted by 'RelPath'.
data Tree = Tree
  { files :: Vector (RelPath, FileSize)
  , dirs :: Vector RelPath
  -- ^ Every directory below the root. The root has no row of its own.
  }
  deriving stock (Eq, Show)

partSuffix :: Text
partSuffix = ".mc3k-part"

-- | The name a file carries until it is complete.
--
-- >>> pathText (partPath (unsafeEncodeUtf "a.mxf"))
-- "a.mxf.mc3k-part"
partPath :: OsPath -> OsPath
partPath target = target <> unsafeEncodeUtf (T.unpack partSuffix)

-- | The path a part file will publish to. 'Nothing' for a name that is not a part file.
-- The suffix on its own strips to the empty path, which is no path at all.
--
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

-- | The one rule for how a path's components become a 'RelPath', shared by every adapter. The
-- result is joined by @\/@ on every host, whatever separator the two arguments carried.
--
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
