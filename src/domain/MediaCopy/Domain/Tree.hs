module MediaCopy.Domain.Tree
  ( Tree (..)
  , ignorePatterns
  , isIgnoredName
  , relPathOf
  ) where

import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Text qualified as T
import Data.Vector qualified as V
import System.OsPath (decodeUtf, makeRelative, splitDirectories)

import MediaCopy.Domain.FileSize (FileSize)

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

data Tree = Tree
  { files :: Vector (RelPath, FileSize)
  , dirs :: Vector RelPath
  }
  deriving stock (Eq, Show)

-- |
-- >>> ignorePatterns
-- [".DS_Store","ascmhl"]
ignorePatterns :: Vector Text
ignorePatterns = V.fromList [".DS_Store", "ascmhl"]

-- |
-- >>> map (isIgnoredName . unsafeEncodeUtf) [".DS_Store", "ascmhl", "A001.mxf"]
-- [True,True,False]
isIgnoredName :: OsPath -> Bool
isIgnoredName name = case decodeUtf name of
  Nothing -> False
  Just s -> T.pack s `V.elem` ignorePatterns

-- |
-- >>> relPathOf (unsafeEncodeUtf "/media/card") (unsafeEncodeUtf "/media/card/day1/a.mxf")
-- Right (RelPath "day1/a.mxf")
relPathOf :: OsPath -> OsPath -> Either Text RelPath
relPathOf root full = do
  components <- traverse decodeComponent (splitDirectories (makeRelative root full))
  case RelPath.parse (T.intercalate "/" components) of
    Nothing -> Left ("file name is not a usable relative path: " <> OsPath.toText full)
    Just rel -> Right rel
  where
    decodeComponent component = case decodeUtf component of
      Nothing -> Left ("file name is not valid UTF-8: " <> OsPath.toText full)
      Just component' -> Right (T.pack component')
