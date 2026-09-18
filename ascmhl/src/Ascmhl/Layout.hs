{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Ascmhl.Layout
  ( dir
  , inHistory
  , chainFile
  , manifestFile
  , manifestNames
  ) where

import Data.Function ((&))
import Data.List (sort)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath, (</>))
import splice System.OsPath (osp)
import System.OsString (isSuffixOf)

import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath

-- $setup
-- >>> import Ascmhl.OsPath qualified as OsPath
-- >>> import System.OsPath (unsafeEncodeUtf)

dir :: OsPath -> OsPath
dir folder = folder </> [osp|ascmhl|]

chainFile :: OsPath -> OsPath
chainFile folder = inHistory folder [osp|ascmhl_chain.xml|]

inHistory :: OsPath -> OsPath -> OsPath
inHistory folder name = dir folder </> name

manifestFile :: OsPath -> RelPath -> OsPath
manifestFile folder path = RelPath.toOsPath (dir folder) path

-- |
-- >>> map OsPath.toText (V.toList (manifestNames (V.fromList (map unsafeEncodeUtf ["0002_b.mhl", "ascmhl_chain.xml", "0001_a.mhl"]))))
-- ["0001_a.mhl","0002_b.mhl"]
manifestNames :: Vector OsPath -> Vector OsPath
manifestNames names = names & V.filter (\name -> [osp|.mhl|] `isSuffixOf` name) & V.toList & sort & V.fromList
