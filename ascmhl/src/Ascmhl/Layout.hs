{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Where an ASC MHL history's files sit on a disk, and what their names mean. Pure: nothing here
-- touches a filesystem, so this module can give a path without a read of one.
module Ascmhl.Layout
  ( ascmhlDir
  , inHistory
  , chainPath
  , manifestPathOf
  , mhlFileNames
  , sequenceOf
  ) where

import Data.Function ((&))
import Data.List (sort)
import Data.Text.Display (display)
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath, (</>))
import splice System.OsPath (osp)
import System.OsString (isSuffixOf)

import Ascmhl.Path (RelPath, relToOsPath)

-- $setup
-- >>> import Ascmhl.Path (RelPath (..), pathText)
-- >>> import System.OsPath (unsafeEncodeUtf)

-- | Where a history sits, and what its files are called.
ascmhlDir :: OsPath -> OsPath
ascmhlDir folder = folder </> [osp|ascmhl|]

chainPath :: OsPath -> OsPath
chainPath folder = inHistory folder [osp|ascmhl_chain.xml|]

-- | A file that sits inside a folder's history.
inHistory :: OsPath -> OsPath -> OsPath
inHistory folder name = ascmhlDir folder </> name

manifestPathOf :: OsPath -> RelPath -> OsPath
manifestPathOf folder path = relToOsPath (ascmhlDir folder) path

-- | The .mhl manifests in an ascmhl listing, in generation order.
--
-- >>> map pathText (V.toList (mhlFileNames (V.fromList (map unsafeEncodeUtf ["0002_b.mhl", "ascmhl_chain.xml", "0001_a.mhl"]))))
-- ["0001_a.mhl","0002_b.mhl"]
mhlFileNames :: Vector OsPath -> Vector OsPath
mhlFileNames names = names & V.filter (\name -> [osp|.mhl|] `isSuffixOf` name) & V.toList & sort & V.fromList

-- | `0003_card_….mhl` is generation 3. A name with no leading number keeps its listing position.
--
-- >>> sequenceOf 7 (RelPath "0003_card_a.mhl")
-- 3
-- >>> sequenceOf 7 (RelPath "legacy.mhl")
-- 7
sequenceOf :: Int -> RelPath -> Int
sequenceOf index p = case TR.decimal (display p) of
  Right (parsed, _rest) -> parsed
  Left _notANumber -> index
