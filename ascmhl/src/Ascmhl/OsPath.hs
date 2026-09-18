module Ascmhl.OsPath
  ( toText
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import System.OsPath (OsPath, decodeUtf)

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

-- |
-- >>> toText (unsafeEncodeUtf "a.mxf")
-- "a.mxf"
toText :: OsPath -> Text
toText p = case decodeUtf p of
  Nothing -> T.show p
  Just s -> T.pack s
