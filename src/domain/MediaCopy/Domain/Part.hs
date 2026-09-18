module MediaCopy.Domain.Part
  ( suffix
  , path
  , strip
  ) where

import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Text qualified as T
import System.OsPath (unsafeEncodeUtf)

-- $setup
-- >>> import Ascmhl.OsPath qualified as OsPath

suffix :: Text
suffix = ".mc3k-part"

-- |
-- >>> OsPath.toText (path (unsafeEncodeUtf "a.mxf"))
-- "a.mxf.mc3k-part"
path :: OsPath -> OsPath
path target = target <> unsafeEncodeUtf (T.unpack suffix)

strip :: RelPath -> Maybe RelPath
strip (RelPath.RelPath t) = T.stripSuffix suffix t >>= RelPath.parse
