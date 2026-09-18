-- | Why this project cannot read a folder's ASC MHL history.
module MediaCopy.Domain.History
  ( HistoryError (..)
  , readOr
  ) where

import Ascmhl.Path (pathText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import System.OsPath (OsPath)

-- $setup
-- >>> import Data.Text.Display (display)
-- >>> import System.OsPath (unsafeEncodeUtf)

data HistoryError
  = -- | The chain file is there and will not parse.
    ChainNotParsed OsPath Text
  | -- | The chain file is there and names nothing, so an earlier seal is lost.
    ChainEmpty OsPath
  | -- | The chain names a manifest that is not on the disk.
    ManifestNotFound OsPath
  | -- | The chain names a manifest that is there and will not parse.
    ManifestNotParsed OsPath Text
  | -- | The folder is there and a file of its history could not be read.
    HistoryUnreadable OsPath Text
  deriving stock (Eq, Show)

-- | The sentence the operator reads.
--
-- >>> display (ChainEmpty (unsafeEncodeUtf "ascmhl_chain.xml"))
-- "ASC MHL chain names no manifest: ascmhl_chain.xml"
-- >>> display (ManifestNotFound (unsafeEncodeUtf "0001_a.mhl"))
-- "0001_a.mhl: missing"
instance Display HistoryError where
  displayBuilder = \case
    ChainNotParsed path detail -> displayBuilder (pathText path <> ": " <> detail)
    ChainEmpty path -> displayBuilder ("ASC MHL chain names no manifest: " <> pathText path)
    ManifestNotFound path -> displayBuilder (pathText path <> ": missing")
    ManifestNotParsed path detail -> displayBuilder (pathText path <> ": " <> detail)
    HistoryUnreadable path detail -> displayBuilder (pathText path <> ": " <> detail)

-- | A history that could not be read, and a folder that holds none, read the same way here: the
-- caller says what nothing looks like.
--
-- >>> readOr (0 :: Int) (Right (Just 3))
-- 3
-- >>> readOr (0 :: Int) (Right Nothing)
-- 0
-- >>> readOr (0 :: Int) (Left (ChainEmpty (unsafeEncodeUtf "ascmhl_chain.xml")))
-- 0
readOr :: a -> Either HistoryError (Maybe a) -> a
readOr empty = either (const empty) (fromMaybe empty)
