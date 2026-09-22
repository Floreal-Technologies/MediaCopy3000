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
  = ChainNotParsed OsPath Text
  | ChainEmpty OsPath
  | ManifestNotFound OsPath
  | ManifestNotParsed OsPath Text
  | HistoryUnreadable OsPath Text
  deriving stock (Eq, Show)

-- |
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

-- |
-- >>> readOr (0 :: Int) (Right (Just 3))
-- 3
-- >>> readOr (0 :: Int) (Right Nothing)
-- 0
-- >>> readOr (0 :: Int) (Left (ChainEmpty (unsafeEncodeUtf "ascmhl_chain.xml")))
-- 0
readOr :: a -> Either HistoryError (Maybe a) -> a
readOr empty = either (const empty) (fromMaybe empty)
