module MediaCopy.Domain.HistoryError
  ( HistoryError (..)
  , readOr
  ) where

import Ascmhl.OsPath qualified as OsPath

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
    ChainNotParsed path detail -> displayBuilder (OsPath.toText path <> ": " <> detail)
    ChainEmpty path -> displayBuilder ("ASC MHL chain names no manifest: " <> OsPath.toText path)
    ManifestNotFound path -> displayBuilder (OsPath.toText path <> ": missing")
    ManifestNotParsed path detail -> displayBuilder (OsPath.toText path <> ": " <> detail)
    HistoryUnreadable path detail -> displayBuilder (OsPath.toText path <> ": " <> detail)

readOr :: a -> Either HistoryError (Maybe a) -> a
readOr empty = either (const empty) (fromMaybe empty)
