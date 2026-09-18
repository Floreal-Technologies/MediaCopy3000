module MediaCopy.Domain.Finding
  ( Finding (..)
  , Code (..)
  , onlyIf
  , blockerFor
  , historyCode
  , formatCode
  ) where

import MediaCopy.Domain.HashFormat (FormatError)
import MediaCopy.Domain.HashFormat qualified as HashFormat
import MediaCopy.Domain.HistoryError (HistoryError)
import MediaCopy.Domain.HistoryError qualified as HistoryError
import MediaCopy.Domain.Severity (Severity)
import MediaCopy.Domain.Severity qualified as Severity

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

data Code
  = SourceMissing
  | SourceEmpty
  | DestinationPartial
  | DestinationForeign
  | DestinationOtherSource
  | DestinationUnavailable
  | DestinationHistoryUnreadable
  | InsufficientSpace
  | FormatUnsettled
  | ChainNamesNoManifest
  | ChainUnreadable
  | ManifestUnreadable
  | NoSeal
  | AlreadySealed
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display Code where
  displayBuilder = \case
    SourceMissing -> "source not found"
    SourceEmpty -> "source is empty"
    DestinationPartial -> "destination holds a partial copy"
    DestinationForeign -> "destination holds files that are not on the media source"
    DestinationOtherSource -> "destination was copied from another media source"
    DestinationUnavailable -> "destination cannot be read"
    DestinationHistoryUnreadable -> "destination history cannot be read"
    InsufficientSpace -> "not enough space"
    FormatUnsettled -> "hash format cannot be settled"
    ChainNamesNoManifest -> "chain names no manifest"
    ChainUnreadable -> "chain cannot be read"
    ManifestUnreadable -> "a manifest the chain names cannot be read"
    NoSeal -> "folder has no history"
    AlreadySealed -> "folder is already sealed"

data Finding = Finding
  { severity :: Severity
  , code :: Code
  , detail :: Text
  }
  deriving stock (Eq, Ord, Show)

onlyIf :: Bool -> Finding -> List Finding
onlyIf holds finding
  | holds = [finding]
  | otherwise = []

blockerFor :: (Display e) => (e -> Code) -> Either e a -> List Finding
blockerFor codeOf = \case
  Left e -> [Finding {severity = Severity.Blocker, code = codeOf e, detail = display e}]
  Right _ -> []

-- | >>> historyCode (HistoryError.ChainEmpty (unsafeEncodeUtf "ascmhl_chain.xml"))
-- ChainNamesNoManifest
-- >>> historyCode (HistoryError.ChainNotParsed (unsafeEncodeUtf "ascmhl_chain.xml") "not well formed")
-- ChainUnreadable
-- >>> historyCode (HistoryError.ManifestNotFound (unsafeEncodeUtf "0001_a.mhl"))
-- ManifestUnreadable
historyCode :: HistoryError -> Code
historyCode = \case
  HistoryError.ChainNotParsed _ _ -> ChainUnreadable
  HistoryError.ChainEmpty _ -> ChainNamesNoManifest
  (HistoryError.ManifestNotFound _; HistoryError.ManifestNotParsed _ _; HistoryError.HistoryUnreadable _ _) -> ManifestUnreadable

formatCode :: FormatError -> Code
formatCode = \case
  HashFormat.MixedFormats _ -> FormatUnsettled
