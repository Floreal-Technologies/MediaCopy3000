module MediaCopy.Interface.Wording
  ( count
  , humanBytes
  , humanRate
  , humanEta
  , resultText
  , jobFinishedToast
  , jobFailedToast
  , savePlanTitle
  , saveReportTitle
  , noHistoryText
  , quietText
  , quietLine
  , doingText
  , fileStatusText
  , KindUi (..)
  , kindUi
  , runningVerbText
  , jobKindText
  , writeModeText
  , processKindText
  , targetStateText
  , findingText
  , pluginFaultText
  , pluginFindingTexts
  , pluginToastText
  ) where

import Ascmhl.Path (pathText)
import Ascmhl.Types
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.English qualified as English
import MediaCopy.Interface.Translation.French qualified as French
import MediaCopy.Interface.Translation.Messages

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)

count :: (Show a) => a -> Text
count n = T.show n

-- |
-- >>> humanBytes (embeddedWording English) 0
-- "0 B"
-- >>> humanBytes (embeddedWording English) 999
-- "999 B"
-- >>> humanBytes (embeddedWording English) 1_000
-- "1 KB"
-- >>> humanBytes (embeddedWording English) 12_288
-- "12 KB"
-- >>> humanBytes (embeddedWording English) 999_499
-- "999 KB"
-- >>> humanBytes (embeddedWording English) 999_500
-- "1.0 MB"
-- >>> humanBytes (embeddedWording English) 95_600_000_000
-- "95.6 GB"
-- >>> humanBytes (embeddedWording English) 2_500_000_000_000
-- "2.5 TB"
humanBytes :: Wording -> Int64 -> Text
humanBytes wording n
  | n < 1_000 = count n <> " B"
  | n < 999_500 = count (round (bytes / 1e3) :: Int64) <> " KB"
  | n < 999_950_000 = decimal wording (bytes / 1e6) <> " MB"
  | n < 999_950_000_000 = decimal wording (bytes / 1e9) <> " GB"
  | otherwise = decimal wording (bytes / 1e12) <> " TB"
  where
    bytes = fromIntegral n :: Double

-- |
-- >>> humanRate (embeddedWording English) 1.1e9
-- "1.1 GB/s"
-- >>> humanRate (embeddedWording English) 0
-- "0 B/s"
-- >>> humanRate (embeddedWording English) (0 / 0)
-- "0 B/s"
humanRate :: Wording -> Double -> Text
humanRate wording bytesPerSecond
  | not (isNormalPositive bytesPerSecond) = "0 B/s"
  | otherwise = humanBytes wording (round bytesPerSecond) <> "/s"

-- |
-- >>> humanEta (embeddedWording English) 34
-- "34 s"
-- >>> humanEta (embeddedWording English) 125
-- "2 min 05 s"
-- >>> humanEta (embeddedWording English) 3_720
-- "1 h 02 min"
-- >>> humanEta (embeddedWording English) (-1)
-- "0 s"
humanEta :: Wording -> Double -> Text
humanEta wording seconds
  | total < 60 = getTranslation' wording durationSeconds [("seconds", str (count total))]
  | total < 3_600 = getTranslation' wording durationMinutes [("minutes", str (count (total `div` 60))), ("seconds", str (pad2 (total `mod` 60)))]
  | otherwise = getTranslation' wording durationHours [("hours", str (count (total `div` 3_600))), ("minutes", str (pad2 (total `mod` 3_600 `div` 60)))]
  where
    total
      | isNormalPositive seconds = round seconds :: Int
      | otherwise = 0

isNormalPositive :: Double -> Bool
isNormalPositive x = not (isNaN x) && not (isInfinite x) && x > 0

quietFor :: NominalDiffTime
quietFor = 2

quietText :: Wording -> UTCTime -> JobState -> Maybe Text
quietText wording now state
  | Just doing <- state.doing
  , unfinished doing
  , quiet >= quietFor =
      Just (quietLine wording doing quiet)
  | otherwise = Nothing
  where
    quiet = diffUTCTime now state.lastMovedAt
    unfinished = \case
      OnFile status -> not (isDone status)
      WritingManifest -> True

quietLine :: Wording -> Doing -> NominalDiffTime -> Text
quietLine wording doing quiet =
  getTranslation'
    wording
    quietDoing
    [ ("doing", str (doingText wording doing))
    , ("elapsed", str (humanEta wording (realToFrac quiet)))
    ]

doingText :: Wording -> Doing -> Text
doingText wording = \case
  OnFile status -> fileStatusText wording status
  WritingManifest -> getTranslation' wording doingWritingManifest []

-- |
-- >>> fileStatusText (embeddedWording English) Flushing
-- "Saving to disk"
-- >>> fileStatusText (embeddedWording English) (Done (IoError "disk full"))
-- "I/O error: disk full"
fileStatusText :: Wording -> FileStatus -> Text
fileStatusText wording = \case
  Pending -> plain fileStatusPending
  Hashing -> plain fileStatusHashing
  Copying -> plain fileStatusCopying
  Flushing -> plain fileStatusFlushing
  Publishing -> plain fileStatusPublishing
  Verifying -> plain fileStatusVerifying
  Done Ok -> plain fileStatusVerified
  Done (HashMismatch _) -> plain fileStatusHashMismatch
  Done Missing -> plain fileStatusMissing
  Done New -> plain fileStatusNew
  Done (IoError message) -> getTranslation' wording fileStatusIoError [("message", str message)]
  Done (Replaced _) -> plain fileStatusReplaced
  where
    plain reference = getTranslation' wording reference []

decimal :: Wording -> Double -> Text
decimal wording = case wording.language of
  English -> English.decimal
  French -> French.decimal

pad2 :: Int -> Text
pad2 n = T.justifyRight 2 '0' (count n)

-- |
-- >>> resultText (embeddedWording English) SealKind AllOk
-- "finished, all files sealed"
-- >>> resultText (embeddedWording English) OffloadKind (WithFailures 1)
-- "finished with 1 failure"
resultText :: Wording -> JobKind -> JobResult -> Text
resultText wording kind = \case
  AllOk -> case kind of
    (OffloadKind; VerifyKind) -> getTranslation' wording resultAllVerified []
    SealKind -> getTranslation' wording resultAllSealed []
  WithFailures n -> getTranslation' wording resultWithFailures [("count", int n)]

-- >>> jobFinishedToast (embeddedWording English) "A001" OffloadKind AllOk
-- "A001: finished, all files verified"
jobFinishedToast :: Wording -> Text -> JobKind -> JobResult -> Text
jobFinishedToast wording label kind result =
  getTranslation' wording toastJobFinished [("label", str label), ("result", str (resultText wording kind result))]

jobFailedToast :: Wording -> Text -> Text -> Text
jobFailedToast wording label message = getTranslation' wording toastJobFailed [("label", str label), ("message", str message)]

savePlanTitle :: Wording -> Text
savePlanTitle wording = getTranslation' wording dialogSavePlan []

saveReportTitle :: Wording -> Text
saveReportTitle wording = getTranslation' wording dialogSaveReport []

-- |
-- >>> noHistoryText (embeddedWording English) (unsafeEncodeUtf "card")
-- "no ASC Media Hash List history (ascmhl/) in card"
noHistoryText :: Wording -> OsPath -> Text
noHistoryText wording folder = getTranslation' wording toastNoHistory [("folder", str (pathText folder))]

data KindUi = KindUi
  { icon :: Text
  , showsProgress :: Bool
  , showsOriginals :: Bool
  }

-- >>> (kindUi VerifyKind).showsProgress
-- False
kindUi :: JobKind -> KindUi
kindUi = \case
  OffloadKind ->
    KindUi {icon = "folder-download-symbolic", showsProgress = True, showsOriginals = True}
  VerifyKind ->
    KindUi {icon = "emblem-ok-symbolic", showsProgress = False, showsOriginals = False}
  SealKind ->
    KindUi {icon = "channel-secure-symbolic", showsProgress = False, showsOriginals = False}

-- | >>> runningVerbText (embeddedWording English) OffloadKind
-- "Copying"
runningVerbText :: Wording -> JobKind -> Text
runningVerbText wording = \case
  OffloadKind -> getTranslation' wording runningCopying []
  VerifyKind -> getTranslation' wording runningVerifying []
  SealKind -> getTranslation' wording runningSealing []

jobKindText :: Wording -> JobKind -> Text
jobKindText wording = \case
  OffloadKind -> getTranslation' wording jobKindOffload []
  VerifyKind -> getTranslation' wording jobKindVerify []
  SealKind -> getTranslation' wording jobKindSeal []

writeModeText :: Wording -> WriteMode -> Text
writeModeText wording = \case
  WriteNew -> getTranslation' wording writeModeCopy []
  Overwrite -> getTranslation' wording writeModeOverwrite []
  Reuse -> getTranslation' wording writeModeReuse []

processKindText :: Wording -> ProcessKind -> Text
processKindText wording = \case
  ProcessTransfer -> getTranslation' wording processTransfer []
  ProcessInPlace -> getTranslation' wording processInPlace []
  ProcessFlatten -> getTranslation' wording processFlatten []

targetStateText :: Wording -> TargetState -> Text
targetStateText wording = \case
  Fresh -> getTranslation' wording targetEmpty []
  NotEmpty -> getTranslation' wording targetNotEmpty []
  Absent -> getTranslation' wording targetAbsent []
  Partial -> getTranslation' wording targetPartial []

-- |
-- >>> findingText (embeddedWording English) DestinationForeign
-- "destination holds files that are not on the media source"
findingText :: Wording -> FindingCode -> Text
findingText wording code = getTranslation' wording reference []
  where
    reference = case code of
      SourceMissing -> findingSourceMissing
      SourceEmpty -> findingSourceEmpty
      DestinationPartial -> findingDestinationPartial
      DestinationForeign -> findingDestinationForeign
      DestinationOtherSource -> findingDestinationOtherSource
      DestinationUnavailable -> findingDestinationUnavailable
      DestinationHistoryUnreadable -> findingDestinationHistoryUnreadable
      InsufficientSpace -> findingInsufficientSpace
      FormatUnsettled -> findingFormatUnsettled
      ChainNamesNoManifest -> findingChainNamesNoManifest
      ChainUnreadable -> findingChainUnreadable
      ManifestUnreadable -> findingManifestUnreadable
      NoSeal -> findingNoSeal
      AlreadySealed -> findingAlreadySealed

-- |
-- >>> pluginFaultText (embeddedWording English) (FieldMissing "camera")
-- "the field camera has no valid value"
-- >>> pluginFaultText (embeddedWording French) (Unavailable "timed out")
-- "extension indisponible"
pluginFaultText :: Wording -> PluginFault -> Text
pluginFaultText wording = \case
  Unavailable _ -> getTranslation' wording pluginUnavailable []
  FieldMissing field -> getTranslation' wording pluginFieldMissing [("field", str field)]
  BadOutput _ -> getTranslation' wording pluginBadOutput []

-- |
-- >>> let probe = PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> pluginFindingTexts (embeddedWording French) PluginFinding {plugin = probe, severity = Blocker, about = Faulted (Unavailable "timed out after 30 s")}
-- ("Probe : extension indisponible","timed out after 30 s")
pluginFindingTexts :: Wording -> PluginFinding -> (Text, Text)
pluginFindingTexts wording finding = (titled title, detail)
  where
    titled text = getTranslation' wording pluginFinding [("plugin", str finding.plugin.name), ("title", str text)]
    (title, detail) = case finding.about of
      Said says -> (says.title, says.detail)
      Faulted fault -> (pluginFaultText wording fault, faultReason fault)
    faultReason = \case
      Unavailable reason -> reason
      FieldMissing _ -> ""
      BadOutput reason -> reason

-- |
-- >>> let probe = PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> pluginToastText (embeddedWording English) "A001" PluginFinding {plugin = probe, severity = Warning, about = Faulted (Unavailable "exit code 4")}
-- "A001: Probe: plug-in unavailable"
pluginToastText :: Wording -> Text -> PluginFinding -> Text
pluginToastText wording label finding = getTranslation' wording toastPlugin [("label", str label), ("message", str (fst (pluginFindingTexts wording finding)))]
