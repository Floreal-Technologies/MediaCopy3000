module MediaCopy.Interface.Wording
  ( count
  , humanBytes
  , humanRate
  , humanEta
  , humanPercent
  , resultText
  , jobFinishedToast
  , jobFailedToast
  , savePlanTitle
  , saveReportTitle
  , noHistoryText
  , quietText
  , quietLine
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

import Ascmhl.OsPath qualified as OsPath
import Ascmhl.Process (Process)
import Ascmhl.Process qualified as Process
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Language.Fluent (toValue)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobResult (JobResult)
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.English qualified as English
import MediaCopy.Interface.Translation.French qualified as French
import MediaCopy.Interface.Translation.Messages

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)
-- >>> import MediaCopy.Domain.Severity qualified as Severity

count :: (Show a) => a -> Text
count n = T.show n

-- |
-- >>> humanBytes (embeddedWording English) 95_600_000_000
-- "95.6 GB"
-- >>> humanBytes (embeddedWording French) 12_288
-- "12\160ko"
-- >>> humanBytes (embeddedWording French) 95_600_000_000
-- "95,6\160Go"
humanBytes :: Wording -> Int64 -> Text
humanBytes wording n
  | n < 1_000 = sized sizeBytes (count n)
  | n < 999_500 = sized sizeKilobytes (count (round (bytes / 1e3) :: Int64))
  | n < 999_950_000 = sized sizeMegabytes (decimal wording (bytes / 1e6))
  | n < 999_950_000_000 = sized sizeGigabytes (decimal wording (bytes / 1e9))
  | otherwise = sized sizeTerabytes (decimal wording (bytes / 1e12))
  where
    bytes = fromIntegral n :: Double
    sized reference size = getTranslation wording reference [("size", str size)]

-- |
-- >>> humanRate (embeddedWording French) 0
-- "0\160o/s"
-- >>> humanRate (embeddedWording French) 1.1e9
-- "1,1\160Go/s"
humanRate :: Wording -> Double -> Text
humanRate wording bytesPerSecond = getTranslation wording sizePerSecond [("size", str (humanBytes wording bytes))]
  where
    bytes
      | isNormalPositive bytesPerSecond = round bytesPerSecond
      | otherwise = 0

-- |
-- >>> humanPercent (embeddedWording English) 42
-- "42 %"
-- >>> humanPercent (embeddedWording French) 42
-- "42\160%"
humanPercent :: Wording -> Int -> Text
humanPercent wording percent = getTranslation wording progressPercent [("percent", str (count percent))]

-- |
-- >>> humanEta (embeddedWording English) 125
-- "2 min 05 s"
-- >>> humanEta (embeddedWording French) 185
-- "3\160min 5\160s"
-- >>> humanEta (embeddedWording French) 3_900
-- "1\160h 5\160min"
humanEta :: Wording -> Double -> Text
humanEta wording seconds
  | total < 60 = getTranslation wording durationSeconds [("seconds", str (count total))]
  | total < 3_600 = getTranslation wording durationMinutes [("minutes", str (count (total `div` 60))), ("seconds", str (subunit wording (total `mod` 60)))]
  | otherwise = getTranslation wording durationHours [("hours", str (count (total `div` 3_600))), ("minutes", str (subunit wording (total `mod` 3_600 `div` 60)))]
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
      JobState.OnFile status -> not (FileStatus.isDone status)
      JobState.WritingManifest -> True

quietLine :: Wording -> JobState.Doing -> NominalDiffTime -> Text
quietLine wording doing quiet =
  getTranslation
    wording
    quietDoing
    [ ("doing", str (doingText wording doing))
    , ("elapsed", str (humanEta wording (realToFrac quiet)))
    ]

doingText :: Wording -> JobState.Doing -> Text
doingText wording = \case
  JobState.OnFile status -> fileStatusText wording status
  JobState.WritingManifest -> getTranslation wording doingWritingManifest []

-- |
-- >>> fileStatusText (embeddedWording English) FileStatus.Flushing
-- "Saving to disk"
-- >>> fileStatusText (embeddedWording English) (FileStatus.Done (FileOutcome.IoError "disk full"))
-- "I/O error: disk full"
fileStatusText :: Wording -> FileStatus -> Text
fileStatusText wording = \case
  FileStatus.Pending -> plain fileStatusPending
  FileStatus.Hashing -> plain fileStatusHashing
  FileStatus.Copying -> plain fileStatusCopying
  FileStatus.Flushing -> plain fileStatusFlushing
  FileStatus.Publishing -> plain fileStatusPublishing
  FileStatus.Verifying -> plain fileStatusVerifying
  FileStatus.Done FileOutcome.Ok -> plain fileStatusVerified
  FileStatus.Done (FileOutcome.HashMismatch _) -> plain fileStatusHashMismatch
  FileStatus.Done FileOutcome.Missing -> plain fileStatusMissing
  FileStatus.Done FileOutcome.New -> plain fileStatusNew
  FileStatus.Done (FileOutcome.IoError message) -> getTranslation wording fileStatusIoError [("message", str message)]
  FileStatus.Done (FileOutcome.Replaced _) -> plain fileStatusReplaced
  where
    plain reference = getTranslation wording reference []

decimal :: Wording -> Double -> Text
decimal wording = case wording.language of
  English -> English.decimal
  French -> French.decimal

subunit :: Wording -> Int -> Text
subunit wording = case wording.language of
  English -> English.subunit
  French -> French.subunit

-- |
-- >>> resultText (embeddedWording English) Job.SealKind JobResult.AllOk
-- "finished, all files sealed"
-- >>> resultText (embeddedWording English) Job.OffloadKind (JobResult.WithFailures 1)
-- "finished with 1 failure"
resultText :: Wording -> Job.Kind -> JobResult -> Text
resultText wording kind = \case
  JobResult.AllOk -> case kind of
    (Job.OffloadKind; Job.VerifyKind) -> getTranslation wording resultAllVerified []
    Job.SealKind -> getTranslation wording resultAllSealed []
  JobResult.WithFailures n -> getTranslation wording resultWithFailures [("count", toValue n)]

-- >>> jobFinishedToast (embeddedWording English) "A001" Job.OffloadKind JobResult.AllOk
-- "A001: finished, all files verified"
jobFinishedToast :: Wording -> Text -> Job.Kind -> JobResult -> Text
jobFinishedToast wording label kind result =
  getTranslation wording toastJobFinished [("label", str label), ("result", str (resultText wording kind result))]

jobFailedToast :: Wording -> Text -> Text -> Text
jobFailedToast wording label message = getTranslation wording toastJobFailed [("label", str label), ("message", str message)]

savePlanTitle :: Wording -> Text
savePlanTitle wording = getTranslation wording dialogSavePlan []

saveReportTitle :: Wording -> Text
saveReportTitle wording = getTranslation wording dialogSaveReport []

-- |
-- >>> noHistoryText (embeddedWording English) (unsafeEncodeUtf "card")
-- "no ASC Media Hash List history (ascmhl/) in card"
noHistoryText :: Wording -> OsPath -> Text
noHistoryText wording folder = getTranslation wording toastNoHistory [("folder", str (OsPath.toText folder))]

data KindUi = KindUi
  { icon :: Text
  , showsProgress :: Bool
  , showsOriginals :: Bool
  }

-- >>> (kindUi Job.VerifyKind).showsProgress
-- False
kindUi :: Job.Kind -> KindUi
kindUi = \case
  Job.OffloadKind ->
    KindUi {icon = "folder-download-symbolic", showsProgress = True, showsOriginals = True}
  Job.VerifyKind ->
    KindUi {icon = "emblem-ok-symbolic", showsProgress = False, showsOriginals = False}
  Job.SealKind ->
    KindUi {icon = "channel-secure-symbolic", showsProgress = False, showsOriginals = False}

-- | >>> runningVerbText (embeddedWording English) Job.OffloadKind
-- "Copying"
runningVerbText :: Wording -> Job.Kind -> Text
runningVerbText wording = \case
  Job.OffloadKind -> getTranslation wording runningCopying []
  Job.VerifyKind -> getTranslation wording runningVerifying []
  Job.SealKind -> getTranslation wording runningSealing []

jobKindText :: Wording -> Job.Kind -> Text
jobKindText wording = \case
  Job.OffloadKind -> getTranslation wording jobKindOffload []
  Job.VerifyKind -> getTranslation wording jobKindVerify []
  Job.SealKind -> getTranslation wording jobKindSeal []

writeModeText :: Wording -> PlannedWrite.Mode -> Text
writeModeText wording = \case
  PlannedWrite.New -> getTranslation wording writeModeCopy []
  PlannedWrite.Overwrite -> getTranslation wording writeModeOverwrite []
  PlannedWrite.Reuse -> getTranslation wording writeModeReuse []

processKindText :: Wording -> Process -> Text
processKindText wording = \case
  Process.Transfer -> getTranslation wording processTransfer []
  Process.InPlace -> getTranslation wording processInPlace []
  Process.Flatten -> getTranslation wording processFlatten []

targetStateText :: Wording -> Target.State -> Text
targetStateText wording = \case
  Target.Fresh -> getTranslation wording targetEmpty []
  Target.NotEmpty -> getTranslation wording targetNotEmpty []
  Target.Absent -> getTranslation wording targetAbsent []
  Target.Partial -> getTranslation wording targetPartial []

-- |
-- >>> findingText (embeddedWording English) Finding.DestinationForeign
-- "destination holds files that are not on the media source"
findingText :: Wording -> Finding.Code -> Text
findingText wording code = getTranslation wording reference []
  where
    reference = case code of
      Finding.SourceMissing -> findingSourceMissing
      Finding.SourceEmpty -> findingSourceEmpty
      Finding.DestinationPartial -> findingDestinationPartial
      Finding.DestinationForeign -> findingDestinationForeign
      Finding.DestinationOtherSource -> findingDestinationOtherSource
      Finding.DestinationUnavailable -> findingDestinationUnavailable
      Finding.DestinationHistoryUnreadable -> findingDestinationHistoryUnreadable
      Finding.InsufficientSpace -> findingInsufficientSpace
      Finding.FormatUnsettled -> findingFormatUnsettled
      Finding.ChainNamesNoManifest -> findingChainNamesNoManifest
      Finding.ChainUnreadable -> findingChainUnreadable
      Finding.ManifestUnreadable -> findingManifestUnreadable
      Finding.NoSeal -> findingNoSeal
      Finding.AlreadySealed -> findingAlreadySealed

-- |
-- >>> pluginFaultText (embeddedWording English) (PluginFinding.FieldMissing "camera")
-- "the field camera has no valid value"
-- >>> pluginFaultText (embeddedWording French) (PluginFinding.Unavailable "timed out")
-- "extension indisponible"
pluginFaultText :: Wording -> PluginFinding.Fault -> Text
pluginFaultText wording = \case
  PluginFinding.Unavailable _ -> getTranslation wording pluginUnavailable []
  PluginFinding.FieldMissing field -> getTranslation wording pluginFieldMissing [("field", str field)]
  PluginFinding.BadOutput _ -> getTranslation wording pluginBadOutput []

-- |
-- >>> let probe = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> pluginFindingTexts (embeddedWording French) PluginFinding.PluginFinding {plugin = probe, severity = Severity.Blocker, about = PluginFinding.Faulted (PluginFinding.Unavailable "timed out after 30 s")}
-- ("Probe\160: extension indisponible","timed out after 30 s")
pluginFindingTexts :: Wording -> PluginFinding -> (Text, Text)
pluginFindingTexts wording finding = (titled title, detail)
  where
    titled text = getTranslation wording pluginFinding [("plugin", str finding.plugin.name), ("title", str text)]
    (title, detail) = case finding.about of
      PluginFinding.Said says -> (says.title, says.detail)
      PluginFinding.Faulted fault -> (pluginFaultText wording fault, faultReason fault)
    faultReason = \case
      PluginFinding.Unavailable reason -> reason
      PluginFinding.FieldMissing _ -> ""
      PluginFinding.BadOutput reason -> reason

-- |
-- >>> let probe = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> pluginToastText (embeddedWording English) "A001" PluginFinding.PluginFinding {plugin = probe, severity = Severity.Warning, about = PluginFinding.Faulted (PluginFinding.Unavailable "exit code 4")}
-- "A001: Probe: plug-in unavailable"
pluginToastText :: Wording -> Text -> PluginFinding -> Text
pluginToastText wording label finding = getTranslation wording toastPlugin [("label", str label), ("message", str (fst (pluginFindingTexts wording finding)))]
