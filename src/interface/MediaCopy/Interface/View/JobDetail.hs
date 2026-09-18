module MediaCopy.Interface.View.JobDetail
  ( pluginStatusRows
  , historyRows
  , DetailView (..)
  , detailView
  , historyFor
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Creator qualified as Creator
import Ascmhl.History (History)
import Ascmhl.History qualified as History
import Ascmhl.OsPath qualified as OsPath
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as V

import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginState (PluginState)
import MediaCopy.Domain.PluginState qualified as PluginState
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Interface.View.Row
import MediaCopy.Interface.Wording

pluginStatusRows :: Wording -> PluginState -> Vector RowView
pluginStatusRows wording st =
  inspecting <> warnings <> skipped
  where
    inspecting
      | st.inspectionsLeft > 0 = V.singleton (rowView ("Inspecting: " <> Prose.plural "file" st.inspectionsLeft <> " left") "")
      | otherwise = V.empty
    warnings = V.map (\finding -> let (title, detail) = pluginFindingTexts wording finding in (rowView title detail) {tone = Just Warn}) st.warnings
    skipped = V.fromList (map (\(ref, left) -> (rowView (ref.name <> ": " <> Prose.plural "file" left <> " not inspected") "") {tone = Just Warn}) (Map.toList st.notInspected))

historyRows :: Wording -> History -> (Text, Vector RowView)
historyRows wording history =
  (Prose.plural "generation" (V.length history.generations), V.map (generationRow wording) history.generations)

generationRow :: Wording -> History.Generation -> RowView
generationRow wording generation =
  (rowView (generationTitle generation) (generationSubtitle wording generation))
    { tone = if generation.failures > 0 then Just Bad else Nothing
    }

generationTitle :: History.Generation -> Text
generationTitle generation =
  T.justifyRight 4 '0' (count generation.number)
    <> " · "
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" generation.creator.creationDate)

generationSubtitle :: Wording -> History.Generation -> Text
generationSubtitle wording generation =
  generation.creator.hostname
    <> " – "
    <> generation.creator.toolName
    <> maybe "" (" " <>) generation.creator.toolVersion
    <> " · "
    <> Algo.renderSet generation.algos
    <> " · "
    <> processKindText wording generation.process
    <> failuresSuffix generation.failures

failuresSuffix :: Int -> Text
failuresSuffix failures
  | failures > 0 = " · " <> Prose.plural "failure" failures
  | otherwise = ""

data DetailView = DetailView
  { title :: Text
  , pathLine :: Text
  , origins :: Maybe Text
  , fraction :: Double
  , progressLeft :: Text
  , progressRight :: Text
  , verified :: Text
  , failed :: Text
  , missing :: Text
  , new :: Text
  , algo :: Text
  , anyVerified :: Bool
  , anyFailed :: Bool
  }
  deriving stock (Eq, Show)

detailView :: Wording -> UTCTime -> Maybe History -> JobState -> DetailView
detailView wording now loaded state =
  let counts = JobState.counts state
  in DetailView
       { title = Job.label state.spec.job
       , pathLine = pathLineText loaded state
       , origins = originsLineText state
       , fraction = JobState.fraction state
       , progressLeft = progressLeftText wording state
       , progressRight = progressRightText wording now state
       , verified = count (counts.verified + counts.replaced)
       , failed = count counts.failed
       , missing = count counts.missing
       , new = count counts.new
       , algo = algoText loaded state
       , anyVerified = counts.verified + counts.replaced > 0
       , anyFailed = counts.failed > 0
       }

historyFor :: Maybe History -> JobState -> Maybe History
historyFor loaded state = case Job.historyFolder state.spec.job of
  Nothing -> Nothing
  Just _ -> loaded

pathLineText :: Maybe History -> JobState -> Text
pathLineText loaded state = case state.spec.job of
  Job.Offload offload ->
    OsPath.toText offload.source
      <> " → "
      <> (offload.destinations & NE.toList & map OsPath.toText & T.intercalate " · ")
  (Job.VerifyFolder _; Job.SealMediaSource _) -> folderLine
  where
    folderLine =
      OsPath.toText (Job.root state.spec.job) <> " · ascmhl/ chain: " <> chainText loaded

originsLineText :: JobState -> Maybe Text
originsLineText state
  | (kindUi (Job.kind state.spec.job)).showsOriginals = fmap (\origin -> "Originals: " <> origin <> existingText state.spec.job) state.originsUsed
  | otherwise = Nothing

existingText :: Job -> Text
existingText = \case
  Job.Offload oj | Just choice <- oj.existingCopy -> "\nExisting copy: " <> pastOf choice
  _ -> ""
  where
    pastOf = \case
      Offload.Resume -> "resumed"
      Offload.Replace -> "replaced"

chainText :: Maybe History -> Text
chainText = \case
  Nothing -> "—"
  Just loaded -> Prose.plural "generation" (V.length loaded.generations)

algoText :: Maybe History -> JobState -> Text
algoText loaded state = case state.originsAlgo of
  Just algo -> display algo
  Nothing -> algoOfLatestGeneration loaded

algoOfLatestGeneration :: Maybe History -> Text
algoOfLatestGeneration = \case
  Nothing -> "—"
  Just loaded -> case V.unsnoc loaded.generations of
    Nothing -> "—"
    Just (_earlier, latest) -> Algo.renderSet latest.algos

progressLeftText :: Wording -> JobState -> Text
progressLeftText wording state =
  verbOf wording state
    <> " "
    <> count (doneCount state)
    <> " / "
    <> Prose.plural "file" (Map.size state.files)
    <> " · "
    <> humanBytes wording state.bytesDone
    <> " of "
    <> humanBytes wording state.bytesTotal

progressRightText :: Wording -> UTCTime -> JobState -> Text
progressRightText wording now state = case state.phase of
  JobState.Running
    | Just quiet <- quietText wording now state -> quiet
    | rate > 0 -> humanRate wording rate <> " · ETA " <> humanEta wording (remainingSeconds state rate)
    | otherwise -> humanRate wording rate <> " · ETA —"
  JobState.Finished _ -> "done"
  _ -> ""
  where
    rate = JobState.rate state

remainingSeconds :: JobState -> Double -> Double
remainingSeconds state rate = fromIntegral (state.bytesTotal - state.bytesDone) / rate

verbOf :: Wording -> JobState -> Text
verbOf wording state = case state.phase of
  JobState.Queued -> "Queued"
  JobState.NeedsReview -> "Needs review"
  JobState.Finished _ -> "Finished"
  JobState.Failed _ -> "Failed"
  JobState.Cancelled -> "Cancelled"
  _ -> runningVerbText wording (Job.kind state.spec.job)

doneCount :: JobState -> Int
doneCount state = state.files & Map.elems & filter (\entry -> FileStatus.isDone entry.status) & length
