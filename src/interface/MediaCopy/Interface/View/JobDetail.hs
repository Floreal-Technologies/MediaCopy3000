module MediaCopy.Interface.View.JobDetail
  ( pluginStatusRows
  , historyRows
  , DetailView (..)
  , detailView
  , historyFor
  ) where

import Ascmhl.Path (pathText)
import Ascmhl.Types (CreatorInfo (..), Generation (..), MhlHistory (..), algosText)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Domain.Job hiding (Progress)
import MediaCopy.Domain.Plugin
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Interface.View.Row
import MediaCopy.Interface.Wording

pluginStatusRows :: Wording -> PluginState -> Vector RowView
pluginStatusRows wording st =
  inspecting <> warnings <> skipped
  where
    inspecting
      | st.inspectionsLeft > 0 = V.singleton (rowView ("Inspecting: " <> plural "file" st.inspectionsLeft <> " left") "")
      | otherwise = V.empty
    warnings = V.map (\finding -> let (title, detail) = pluginFindingTexts wording finding in (rowView title detail) {tone = Just Warn}) st.warnings
    skipped = V.fromList (map (\(ref, left) -> (rowView (ref.name <> ": " <> plural "file" left <> " not inspected") "") {tone = Just Warn}) (Map.toList st.notInspected))

historyRows :: Wording -> MhlHistory -> (Text, Vector RowView)
historyRows wording history =
  (plural "generation" (V.length history.generations), V.map (generationRow wording) history.generations)

generationRow :: Wording -> Generation -> RowView
generationRow wording generation =
  (rowView (generationTitle generation) (generationSubtitle wording generation))
    { tone = if generation.failures > 0 then Just Bad else Nothing
    }

generationTitle :: Generation -> Text
generationTitle generation =
  T.justifyRight 4 '0' (count generation.number)
    <> " · "
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" generation.creator.creationDate)

generationSubtitle :: Wording -> Generation -> Text
generationSubtitle wording generation =
  generation.creator.hostname
    <> " — "
    <> generation.creator.toolName
    <> maybe "" (" " <>) generation.creator.toolVersion
    <> " · "
    <> algosText generation.algos
    <> " · "
    <> processKindText wording generation.process
    <> failuresSuffix generation.failures

failuresSuffix :: Int -> Text
failuresSuffix failures
  | failures > 0 = " · " <> plural "failure" failures
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

detailView :: Wording -> UTCTime -> Maybe MhlHistory -> JobState -> DetailView
detailView wording now loaded state =
  let counts = countOutcomes state
  in DetailView
       { title = jobLabel state.spec.job
       , pathLine = pathLineText loaded state
       , origins = originsLineText state
       , fraction = fractionOf state
       , progressLeft = progressLeftText wording state
       , progressRight = progressRightText wording now state (rateOf state)
       , verified = count (counts.verified + counts.replaced)
       , failed = count counts.failed
       , missing = count counts.missing
       , new = count counts.new
       , algo = algoText loaded state
       , anyVerified = counts.verified + counts.replaced > 0
       , anyFailed = counts.failed > 0
       }

historyFor :: Maybe MhlHistory -> JobState -> Maybe MhlHistory
historyFor loaded state = case historyFolder state.spec.job of
  Nothing -> Nothing
  Just _ -> loaded

pathLineText :: Maybe MhlHistory -> JobState -> Text
pathLineText loaded state = case state.spec.job of
  Offload offload ->
    pathText offload.source
      <> " → "
      <> (offload.destinations & NE.toList & map pathText & T.intercalate " · ")
  (VerifyFolder _; SealMediaSource _) -> folderLine
  where
    folderLine =
      pathText (jobRoot state.spec.job) <> " · ascmhl/ chain: " <> chainText loaded

originsLineText :: JobState -> Maybe Text
originsLineText state
  | (kindUi (jobKind state.spec.job)).showsOriginals = fmap (\origin -> "Originals: " <> origin <> existingText state.spec.job) state.originsUsed
  | otherwise = Nothing

existingText :: Job -> Text
existingText = \case
  Offload oj | Just choice <- oj.existingCopy -> "\nExisting copy: " <> pastOf choice
  _ -> ""
  where
    pastOf = \case
      Resume -> "resumed"
      Replace -> "replaced"

chainText :: Maybe MhlHistory -> Text
chainText = \case
  Nothing -> "—"
  Just loaded -> plural "generation" (V.length loaded.generations)

algoText :: Maybe MhlHistory -> JobState -> Text
algoText loaded state = case state.originsAlgo of
  Just algo -> display algo
  Nothing -> algoOfLatestGeneration loaded

algoOfLatestGeneration :: Maybe MhlHistory -> Text
algoOfLatestGeneration = \case
  Nothing -> "—"
  Just loaded -> case V.unsnoc loaded.generations of
    Nothing -> "—"
    Just (_earlier, latest) -> algosText latest.algos

progressLeftText :: Wording -> JobState -> Text
progressLeftText wording state =
  verbOf wording state
    <> " "
    <> count (doneCount state)
    <> " / "
    <> plural "file" (Map.size state.files)
    <> " · "
    <> humanBytes wording state.bytesDone
    <> " of "
    <> humanBytes wording state.bytesTotal

progressRightText :: Wording -> UTCTime -> JobState -> Double -> Text
progressRightText wording now state rate = case state.phase of
  Running
    | Just quiet <- quietText wording now state -> quiet
    | rate > 0 -> humanRate wording rate <> " · ETA " <> humanEta wording (remainingSeconds state rate)
    | otherwise -> humanRate wording rate <> " · ETA —"
  Finished _ -> "done"
  _ -> ""

remainingSeconds :: JobState -> Double -> Double
remainingSeconds state rate = fromIntegral (state.bytesTotal - state.bytesDone) / rate

verbOf :: Wording -> JobState -> Text
verbOf wording state = case state.phase of
  Queued -> "Queued"
  NeedsReview -> "Needs review"
  Finished _ -> "Finished"
  Failed _ -> "Failed"
  Cancelled -> "Cancelled"
  _ -> runningVerbText wording (jobKind state.spec.job)

doneCount :: JobState -> Int
doneCount state = state.files & Map.elems & filter (\entry -> isDone entry.status) & length
