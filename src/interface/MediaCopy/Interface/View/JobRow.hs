module MediaCopy.Interface.View.JobRow
  ( JobRowView (..)
  , jobRowView
  ) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)

import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording

data JobRowView = JobRowView
  { icon :: Text
  , label :: Text
  , phase :: Text
  , fraction :: Double
  , allOk :: Bool
  , bad :: Bool
  }
  deriving stock (Eq, Show)

jobRowView :: Wording -> UTCTime -> JobState -> JobRowView
jobRowView wording now state =
  JobRowView
    { icon = (kindUi (Job.kind state.spec.job)).icon
    , label = Job.label state.spec.job
    , phase = phaseText wording now state
    , fraction = JobState.fraction state
    , allOk = state.phase == JobState.Finished JobResult.AllOk
    , bad = case state.phase of
        (JobState.Finished (JobResult.WithFailures _); JobState.Failed _) -> True
        _ -> False
    }

phaseText :: Wording -> UTCTime -> JobState -> Text
phaseText wording now state = case state.phase of
  JobState.Queued -> "Queued"
  JobState.NeedsReview -> "Needs review"
  JobState.Running -> runningText wording now state
  JobState.Finished JobResult.AllOk -> "Finished · " <> Prose.plural "file" (Map.size state.files) <> " · all OK"
  JobState.Finished (JobResult.WithFailures failures) -> "Finished · " <> Prose.plural "failure" failures
  JobState.Failed _ -> "Failed"
  JobState.Cancelled -> "Cancelled"

runningText :: Wording -> UTCTime -> JobState -> Text
runningText wording now state
  | Just quiet <- quietText wording now state = quiet
  | (kindUi kind).showsProgress =
      verb
        <> " • "
        <> count (floor (JobState.fraction state * 100) :: Int)
        <> " % • "
        <> humanRate wording (JobState.rate state)
  | otherwise = verb <> "…"
  where
    kind = Job.kind state.spec.job
    verb = runningVerbText wording kind
