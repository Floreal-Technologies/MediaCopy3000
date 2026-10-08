module MediaCopy.Interface.View.JobRow
  ( JobRowView (..)
  , jobRowView
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)

import MediaCopy.Domain.Job
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
    { icon = (kindUi (jobKind state.spec.job)).icon
    , label = jobLabel state.spec.job
    , phase = phaseText wording now state
    , fraction = fractionOf state
    , allOk = state.phase == Finished AllOk
    , bad = case state.phase of
        (Finished (WithFailures _); Failed _) -> True
        _ -> False
    }

phaseText :: Wording -> UTCTime -> JobState -> Text
phaseText wording now state = case state.phase of
  Queued -> "Queued"
  NeedsReview -> "Needs review"
  Running -> runningText wording now state
  Finished AllOk -> "Finished · " <> plural "file" (Map.size state.files) <> " · all OK"
  Finished (WithFailures failures) -> "Finished · " <> plural "failure" failures
  Failed _ -> "Failed"
  Cancelled -> "Cancelled"

runningText :: Wording -> UTCTime -> JobState -> Text
runningText wording now state
  | Just quiet <- quietText wording now state = quiet
  | (kindUi kind).showsProgress =
      verb
        <> " • "
        <> count (floor (fractionOf state * 100) :: Int)
        <> " % • "
        <> humanRate wording (rateOf state)
  | otherwise = verb <> "…"
  where
    kind = jobKind state.spec.job
    verb = runningVerbText wording kind
