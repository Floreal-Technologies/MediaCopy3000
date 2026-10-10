module MediaCopy.Interface.View.PlanSheet
  ( SealView (..)
  , sealView
  , sealChoice
  , SheetPage (..)
  , BodyView (..)
  , PhaseView (..)
  , phaseView
  ) where

import Ascmhl.Path (pathText)
import Ascmhl.Types (Author (..), ProcessKind (..))
import Ascmhl.Write (formatMhlTime)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (takeFileName)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Interface.View.Row
import MediaCopy.Interface.Wording
import MediaCopy.Model (PlanPhase (..))

-- $setup
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)

data SealView = SealView
  { shown :: Bool
  , sealing :: Bool
  , anyway :: Bool
  , subtitle :: Text
  , policyShown :: Bool
  , existing :: Maybe ExistingCopy
  , existingShown :: Bool
  }
  deriving stock (Eq, Show)

sealView :: Wording -> JobPlan -> SealView
sealView wording plan = case plan.spec.job of
  Offload oj ->
    let sealing = case plan.sealPass of
          Nothing -> False
          Just _ -> True
        anyway = case plan.sealPass of
          Just pass | pass.onFailure == CopyAnyway -> True
          _ -> False
    in SealView
         { shown = True
         , sealing
         , anyway
         , subtitle = sealSubtitle wording plan
         , policyShown = sealing
         , existing = oj.existingCopy
         , existingShown = V.any (\target -> target.state == Partial) plan.targets
         }
  _ -> SealView {shown = False, sealing = False, anyway = False, subtitle = "", policyShown = False, existing = Nothing, existingShown = False}

-- |
-- >>> sealChoice False True
-- UseHistory
-- >>> sealChoice True True
-- SealBeforeCopy CopyAnyway
-- >>> sealChoice True False
-- SealBeforeCopy StopBeforeCopy
sealChoice :: Bool -> Bool -> SealFirst
sealChoice sealing anyway
  | not sealing = UseHistory
  | anyway = SealBeforeCopy CopyAnyway
  | otherwise = SealBeforeCopy StopBeforeCopy

sealSubtitle :: Wording -> JobPlan -> Text
sealSubtitle wording plan = case plan.sealPass of
  Just pass -> "reads " <> humanBytes wording pass.bytes <> " first, then writes generation " <> count (plan.generations + 1)
  Nothing
    | plan.generations == 0 -> "the media source has no history; sealing records its hashes before a byte is copied"
    | otherwise -> "the media source already holds " <> plural "generation" plan.generations <> ", which the copies are checked against"

data SheetPage = PlanningPage | ReadyPage | ErrorPage
  deriving stock (Eq, Show)

data BodyView = BodyView
  { summary :: Vector RowView
  , targets :: Vector RowView
  , findings :: Vector RowView
  , manifest :: Vector RowView
  , script :: Vector RowView
  , seal :: SealView
  }
  deriving stock (Eq, Show)

data PhaseView = PhaseView
  { title :: Maybe Text
  , page :: SheetPage
  , startEnabled :: Bool
  , saveEnabled :: Bool
  , problem :: Text
  , body :: Maybe BodyView
  }
  deriving stock (Eq, Show)

-- |
-- >>> phaseView (embeddedWording English) Idle
-- Nothing
-- >>> fmap (\view -> (view.title, view.page, view.startEnabled)) (phaseView (embeddedWording English) (PlanError "boom"))
-- Just (Nothing,ErrorPage,False)
phaseView :: Wording -> PlanPhase -> Maybe PhaseView
phaseView wording = \case
  Idle -> Nothing
  Planning spec -> Just (waiting (Just ("Plan · " <> jobLabel spec.job)) PlanningPage "" Nothing)
  PlanError message -> Just (waiting Nothing ErrorPage message Nothing)
  Refreshing plan _ -> Just (waiting Nothing ReadyPage "" (Just (bodyView wording plan)))
  Ready plan ->
    Just
      PhaseView
        { title = Just ("Plan · " <> jobLabel plan.spec.job)
        , page = ReadyPage
        , startEnabled = not (planBlocked plan)
        , saveEnabled = True
        , problem = ""
        , body = Just (bodyView wording plan)
        }
  where
    waiting title page problem body =
      PhaseView {title, page, startEnabled = False, saveEnabled = False, problem, body}

bodyView :: Wording -> JobPlan -> BodyView
bodyView wording plan =
  BodyView
    { summary = summaryOf wording plan
    , targets = V.map (targetRow wording) plan.targets
    , findings = findingRowsOf wording plan
    , manifest = manifestRowsOf plan.plugins.contributions
    , script = scriptOf wording plan
    , seal = sealView wording plan
    }

summaryOf :: Wording -> JobPlan -> Vector RowView
summaryOf wording plan =
  V.fromList
    [ rowView (plural "file" (V.length plan.steps)) (humanBytes wording plan.totalBytes)
    , rowView "Hash format" (formatText plan)
    , rowView "Steps" (stepCounts wording plan)
    ]

formatText :: JobPlan -> Text
formatText plan = case plan.format of
  Nothing -> "not settled"
  Just fmt -> display fmt <> " · originals: " <> plan.originsUsed

stepCounts :: Wording -> JobPlan -> Text
stepCounts wording plan =
  plan.steps
    & V.foldr (\step tally -> Map.insertWith (\_ n -> n + 1) (stepName wording step) (1 :: Int) tally) Map.empty
    & Map.toList
    & map (\(name, n) -> name <> " " <> count n)
    & T.intercalate " · "

stepName :: Wording -> PlanStep -> Text
stepName wording step = case step.op of
  Copy _
    | V.any (\w -> w.mode == Overwrite) step.writes -> writeModeText wording Overwrite
    | not (V.null step.writes) && V.all (\w -> w.mode == Reuse) step.writes -> writeModeText wording Reuse
    | otherwise -> writeModeText wording WriteNew
  VerifyAgainst _ -> "verify"
  ReportNew -> "record"
  ReportMissing -> "missing"

scriptSample :: Int
scriptSample = 20

scriptOf :: Wording -> JobPlan -> Vector RowView
scriptOf wording plan =
  V.fromList
    [ rowView "Execution" (executionText wording plan)
    , rowView "Instant" (formatMhlTime plan.spec.createdAt)
    , rowView "Bytes" (count plan.bytesToRead <> " to read, " <> count plan.totalBytes <> " in the main pass")
    , rowView "Creates" (count (V.length plan.creates) <> " directories")
    , rowView "Directories" (directoriesText plan)
    , rowView "Ignores" (T.intercalate " · " (V.toList plan.ignorePatterns))
    ]
    <> V.map (generationRow wording) (plannedGenerations plan)
    <> V.map (stepRow wording) (V.take scriptSample plan.steps)
    <> overflowRow (V.length plan.steps)

directoriesText :: JobPlan -> Text
directoriesText plan =
  plannedGenerations plan
    & V.toList
    & map (\planned -> count (V.length planned.directories))
    & T.intercalate " · "

executionText :: Wording -> JobPlan -> Text
executionText wording plan = case plan.execution of
  CopyInto copy -> processKindText wording ProcessTransfer <> " · source: " <> pathText copy.source <> " · originals: " <> plan.originsUsed <> carriedText copy.carried
  RecordAt record -> processKindText wording ProcessInPlace <> " · folder: " <> pathText record.folder

carriedText :: Int -> Text
carriedText carried
  | carried <= 0 = ""
  | otherwise = " · carries " <> plural "generation" carried

generationRow :: Wording -> PlannedGeneration -> RowView
generationRow wording planned =
  rowView
    (pathText (takeFileName planned.manifest))
    (pathText planned.folder <> " · generation " <> count planned.number <> " · " <> processKindText wording planned.process)

stepRow :: Wording -> PlanStep -> RowView
stepRow wording step =
  rowView
    (display step.path)
    (humanBytes wording step.size <> " · " <> stepName wording step <> " · " <> expectationText step.op <> tempText step)

expectationText :: FileOp -> Text
expectationText op = case op of
  Copy NoOriginal -> "no original"
  Copy (Recorded _) -> "recorded hash"
  Copy FromSealPass -> "from the seal pass"
  VerifyAgainst _ -> "history hash"
  ReportNew -> "new"
  ReportMissing -> "missing"

tempText :: PlanStep -> Text
tempText step = case step.writes V.!? 0 of
  Nothing -> ""
  Just w -> " · " <> pathText (takeFileName w.temp)

overflowRow :: Int -> Vector RowView
overflowRow total
  | total <= scriptSample = V.empty
  | otherwise = V.singleton (rowView ("and " <> count (total - scriptSample) <> " more") "")

targetRow :: Wording -> Target -> RowView
targetRow wording target =
  rowView
    (pathText (takeFileName target.root))
    (pathText target.root <> " · " <> freeText wording target <> " · " <> targetStateText wording target.state)

freeText :: Wording -> Target -> Text
freeText wording target = maybe "free space unknown" (\free -> humanBytes wording free <> " free") target.freeBytes

findingRow :: Wording -> Finding -> RowView
findingRow wording finding =
  (rowView (findingText wording finding.code) finding.detail)
    { tone = Just (case finding.severity of Blocker -> Bad; Warning -> Warn)
    }

findingRowsOf :: Wording -> JobPlan -> Vector RowView
findingRowsOf wording plan =
  V.map (findingRow wording) (blockers plan)
    <> V.map (pluginFindingRow wording) (pluginBlockers plan.plugins)
    <> V.map (findingRow wording) (V.filter (\finding -> finding.severity == Warning) plan.findings)
    <> V.map (pluginFindingRow wording) (V.filter (\finding -> finding.severity == Warning) plan.plugins.findings)

pluginFindingRow :: Wording -> PluginFinding -> RowView
pluginFindingRow wording finding =
  let (title, detail) = pluginFindingTexts wording finding
  in (rowView title detail) {tone = Just (case finding.severity of Blocker -> Bad; Warning -> Warn)}

manifestRowsOf :: Contributions -> Vector RowView
manifestRowsOf contributions =
  V.map authorRow contributions.authors <> metadataRow
  where
    authorRow author = rowView author.name (T.intercalate " · " (catMaybes [author.role, author.email, author.phone]))
    metadataRow
      | Map.null contributions.fileMetadata && isNothing contributions.manifestMetadata = V.empty
      | otherwise =
          V.singleton
            ( rowView
                ("Metadata for " <> plural "file" (Map.size contributions.fileMetadata) <> (if isJust contributions.manifestMetadata then " and the manifest" else ""))
                (T.intercalate ", " (V.toList (V.map (\ref -> ref.name) contributions.contributors)))
            )
