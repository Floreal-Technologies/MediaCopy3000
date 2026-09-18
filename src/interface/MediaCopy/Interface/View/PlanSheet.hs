module MediaCopy.Interface.View.PlanSheet
  ( SealView (..)
  , sealChoice
  , SheetPage (..)
  , BodyView (..)
  , PhaseView (..)
  , phaseView
  ) where

import Ascmhl.Author qualified as Author
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.Process qualified as Process
import Ascmhl.Time qualified as Time
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import System.OsPath (takeFileName)

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.Finding (Finding)
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PlannedGeneration (PlannedGeneration)
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Target (Target)
import MediaCopy.Domain.Target qualified as Target
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
  , existing :: Maybe Offload.ExistingCopy
  , existingShown :: Bool
  }
  deriving stock (Eq, Show)

sealView :: Wording -> Plan -> SealView
sealView wording plan = case plan.spec.job of
  Job.Offload oj ->
    let sealing = case plan.sealPass of
          Nothing -> False
          Just _ -> True
        anyway = case plan.sealPass of
          Just pass | pass.onFailure == Offload.CopyAnyway -> True
          _ -> False
    in SealView
         { shown = True
         , sealing
         , anyway
         , subtitle = sealSubtitle wording plan
         , policyShown = sealing
         , existing = oj.existingCopy
         , existingShown = V.any (\target -> target.state == Target.Partial) plan.targets
         }
  _ -> SealView {shown = False, sealing = False, anyway = False, subtitle = "", policyShown = False, existing = Nothing, existingShown = False}

sealChoice :: Bool -> Bool -> Offload.SealFirst
sealChoice sealing anyway
  | not sealing = Offload.UseHistory
  | anyway = Offload.SealBeforeCopy Offload.CopyAnyway
  | otherwise = Offload.SealBeforeCopy Offload.StopBeforeCopy

sealSubtitle :: Wording -> Plan -> Text
sealSubtitle wording plan = case plan.sealPass of
  Just pass -> "reads " <> humanBytes wording pass.bytes <> " first, then writes generation " <> count (plan.generations + 1)
  Nothing
    | plan.generations == 0 -> "the media source has no history; sealing records its hashes before a byte is copied"
    | otherwise -> "the media source already holds " <> Prose.plural "generation" plan.generations <> ", which the copies are checked against"

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
  Planning spec -> Just (waiting (Just ("Plan · " <> Job.label spec.job)) PlanningPage "" Nothing)
  PlanError message -> Just (waiting Nothing ErrorPage message Nothing)
  Refreshing plan _ -> Just (waiting Nothing ReadyPage "" (Just (bodyView wording plan)))
  Ready plan ->
    Just
      PhaseView
        { title = Just ("Plan · " <> Job.label plan.spec.job)
        , page = ReadyPage
        , startEnabled = not (Plan.blocked plan)
        , saveEnabled = True
        , problem = ""
        , body = Just (bodyView wording plan)
        }
  where
    waiting title page problem body =
      PhaseView {title, page, startEnabled = False, saveEnabled = False, problem, body}

bodyView :: Wording -> Plan -> BodyView
bodyView wording plan =
  BodyView
    { summary = summaryOf wording plan
    , targets = V.map (targetRow wording) plan.targets
    , findings = findingRowsOf wording plan
    , manifest = manifestRowsOf plan.plugins.contributions
    , script = scriptOf wording plan
    , seal = sealView wording plan
    }

summaryOf :: Wording -> Plan -> Vector RowView
summaryOf wording plan =
  V.fromList
    [ rowView (Prose.plural "file" (V.length plan.steps)) (humanBytes wording plan.totalBytes)
    , rowView "Hash format" (formatText plan)
    , rowView "Steps" (stepCounts wording plan)
    ]

formatText :: Plan -> Text
formatText plan = case plan.format of
  Nothing -> "not settled"
  Just fmt -> display fmt <> " · originals: " <> plan.originsUsed

stepCounts :: Wording -> Plan -> Text
stepCounts wording plan =
  plan.steps
    & V.foldr (\step tally -> Map.insertWith (\_ n -> n + 1) (stepName wording step) (1 :: Int) tally) Map.empty
    & Map.toList
    & map (\(name, n) -> name <> " " <> count n)
    & T.intercalate " · "

stepName :: Wording -> PlanStep -> Text
stepName wording step = case step.op of
  PlanStep.Copy _
    | V.any (\w -> w.mode == PlannedWrite.Overwrite) step.writes -> writeModeText wording PlannedWrite.Overwrite
    | not (V.null step.writes) && V.all (\w -> w.mode == PlannedWrite.Reuse) step.writes -> writeModeText wording PlannedWrite.Reuse
    | otherwise -> writeModeText wording PlannedWrite.New
  PlanStep.VerifyAgainst _ -> "verify"
  PlanStep.ReportNew -> "record"
  PlanStep.ReportMissing -> "missing"

scriptSample :: Int
scriptSample = 20

scriptOf :: Wording -> Plan -> Vector RowView
scriptOf wording plan =
  V.fromList
    [ rowView "Execution" (executionText wording plan)
    , rowView "Instant" (Time.render plan.spec.createdAt)
    , rowView "Bytes" (count plan.bytesToRead <> " to read, " <> count plan.totalBytes <> " in the main pass")
    , rowView "Creates" (count (V.length plan.creates) <> " directories")
    , rowView "Directories" (directoriesText plan)
    , rowView "Ignores" (T.intercalate " · " (V.toList plan.ignorePatterns))
    ]
    <> V.map (generationRow wording) (Plan.plannedGenerations plan)
    <> V.map (stepRow wording) (V.take scriptSample plan.steps)
    <> overflowRow (V.length plan.steps)

directoriesText :: Plan -> Text
directoriesText plan =
  Plan.plannedGenerations plan
    & V.toList
    & map (\planned -> count (V.length planned.directories))
    & T.intercalate " · "

executionText :: Wording -> Plan -> Text
executionText wording plan = case plan.execution of
  Plan.CopyInto copy -> processKindText wording Process.Transfer <> " · source: " <> OsPath.toText copy.source <> " · originals: " <> plan.originsUsed <> carriedText copy.carried
  Plan.RecordAt record -> processKindText wording Process.InPlace <> " · folder: " <> OsPath.toText record.folder

carriedText :: Int -> Text
carriedText carried
  | carried <= 0 = ""
  | otherwise = " · carries " <> Prose.plural "generation" carried

generationRow :: Wording -> PlannedGeneration -> RowView
generationRow wording planned =
  rowView
    (OsPath.toText (takeFileName planned.manifest))
    (OsPath.toText planned.folder <> " · generation " <> count planned.number <> " · " <> processKindText wording planned.process)

stepRow :: Wording -> PlanStep -> RowView
stepRow wording step =
  rowView
    (display step.path)
    (humanBytes wording step.size <> " · " <> stepName wording step <> " · " <> expectationText step.op <> tempText step)

expectationText :: PlanStep.FileOp -> Text
expectationText op = case op of
  PlanStep.Copy PlanStep.NoOriginal -> "no original"
  PlanStep.Copy (PlanStep.Recorded _) -> "recorded hash"
  PlanStep.Copy PlanStep.FromSealPass -> "from the seal pass"
  PlanStep.VerifyAgainst _ -> "history hash"
  PlanStep.ReportNew -> "new"
  PlanStep.ReportMissing -> "missing"

tempText :: PlanStep -> Text
tempText step = case step.writes V.!? 0 of
  Nothing -> ""
  Just w -> " · " <> OsPath.toText (takeFileName w.temp)

overflowRow :: Int -> Vector RowView
overflowRow total
  | total <= scriptSample = V.empty
  | otherwise = V.singleton (rowView ("and " <> count (total - scriptSample) <> " more") "")

targetRow :: Wording -> Target -> RowView
targetRow wording target =
  rowView
    (OsPath.toText (takeFileName target.root))
    (OsPath.toText target.root <> " · " <> freeText wording target <> " · " <> targetStateText wording target.state)

freeText :: Wording -> Target -> Text
freeText wording target = maybe "free space unknown" (\free -> humanBytes wording free <> " free") target.freeBytes

findingRow :: Wording -> Finding -> RowView
findingRow wording finding =
  (rowView (findingText wording finding.code) finding.detail)
    { tone = Just (case finding.severity of Severity.Blocker -> Bad; Severity.Warning -> Warn)
    }

findingRowsOf :: Wording -> Plan -> Vector RowView
findingRowsOf wording plan =
  V.map (findingRow wording) (Plan.blockers plan)
    <> V.map (pluginFindingRow wording) (PluginPlan.blockers plan.plugins)
    <> V.map (findingRow wording) (V.filter (\finding -> finding.severity == Severity.Warning) plan.findings)
    <> V.map (pluginFindingRow wording) (V.filter (\finding -> finding.severity == Severity.Warning) plan.plugins.findings)

pluginFindingRow :: Wording -> PluginFinding -> RowView
pluginFindingRow wording finding =
  let (title, detail) = pluginFindingTexts wording finding
  in (rowView title detail) {tone = Just (case finding.severity of Severity.Blocker -> Bad; Severity.Warning -> Warn)}

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
                ("Metadata for " <> Prose.plural "file" (Map.size contributions.fileMetadata) <> (if isJust contributions.manifestMetadata then " and the manifest" else ""))
                (T.intercalate ", " (V.toList (V.map (\ref -> ref.name) contributions.contributors)))
            )
