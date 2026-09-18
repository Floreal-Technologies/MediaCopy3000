module MediaCopy.Report
  ( renderReport
  , renderPlanText
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Author (Author)
import Ascmhl.Author qualified as Author
import Ascmhl.Creator qualified as Creator
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.History (History)
import Ascmhl.History qualified as History
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.Time qualified as Time
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import System.OsPath (takeFileName)

import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Finding (Finding)
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState (JobState)
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlannedGeneration (PlannedGeneration)
import MediaCopy.Domain.PlannedGeneration qualified as PlannedGeneration
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginPlan (PluginPlan)
import MediaCopy.Domain.PluginPlan qualified as PluginPlan
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginReport qualified as PluginReport
import MediaCopy.Domain.PluginState (PluginState)
import MediaCopy.Domain.PluginState qualified as PluginState
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Domain.Target (Target)
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording (count, resultText)

renderReport :: Wording -> JobState -> Maybe Plan -> Maybe History -> Text
renderReport wording st plan mhlHist =
  ( line "MediaCopy 3000 report"
      <> renderBody wording st plan
      <> renderFailures st
      <> renderPlugins st.plugins
      <> foldMap renderHistory mhlHist
  )
    & TB.toLazyText
    & TL.toStrict

renderPlanText :: JobSpec -> Plan -> Text
renderPlanText spec plan =
  ( field "Job" (display (Job.kind spec.job))
      <> renderJobDetails spec.job
      <> field "Created" (Time.render spec.createdAt)
      <> renderPlan plan
  )
    & TB.toLazyText
    & TL.toStrict

line :: Text -> Builder
line text = TB.fromText (text <> "\n")

field :: Text -> Text -> Builder
field label value = line (label <> ": " <> value)

renderBody :: Wording -> JobState -> Maybe Plan -> Builder
renderBody wording st plan =
  field "Job" (display (Job.kind st.spec.job))
    <> renderJobDetails st.spec.job
    <> field "Created" (Time.render st.spec.createdAt)
    <> foldMap renderPlan plan
    <> renderResult wording (Job.kind st.spec.job) st.phase
    <> renderCounts st
    <> foldMap (field "Manifest" . OsPath.toText) st.mhlPaths
    <> foldMap (field "Originals") st.originsUsed
    <> foldMap (field "Log" . OsPath.toText) st.logPath

renderPlan :: Plan -> Builder
renderPlan plan =
  line "Plan"
    <> field "  files" (count (V.length plan.steps))
    <> field "  bytes" (T.show plan.totalBytes)
    <> field "  originals" plan.originsUsed
    <> renderExistingCopy plan.spec.job
    <> foldMap renderSealPass plan.sealPass
    <> foldMap renderGeneration (Plan.plannedGenerations plan)
    <> foldMap renderTarget plan.targets
    <> foldMap renderFinding plan.findings
    <> renderPluginPlan plan.plugins

renderExistingCopy :: Job -> Builder
renderExistingCopy = \case
  Job.Offload oj | Just choice <- oj.existingCopy -> field "  existing copy" (display choice)
  _ -> mempty

renderSealPass :: Plan.SealPass -> Builder
renderSealPass pass =
  field
    "  seal first"
    ( Prose.plural "file" (V.length pass.steps)
        <> ", "
        <> T.show pass.bytes
        <> " bytes, on failure: "
        <> display pass.onFailure
    )

renderGeneration :: PlannedGeneration -> Builder
renderGeneration planned =
  field
    "  generation"
    (OsPath.toText planned.folder <> " (" <> count planned.number <> ", " <> OsPath.toText (takeFileName planned.manifest) <> ")")

renderTarget :: Target -> Builder
renderTarget target =
  field "  target" (OsPath.toText target.root <> " (" <> display target.state <> ")")

renderFinding :: Finding -> Builder
renderFinding finding =
  line ("  " <> display finding.severity <> ": " <> display finding.code <> " – " <> finding.detail)

renderJobDetails :: Job -> Builder
renderJobDetails job = case job of
  Job.Offload oj ->
    field "Source" (OsPath.toText oj.source)
      <> foldMap (field "Destination" . OsPath.toText) oj.destinations
  (Job.VerifyFolder _; Job.SealMediaSource _) -> folderField
  where
    folderField = field "Folder" (OsPath.toText (Job.root job))

renderResult :: Wording -> Job.Kind -> JobState.Phase -> Builder
renderResult wording kind phase = case phase of
  JobState.Queued -> field "Result" "queued"
  JobState.Running -> field "Result" "running"
  JobState.NeedsReview -> field "Result" "waiting for review"
  JobState.Finished result -> field "Result" (resultText wording kind result)
  JobState.Failed msg -> field "Result" ("failed – " <> msg)
  JobState.Cancelled -> field "Result" "cancelled"

renderCounts :: JobState -> Builder
renderCounts st =
  let c = JobState.counts st
  in field
       "Files"
       ( count (Map.size st.files)
           <> " total, "
           <> count c.verified
           <> " verified, "
           <> count c.failed
           <> " hash mismatch/io error, "
           <> count c.missing
           <> " missing, "
           <> count c.new
           <> " new"
           <> ", "
           <> count c.replaced
           <> " replaced"
       )

renderFailures :: JobState -> Builder
renderFailures st =
  let failures = Map.toList st.files & filter (\pair -> FileStatus.isFailure (snd pair).status)
  in if null failures
       then mempty
       else "\nFailures:\n" <> foldMap renderFailureLine failures

renderFailureLine :: (RelPath, JobState.FileEntry) -> Builder
renderFailureLine (path, entry) = case entry.status of
  FileStatus.Done (FileOutcome.HashMismatch mismatch) ->
    line
      ( "FAILED  "
          <> display path
          <> "  hash mismatch expected "
          <> hashText mismatch.expected
          <> " actual "
          <> hashText mismatch.actual
      )
  FileStatus.Done (FileOutcome.IoError msg) -> line ("FAILED  " <> display path <> "  io error " <> msg)
  FileStatus.Done FileOutcome.Missing -> line ("MISSING " <> display path)
  _ -> mempty

hashText :: Hash -> Text
hashText hash = display hash.algo <> " " <> hash.value

renderHistory :: History -> Builder
renderHistory hist =
  if null hist.generations
    then mempty
    else "\nHistory:\n" <> foldMap renderGenerationLine hist.generations

renderGenerationLine :: History.Generation -> Builder
renderGenerationLine gen =
  line
    ( T.justifyRight 4 '0' (count gen.number)
        <> "  "
        <> Time.render gen.creator.creationDate
        <> "  "
        <> gen.creator.hostname
        <> " – "
        <> gen.creator.toolName
        <> " "
        <> fromMaybe "" gen.creator.toolVersion
        <> "  "
        <> Algo.renderSet gen.algos
        <> "  "
        <> display gen.process
        <> "  "
        <> Prose.plural "failure" gen.failures
    )

renderPluginPlan :: PluginPlan -> Builder
renderPluginPlan pluginPlan =
  foldMap (\ref -> field "  plug-in" (ref.name <> " (" <> ref.id <> ")")) pluginPlan.active
    <> foldMap (\finding -> line ("  " <> display finding.severity <> ": " <> display finding)) pluginPlan.findings
    <> foldMap (field "  author" . authorText) pluginPlan.contributions.authors
    <> metadataLine pluginPlan.contributions
  where
    metadataLine contributions
      | Map.null contributions.fileMetadata && isNothing contributions.manifestMetadata = mempty
      | otherwise =
          field
            "  metadata"
            ( Prose.plural "file" (Map.size contributions.fileMetadata)
                <> (if isJust contributions.manifestMetadata then ", manifest" else "")
                <> " – "
                <> T.intercalate ", " (V.toList (V.map (\ref -> ref.name) contributions.contributors))
            )

authorText :: Author -> Text
authorText author =
  author.name
    <> foldMap (\role -> " (" <> role <> ")") author.role
    <> foldMap (\email -> " <" <> email <> ">") author.email
    <> foldMap (" " <>) author.phone

renderPlugins :: PluginState -> Builder
renderPlugins st
  | st == PluginState.empty = mempty
  | otherwise =
      "\nPlug-ins:\n"
        <> foldMap (\finding -> line (T.toUpper (display finding.severity) <> "  " <> display finding)) st.warnings
        <> foldMap (\(ref, left) -> line ("NOT INSPECTED  " <> ref.name <> "  " <> Prose.plural "file" left)) (Map.toList st.notInspected)
        <> foldMap annotationLines (Map.toList st.annotations)
  where
    annotationLines (path, notes) =
      foldMap (\note -> line ("NOTE  " <> display path <> "  " <> note.plugin.name <> "  " <> note.label <> ": " <> note.value)) notes
