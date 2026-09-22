module MediaCopy.Report
  ( renderReport
  , renderPlanText
  ) where

import Ascmhl.Hash (Hash (..))
import Ascmhl.Path (RelPath, pathText)
import Ascmhl.Types
import Ascmhl.Write (formatMhlTime)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import System.OsPath (takeFileName)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Interface.Wording (count, resultText)

renderReport :: JobState -> Maybe JobPlan -> Maybe MhlHistory -> Text
renderReport st plan mhlHist =
  ( line "MediaCopy 3000 report"
      <> renderBody st plan
      <> renderFailures st
      <> foldMap (\hist -> renderHistory hist) mhlHist
  )
    & TB.toLazyText
    & TL.toStrict

renderPlanText :: JobSpec -> JobPlan -> Text
renderPlanText spec plan =
  ( field "Job" (display (jobKind spec.job))
      <> renderJobDetails spec.job
      <> field "Created" (formatMhlTime spec.createdAt)
      <> renderPlan plan
  )
    & TB.toLazyText
    & TL.toStrict

line :: Text -> Builder
line text = TB.fromText (text <> "\n")

field :: Text -> Text -> Builder
field label value = line (label <> ": " <> value)

renderBody :: JobState -> Maybe JobPlan -> Builder
renderBody st plan =
  field "Job" (display (jobKind st.spec.job))
    <> renderJobDetails st.spec.job
    <> field "Created" (formatMhlTime st.spec.createdAt)
    <> foldMap (\ready -> renderPlan ready) plan
    <> renderResult (jobKind st.spec.job) st.phase
    <> renderCounts st
    <> foldMap (\p -> field "Manifest" (pathText p)) st.mhlPaths
    <> foldMap (\origin -> field "Originals" origin) st.originsUsed
    <> foldMap (\p -> field "Log" (pathText p)) st.logPath

renderPlan :: JobPlan -> Builder
renderPlan plan =
  line "Plan"
    <> field "  files" (count (V.length plan.steps))
    <> field "  bytes" (T.pack (show plan.totalBytes))
    <> field "  originals" plan.originsUsed
    <> renderExistingCopy plan.spec.job
    <> foldMap (\pass -> renderSealPass pass) plan.sealPass
    <> foldMap (\planned -> renderGeneration planned) (plannedGenerations plan)
    <> foldMap (\target -> renderTarget target) plan.targets
    <> foldMap (\finding -> renderFinding finding) plan.findings

renderExistingCopy :: Job -> Builder
renderExistingCopy = \case
  Offload oj | Just choice <- oj.existingCopy -> field "  existing copy" (display choice)
  _ -> mempty

renderSealPass :: SealPass -> Builder
renderSealPass pass =
  field
    "  seal first"
    ( plural "file" (V.length pass.steps)
        <> ", "
        <> T.pack (show pass.bytes)
        <> " bytes, on failure: "
        <> display pass.onFailure
    )

renderGeneration :: PlannedGeneration -> Builder
renderGeneration planned =
  field
    "  generation"
    (pathText planned.folder <> " (" <> count planned.number <> ", " <> pathText (takeFileName planned.manifest) <> ")")

renderTarget :: Target -> Builder
renderTarget target =
  field "  target" (pathText target.root <> " (" <> display target.state <> ")")

renderFinding :: Finding -> Builder
renderFinding finding =
  line ("  " <> display finding.severity <> ": " <> display finding.code <> " – " <> finding.detail)

renderJobDetails :: Job -> Builder
renderJobDetails job = case job of
  Offload oj ->
    field "Source" (pathText oj.source)
      <> foldMap (\dest -> field "Destination" (pathText dest)) oj.destinations
  (VerifyFolder _; SealMediaSource _) -> folderField
  where
    folderField = field "Folder" (pathText (jobRoot job))

renderResult :: JobKind -> JobPhase -> Builder
renderResult kind phase = case phase of
  Queued -> field "Result" "queued"
  Running -> field "Result" "running"
  NeedsReview -> field "Result" "waiting for review"
  Finished result -> field "Result" (resultText kind result)
  Failed msg -> field "Result" ("failed – " <> msg)
  Cancelled -> field "Result" "cancelled"

renderCounts :: JobState -> Builder
renderCounts st =
  let c = countOutcomes st
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
  let failures = Map.toList st.files & filter (\pair -> isFailure (snd pair).status)
  in if null failures
       then mempty
       else "\nFailures:\n" <> foldMap (\pair -> renderFailureLine pair) failures

renderFailureLine :: (RelPath, FileEntry) -> Builder
renderFailureLine (path, entry) = case entry.status of
  Done (HashMismatch mismatch) ->
    line
      ( "FAILED  "
          <> display path
          <> "  hash mismatch expected "
          <> hashText mismatch.expected
          <> " actual "
          <> hashText mismatch.actual
      )
  Done (IoError msg) -> line ("FAILED  " <> display path <> "  io error " <> msg)
  Done Missing -> line ("MISSING " <> display path)
  _ -> mempty

hashText :: Hash -> Text
hashText hash = display hash.algo <> " " <> hash.value

renderHistory :: MhlHistory -> Builder
renderHistory hist =
  if null hist.generations
    then mempty
    else "\nHistory:\n" <> foldMap (\gen -> renderGenerationLine gen) hist.generations

renderGenerationLine :: Generation -> Builder
renderGenerationLine gen =
  line
    ( T.justifyRight 4 '0' (count gen.number)
        <> "  "
        <> formatMhlTime gen.creator.creationDate
        <> "  "
        <> gen.creator.hostname
        <> " — "
        <> gen.creator.toolName
        <> " "
        <> fromMaybe "" gen.creator.toolVersion
        <> "  "
        <> algosText gen.algos
        <> "  "
        <> display gen.process
        <> "  "
        <> plural "failure" gen.failures
    )
