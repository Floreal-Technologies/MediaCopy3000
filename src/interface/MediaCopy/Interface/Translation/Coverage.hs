module MediaCopy.Interface.Translation.Coverage
  ( wordingFaults
  , messageShapeFaults
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Hash qualified as Hash
import Control.Exception (evaluate)
import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fluent.AST
import System.OsPath (unsafeEncodeUtf)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Interface.Command
import MediaCopy.Interface.Theme
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording

wordingFaults :: Wording -> IO (List Text)
wordingFaults wording = nubOrd . catMaybes <$> traverse attempt (expectedWording wording {strict = True})
  where
    attempt text =
      try (evaluate (T.length text)) >>= \case
        Left (WordingFault fault) -> pure (Just fault)
        Right _ -> pure Nothing

expectedWording :: Wording -> List Text
expectedWording wording =
  concatMap (\kind -> List.map (resultText wording kind) (JobResult.AllOk : map JobResult.WithFailures counts)) [minBound @Job.Kind ..]
    <> [noHistoryText wording (unsafeEncodeUtf "card")]
    <> [jobFinishedToast wording "A001" minBound JobResult.AllOk, jobFailedToast wording "A001" "disk full", savePlanTitle wording, saveReportTitle wording]
    <> map (humanBytes wording) [0, 12_288, 999_500, 95_600_000_000, 2_500_000_000_000]
    <> map (humanRate wording) [0, 1.1e9]
    <> map (humanEta wording) [34, 125, 3_720]
    <> map (humanPercent wording) [0, 42]
    <> map (runningVerbText wording) [minBound ..]
    <> map (\doing -> quietLine wording doing 25) (JobState.WritingManifest : map JobState.OnFile fileStatuses)
    <> map (jobKindText wording) [minBound ..]
    <> map (writeModeText wording) [minBound ..]
    <> map (processKindText wording) [minBound ..]
    <> map (targetStateText wording) [minBound ..]
    <> map (findingText wording) [minBound ..]
    <> map (pluginFaultText wording) [PluginFinding.Unavailable "timed out", PluginFinding.FieldMissing "camera", PluginFinding.BadOutput "not JSON"]
    <> [pluginToastText wording "A001" PluginFinding.PluginFinding {plugin = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}, severity = Severity.Warning, about = PluginFinding.Faulted (PluginFinding.BadOutput "x")}]
    <> [fst (pluginFindingTexts wording PluginFinding.PluginFinding {plugin = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}, severity = Severity.Warning, about = PluginFinding.Said PluginFinding.Says {key = "k", title = "t", detail = "d"}})]
    <> map (displayBase wording) [minBound ..]
    <> map (displayAccent wording) (Nothing : map Just [minBound ..])
    <> map (commandLabel wording) commands
    <> map (sectionLabel wording) [minBound ..]
    <> [mainMenuLabel wording, palettePlaceholder wording, paletteNoMatch wording]

fileStatuses :: List FileStatus
fileStatuses =
  [FileStatus.Pending, FileStatus.Hashing, FileStatus.Copying, FileStatus.Flushing, FileStatus.Publishing, FileStatus.Verifying]
    <> List.map FileStatus.Done [FileOutcome.Ok, FileOutcome.HashMismatch mismatch, FileOutcome.Missing, FileOutcome.New, FileOutcome.IoError "disk full", FileOutcome.Replaced mismatch]
  where
    mismatch = FileOutcome.Mismatch {expected = hash, actual = hash}
    hash = Hash.Hash {algo = Algo.XXH64, value = "0"}

counts :: List Int
counts = [0, 1, 3, 1_000_000]

messageShapeFaults :: Resource -> Resource -> List Text
messageShapeFaults reference translation =
  concatMap fault (messageShapes reference)
  where
    translated = messageShapes translation
    fault (identifier, expected) = case List.lookup identifier translated of
      Nothing -> ["missing message: " <> identifier]
      Just actual ->
        differ "missing" (Set.difference expected actual)
          <> differ "unknown" (Set.difference actual expected)
      where
        differ label names =
          [ "message " <> identifier <> ": " <> label <> " variables: " <> T.intercalate ", " (map ("$" <>) (Set.toAscList names))
          | not (Set.null names)
          ]

messageShapes :: Resource -> List (Text, Set Text)
messageShapes resource =
  mapMaybe
    ( \case
        MessageEntry message ->
          Just (identifierText message.id, foldMap patternVariables message.value <> foldMap (\(Attribute _ pattern') -> patternVariables pattern') message.attributes)
        _ -> Nothing
    )
    resource.entries

patternVariables :: Pattern -> Set Text
patternVariables (Pattern elements) = foldMap elementVariables elements

elementVariables :: PatternElement -> Set Text
elementVariables = \case
  Placeable placeable -> expressionVariables (placeableExpression placeable)
  InlineText _ -> Set.empty
  BlockText _ -> Set.empty

expressionVariables :: Expression -> Set Text
expressionVariables = \case
  Select (SelectExpression selector (VariantList variants)) ->
    inlineVariables selector <> foldMap (\variant -> patternVariables variant.value) variants
  Inline inline -> inlineVariables inline

inlineVariables :: InlineExpression -> Set Text
inlineVariables = \case
  VariableReference identifier -> Set.singleton (identifierText identifier)
  PlaceableExpression expression -> expressionVariables expression
  FunctionReference _ arguments -> argumentVariables arguments
  TermReference _ _ arguments -> foldMap argumentVariables arguments
  StringLiteralExpression _ -> Set.empty
  NumberLiteralExpression _ -> Set.empty
  MessageReference _ _ -> Set.empty

argumentVariables :: CallArguments -> Set Text
argumentVariables (CallArguments arguments) = foldMap (either (const Set.empty) inlineVariables) arguments

identifierText :: Identifier -> Text
identifierText (Identifier identifier) = identifier
