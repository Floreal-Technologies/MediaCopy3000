-- | The plan sheet: what a job will do, shown before it starts.
module MediaCopy.Gtk.Widgets.PlanSheet
  ( PlanSheet
  , newPlanSheet
  , renderPlanSheet
  ) where

import Ascmhl.Path (pathText)
import Ascmhl.Write (formatMhlTime)
import Control.Monad (void, when)
import Data.Function ((&))
import Data.GI.Base
import Data.IORef (IORef, newIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import System.OsPath (takeFileName)

import MediaCopy.Domain.Job (ExistingCopy (..), Job (..), JobSpec (..), OffloadJob (..), OnSealFailure (..), SealFirst (..), jobLabel)
import MediaCopy.Domain.JobFormat (formatAlgo)
import MediaCopy.Domain.Plan
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Wording (count, humanBytes)
import MediaCopy.Model (Model (..), PlanPhase (..), UiMessage (..))

data PlanSheet = PlanSheet
  { phaseCell :: Cell PlanPhase
  , openCell :: Cell Bool
  }

newPlanSheet :: Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO PlanSheet
newPlanSheet window dispatch = do
  seal <- newSealGroup dispatch
  body <- newReadyBody seal
  (stack, errorPage) <- newStackPages body.scroll
  saveBtn <- new Gtk.Button [#label := "Save _Plan…", #useUnderline := True, #sensitive := False, On #clicked (dispatch SavePlan)]
  shell <-
    newDialogShell
      [#title := "Plan", #contentWidth := 560, #contentHeight := 640]
      ShellButtons
        { leading = ShellButton {label = "_Back", clicked = dispatch DiscardPlan}
        , primary = ShellButton {label = "_Start", clicked = dispatch ConfirmPlan}
        }
      [saveBtn]
      stack
  let sheet = Sheet {dialog = shell.dialog, startBtn = shell.primaryButton, saveBtn, stack, errorPage, body}
  phaseCell <- newCell (renderPhase sheet)
  openCell <- newOpenCell shell.dialog window
  onDialogClosed shell.dialog openCell (dispatch DiscardPlan)
  pure PlanSheet {phaseCell, openCell}

-- | Everything a phase paint writes: the shell it titles, the stack it turns, and the ready body.
data Sheet = Sheet
  { dialog :: Adw.Dialog
  , startBtn :: Gtk.Button
  , saveBtn :: Gtk.Button
  , stack :: Gtk.Stack
  , errorPage :: Adw.StatusPage
  , body :: ReadyBody
  }

-- | The sheet's three faces, in the order the stack holds them.
newStackPages :: Gtk.ScrolledWindow -> IO (Gtk.Stack, Adw.StatusPage)
newStackPages ready = do
  stack <- new Gtk.Stack []
  planningPage <- new Adw.StatusPage [#title := "Reading the Folder…", #description := "No file has been changed"]
  -- Gtk.Spinner, not Adw.Spinner: the latter arrives in libadwaita 1.6, above the floor the
  -- oldest supported distribution sets. See .tasks/lessons.md.
  spinner <-
    new
      Gtk.Spinner
      [ #spinning := True
      , #widthRequest := 32
      , #heightRequest := 32
      , #halign := Gtk.AlignCenter
      , #accessibleRole := Gtk.AccessibleRolePresentation
      ]
  Adw.statusPageSetChild planningPage (Just spinner)
  errorPage <- new Adw.StatusPage [#title := "The Plan Could Not Be Made"]
  void (Gtk.stackAddNamed stack planningPage (Just "planning"))
  void (Gtk.stackAddNamed stack ready (Just "ready"))
  void (Gtk.stackAddNamed stack errorPage (Just "error"))
  pure (stack, errorPage)

renderPhase :: Sheet -> PlanPhase -> IO ()
renderPhase sheet phase = case phase of
  Idle -> pure ()
  Planning spec -> do
    set sheet.dialog [#title := "Plan · " <> jobLabel spec.job]
    set sheet.startBtn [#sensitive := False]
    set sheet.saveBtn [#sensitive := False]
    Gtk.stackSetVisibleChildName sheet.stack "planning"
  PlanError message -> do
    set sheet.errorPage [#description := message]
    set sheet.startBtn [#sensitive := False]
    set sheet.saveBtn [#sensitive := False]
    Gtk.stackSetVisibleChildName sheet.stack "error"
  Ready plan -> do
    set sheet.dialog [#title := "Plan · " <> jobLabel plan.spec.job]
    renderReadyBody sheet.body plan
    set sheet.startBtn [#sensitive := not (planBlocked plan)]
    set sheet.saveBtn [#sensitive := True]
    Gtk.stackSetVisibleChildName sheet.stack "ready"

-- * The ready body

-- | What the sheet shows once a plan is made: one group per part of it, and the rows each group holds.
data ReadyBody = ReadyBody
  { scroll :: Gtk.ScrolledWindow
  , summaryGroup :: Adw.PreferencesGroup
  , summaryRows :: IORef (Vector Adw.ActionRow)
  , targetGroup :: Adw.PreferencesGroup
  , targetRows :: IORef (Vector Adw.ActionRow)
  , findingGroup :: Adw.PreferencesGroup
  , findingRows :: IORef (Vector Adw.ActionRow)
  , scriptExpander :: Adw.ExpanderRow
  , scriptRows :: IORef (Vector Adw.ActionRow)
  , seal :: SealControls
  }

newReadyBody :: SealControls -> IO ReadyBody
newReadyBody seal = do
  readyBox <- paddedBox Gtk.OrientationVertical 18 18
  summaryGroup <- new Adw.PreferencesGroup [#title := "This Job"]
  targetGroup <- new Adw.PreferencesGroup [#title := "Destinations"]
  findingGroup <- new Adw.PreferencesGroup [#title := "Findings"]
  Gtk.boxAppend readyBox summaryGroup
  Gtk.boxAppend readyBox seal.group
  Gtk.boxAppend readyBox targetGroup
  Gtk.boxAppend readyBox findingGroup
  scriptGroup <- new Adw.PreferencesGroup [#title := "Details"]
  scriptExpander <- new Adw.ExpanderRow [#title := "Steps", #expanded := False]
  Adw.preferencesGroupAdd scriptGroup scriptExpander
  Gtk.boxAppend readyBox scriptGroup
  scroll <- new Gtk.ScrolledWindow [#child := readyBox, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
  summaryRows <- newIORef V.empty
  targetRows <- newIORef V.empty
  findingRows <- newIORef V.empty
  scriptRows <- newIORef V.empty
  pure
    ReadyBody
      { scroll
      , summaryGroup
      , summaryRows
      , targetGroup
      , targetRows
      , findingGroup
      , findingRows
      , scriptExpander
      , scriptRows
      , seal
      }

renderReadyBody :: ReadyBody -> JobPlan -> IO ()
renderReadyBody body plan = do
  renderRows body.summaryGroup body.summaryRows (summaryOf plan)
  renderRows body.targetGroup body.targetRows (V.map (\target -> targetRow target) plan.targets)
  renderRows body.findingGroup body.findingRows (V.map (\finding -> findingRow finding) (orderedFindings plan))
  renderActionRows body.scriptRows (InExpander body.scriptExpander) (scriptOf plan)
  renderSealChoice body.seal plan

-- * The seal choice

-- | The "Before Copying" group. `suppress` is True while a render writes the switches,
-- so a programmatic write never dispatches.
data SealControls = SealControls
  { group :: Adw.PreferencesGroup
  , sealSwitch :: Adw.SwitchRow
  , policySwitch :: Adw.SwitchRow
  , resumeButton :: Gtk.CheckButton
  , replaceButton :: Gtk.CheckButton
  , existingRow :: Adw.ActionRow
  , suppress :: IORef Bool
  }

newSealGroup :: (UiMessage -> IO ()) -> IO SealControls
newSealGroup dispatch = do
  group <- new Adw.PreferencesGroup [#title := "Before Copying"]
  suppress <- newIORef False
  sealSwitch <- new Adw.SwitchRow [#title := "Seal the media source first", #active := False]
  policySwitch <- new Adw.SwitchRow [#title := "Copy anyway if the seal finds a problem", #active := False]
  resumeButton <- new Gtk.CheckButton [#label := "Resume", #valign := Gtk.AlignCenter]
  replaceButton <- new Gtk.CheckButton [#label := "Replace", #valign := Gtk.AlignCenter]
  Gtk.checkButtonSetGroup replaceButton (Just resumeButton)
  existingRow <- new Adw.ActionRow [#title := "Existing copy", #subtitle := "a destination already holds part of this media source"]
  Adw.actionRowAddSuffix existingRow resumeButton
  Adw.actionRowAddSuffix existingRow replaceButton
  Adw.preferencesGroupAdd group existingRow
  Adw.preferencesGroupAdd group sealSwitch
  Adw.preferencesGroupAdd group policySwitch
  let seal = SealControls {group, sealSwitch, policySwitch, resumeButton, replaceButton, existingRow, suppress}
  void (on sealSwitch (Adw.PropertyNotify #active) (const (reportSealChoice seal dispatch)))
  void (on policySwitch (Adw.PropertyNotify #active) (const (reportSealChoice seal dispatch)))
  void (on resumeButton #toggled (reportExistingChoice seal dispatch))
  void (on replaceButton #toggled (reportExistingChoice seal dispatch))
  pure seal

-- | The two switches are one choice, so both read both before they report it.
reportSealChoice :: SealControls -> (UiMessage -> IO ()) -> IO ()
reportSealChoice seal dispatch = unlessSuppressed seal.suppress $ do
  sealing <- get seal.sealSwitch #active
  anyway <- get seal.policySwitch #active
  dispatch (SetSealFirst (choiceOf sealing anyway))

-- | Two buttons are one choice. Only the active one reports.
reportExistingChoice :: SealControls -> (UiMessage -> IO ()) -> IO ()
reportExistingChoice seal dispatch = unlessSuppressed seal.suppress $ do
  resume <- get seal.resumeButton #active
  replace <- get seal.replaceButton #active
  if resume then dispatch (SetExistingCopy Resume) else when replace (dispatch (SetExistingCopy Replace))

-- | A render writes the switches with dispatch suppressed, so it never looks like an
-- operator's press.
renderSealChoice :: SealControls -> JobPlan -> IO ()
renderSealChoice seal plan = case plan.spec.job of
  Offload oj -> do
    let sealing = case plan.sealPass of
          Nothing -> False
          Just _ -> True
        anyway = case plan.sealPass of
          Just pass | pass.onFailure == CopyAnyway -> True
          _ -> False
    suppressing seal.suppress $ do
      set seal.sealSwitch [#active := sealing, #subtitle := sealSubtitle plan]
      set seal.policySwitch [#active := anyway, #visible := sealing]
      set seal.resumeButton [#active := oj.existingCopy == Just Resume]
      set seal.replaceButton [#active := oj.existingCopy == Just Replace]
    Gtk.widgetSetVisible seal.existingRow (V.any (\target -> target.state == Partial) plan.targets)
    Gtk.widgetSetVisible seal.group True
  _ -> Gtk.widgetSetVisible seal.group False

choiceOf :: Bool -> Bool -> SealFirst
choiceOf sealing anyway
  | not sealing = UseHistory
  | anyway = SealBeforeCopy CopyAnyway
  | otherwise = SealBeforeCopy StopBeforeCopy

renderPlanSheet :: PlanSheet -> Model -> IO ()
renderPlanSheet widgets current = do
  renderCell widgets.phaseCell current.planPhase
  renderCell widgets.openCell (isOpen current.planPhase)

isOpen :: PlanPhase -> Bool
isOpen phase = case phase of
  Idle -> False
  _ -> True

sealSubtitle :: JobPlan -> Text
sealSubtitle plan = case plan.sealPass of
  Just pass -> "reads " <> humanBytes pass.bytes <> " first, then writes generation " <> count (plan.generations + 1)
  Nothing
    | plan.generations == 0 -> "the media source has no history; sealing records its hashes before a byte is copied"
    | otherwise -> "the media source already holds " <> count plan.generations <> " generations, which the copies are checked against"

-- | A blocker is what stops the job, so it is never below a warning.
orderedFindings :: JobPlan -> Vector Finding
orderedFindings plan = blockers plan <> V.filter (\finding -> finding.severity == Warning) plan.findings

-- | The three rows of the sheet's "This job" group.
summaryOf :: JobPlan -> Vector Row
summaryOf plan =
  V.fromList
    [ plainRow (count (V.length plan.steps) <> " files") (humanBytes plan.totalBytes)
    , plainRow "Hash format" (formatText plan)
    , plainRow "Steps" (stepCounts plan)
    ]

formatText :: JobPlan -> Text
formatText plan = case plan.format of
  Nothing -> "not settled"
  Just fmt -> display (formatAlgo fmt) <> " · originals: " <> plan.originsUsed

-- | The plan's own tally, so the operator reads what will happen and not only how much of it.
stepCounts :: JobPlan -> Text
stepCounts plan =
  plan.steps
    & V.foldr (\step tally -> Map.insertWith (\_ n -> n + 1) (stepName step) (1 :: Int) tally) Map.empty
    & Map.toList
    & map (\pair -> fst pair <> " " <> count (snd pair))
    & T.intercalate " · "

-- | A copy step reads as its heaviest write: an overwrite anywhere names it, else a reuse everywhere, else a copy.
stepName :: PlanStep -> Text
stepName step = case step.op of
  Copy _
    | V.any (\w -> w.mode == Overwrite) step.writes -> display Overwrite
    | not (V.null step.writes) && V.all (\w -> w.mode == Reuse) step.writes -> display Reuse
    | otherwise -> display WriteNew
  VerifyAgainst _ -> "verify"
  ReportNew -> "record"
  ReportMissing -> "missing"

-- | The bound keeps the group at about thirty widgets, whatever the size of the media source.
scriptSample :: Int
scriptSample = 20

-- | What the engine will do, for a developer who reads a plan. The operator never sees this group.
scriptOf :: JobPlan -> Vector Row
scriptOf plan =
  V.fromList
    [ plainRow "Execution" (executionText plan)
    , plainRow "Instant" (formatMhlTime plan.spec.createdAt)
    , plainRow "Bytes" (count plan.bytesToRead <> " to read, " <> count plan.totalBytes <> " in the main pass")
    , plainRow "Creates" (count (V.length plan.creates) <> " directories")
    , plainRow "Directories" (directoriesText plan)
    , plainRow "Ignores" (T.intercalate " · " (V.toList plan.ignorePatterns))
    ]
    <> V.map (\planned -> generationRow planned) (plannedGenerations plan)
    <> V.map (\step -> stepRow step) (V.take scriptSample plan.steps)
    <> overflowRow (V.length plan.steps)

-- | Each planned generation records its own tree, so the row shows one count for each of them.
directoriesText :: JobPlan -> Text
directoriesText plan =
  plannedGenerations plan
    & V.toList
    & map (\planned -> count (V.length planned.directories))
    & T.intercalate " · "

executionText :: JobPlan -> Text
executionText plan = case plan.execution of
  CopyInto copy -> display copy.process <> " · source: " <> pathText copy.source <> " · originals: " <> plan.originsUsed <> carriedText copy.carried
  RecordAt record -> display record.process <> " · folder: " <> pathText record.folder

-- | Nothing when no history is carried, so a plain offload's row reads as before.
carriedText :: Int -> Text
carriedText carried
  | carried <= 0 = ""
  | carried == 1 = " · carries 1 generation"
  | otherwise = " · carries " <> count carried <> " generations"

generationRow :: PlannedGeneration -> Row
generationRow planned =
  plainRow
    (pathText (takeFileName planned.manifest))
    (pathText planned.folder <> " · generation " <> count planned.number <> " · " <> display planned.process)

stepRow :: PlanStep -> Row
stepRow step =
  plainRow
    (display step.path)
    (humanBytes step.size <> " · " <> stepName step <> " · " <> expectationText step.op <> tempText step)

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

overflowRow :: Int -> Vector Row
overflowRow total
  | total <= scriptSample = V.empty
  | otherwise = V.singleton (plainRow ("and " <> count (total - scriptSample) <> " more") "")

targetRow :: Target -> Row
targetRow target =
  plainRow
    (pathText (takeFileName target.root))
    (pathText target.root <> " · " <> freeText target <> " · " <> display target.state)

freeText :: Target -> Text
freeText target = maybe "free space unknown" (\free -> humanBytes free <> " free") target.freeBytes

findingRow :: Finding -> Row
findingRow finding =
  (plainRow (display finding.code) finding.detail)
    { cssClass = Just (case finding.severity of Blocker -> "error"; Warning -> "warning")
    }

-- | A group with no rows says nothing, so this function hides it rather than leaves an empty frame.
renderRows :: Adw.PreferencesGroup -> IORef (Vector Adw.ActionRow) -> Vector Row -> IO ()
renderRows group rowsRef wanted = do
  renderActionRows rowsRef (InGroup group) wanted
  Gtk.widgetSetVisible group (not (V.null wanted))
