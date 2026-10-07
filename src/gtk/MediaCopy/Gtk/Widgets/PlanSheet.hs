module MediaCopy.Gtk.Widgets.PlanSheet
  ( PlanSheet
  , newPlanSheet
  , renderPlanSheet
  ) where

import Ascmhl.Path (pathText)
import Ascmhl.Types (Author (..))
import Ascmhl.Write (formatMhlTime)
import Control.Monad (void, when, zipWithM_)
import Data.Function ((&))
import Data.GI.Base
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk
import System.OsPath (takeFileName)

import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (formatAlgo)
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Domain.PluginCatalog (FieldValue (..), FieldView (..), enabledJobFields)
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), fieldRow)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording
import MediaCopy.Model

data PlanSheet = PlanSheet
  { phaseCell :: Cell (Wording, PlanPhase)
  , openCell :: Cell Bool
  , fieldsCell :: Cell (Vector (PluginRef, Vector FieldView), Map.Map Text (Map.Map Text Text))
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
  fieldsCell <- newCell (renderJobFields body dispatch)
  onDialogClosed shell.dialog openCell (dispatch DiscardPlan)
  pure PlanSheet {phaseCell, openCell, fieldsCell}

data Sheet = Sheet
  { dialog :: Adw.Dialog
  , startBtn :: Gtk.Button
  , saveBtn :: Gtk.Button
  , stack :: Gtk.Stack
  , errorPage :: Adw.StatusPage
  , body :: ReadyBody
  }

newStackPages :: Gtk.ScrolledWindow -> IO (Gtk.Stack, Adw.StatusPage)
newStackPages ready = do
  stack <- new Gtk.Stack []
  planningPage <- new Adw.StatusPage [#title := "Reading the Folder…", #description := "No file has been changed"]
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

renderPhase :: Sheet -> (Wording, PlanPhase) -> IO ()
renderPhase sheet (wording, phase) = case phase of
  Idle -> pure ()
  Planning spec -> do
    set sheet.dialog [#title := "Plan · " <> jobLabel spec.job]
    set sheet.startBtn [#sensitive := False]
    set sheet.saveBtn [#sensitive := False]
    Gtk.stackSetVisibleChildName sheet.stack "planning"
  PlanError message -> do
    escaped <- GLib.markupEscapeText message (-1)
    set sheet.errorPage [#description := escaped]
    set sheet.startBtn [#sensitive := False]
    set sheet.saveBtn [#sensitive := False]
    Gtk.stackSetVisibleChildName sheet.stack "error"
  Refreshing plan _ -> do
    renderReadyBody (wording, sheet.body) plan
    set sheet.startBtn [#sensitive := False]
    set sheet.saveBtn [#sensitive := False]
    Gtk.stackSetVisibleChildName sheet.stack "ready"
  Ready plan -> do
    set sheet.dialog [#title := "Plan · " <> jobLabel plan.spec.job]
    renderReadyBody (wording, sheet.body) plan
    set sheet.startBtn [#sensitive := not (planBlocked plan)]
    set sheet.saveBtn [#sensitive := True]
    Gtk.stackSetVisibleChildName sheet.stack "ready"

data ReadyBody = ReadyBody
  { scroll :: Gtk.ScrolledWindow
  , summaryGroup :: Adw.PreferencesGroup
  , summaryRows :: IORef (Vector Adw.ActionRow)
  , targetGroup :: Adw.PreferencesGroup
  , targetRows :: IORef (Vector Adw.ActionRow)
  , findingGroup :: Adw.PreferencesGroup
  , findingRows :: IORef (Vector Adw.ActionRow)
  , manifestGroup :: Adw.PreferencesGroup
  , manifestRows :: IORef (Vector Adw.ActionRow)
  , fieldGroup :: Adw.PreferencesGroup
  , fieldRows :: IORef (Maybe (List (PluginRef, FieldView), List FieldRow))
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
  manifestGroup <- new Adw.PreferencesGroup [#title := "Recorded in the Manifest", #description := "Plug-ins add this to each manifest the job writes"]
  Gtk.boxAppend readyBox summaryGroup
  Gtk.boxAppend readyBox seal.group
  Gtk.boxAppend readyBox targetGroup
  fieldGroup <- new Adw.PreferencesGroup [#title := "Job Fields", #description := "Values that plug-ins ask for this job", #visible := False]
  Gtk.boxAppend readyBox findingGroup
  Gtk.boxAppend readyBox fieldGroup
  Gtk.boxAppend readyBox manifestGroup
  scriptGroup <- new Adw.PreferencesGroup [#title := "Details"]
  scriptExpander <- new Adw.ExpanderRow [#title := "Steps", #expanded := False]
  Adw.preferencesGroupAdd scriptGroup scriptExpander
  Gtk.boxAppend readyBox scriptGroup
  scroll <- new Gtk.ScrolledWindow [#child := readyBox, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
  summaryRows <- newIORef V.empty
  targetRows <- newIORef V.empty
  findingRows <- newIORef V.empty
  manifestRows <- newIORef V.empty
  fieldRows <- newIORef Nothing
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
      , manifestGroup
      , manifestRows
      , fieldGroup
      , fieldRows
      , scriptExpander
      , scriptRows
      , seal
      }

renderReadyBody :: (Wording, ReadyBody) -> JobPlan -> IO ()
renderReadyBody (wording, body) plan = do
  renderRows body.summaryGroup body.summaryRows (summaryOf wording plan)
  renderRows body.targetGroup body.targetRows (V.map (targetRow wording) plan.targets)
  renderRows body.findingGroup body.findingRows (findingRowsOf wording plan)
  renderRows body.manifestGroup body.manifestRows (manifestRowsOf plan.plugins.contributions)
  renderActionRows body.scriptRows (InExpander body.scriptExpander) (scriptOf wording plan)
  renderSealChoice body.seal wording plan

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

reportSealChoice :: SealControls -> (UiMessage -> IO ()) -> IO ()
reportSealChoice seal dispatch = unlessSuppressed seal.suppress $ do
  sealing <- get seal.sealSwitch #active
  anyway <- get seal.policySwitch #active
  dispatch (SetSealFirst (choiceOf sealing anyway))

reportExistingChoice :: SealControls -> (UiMessage -> IO ()) -> IO ()
reportExistingChoice seal dispatch = unlessSuppressed seal.suppress $ do
  resume <- get seal.resumeButton #active
  replace <- get seal.replaceButton #active
  if resume then dispatch (SetExistingCopy Resume) else when replace (dispatch (SetExistingCopy Replace))

renderSealChoice :: SealControls -> Wording -> JobPlan -> IO ()
renderSealChoice seal wording plan = case plan.spec.job of
  Offload oj -> do
    let sealing = case plan.sealPass of
          Nothing -> False
          Just _ -> True
        anyway = case plan.sealPass of
          Just pass | pass.onFailure == CopyAnyway -> True
          _ -> False
    suppressing seal.suppress $ do
      set seal.sealSwitch [#active := sealing, #subtitle := sealSubtitle wording plan]
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
  renderCell widgets.phaseCell (current.wording, current.planPhase)
  renderCell widgets.openCell (isOpen current.planPhase)
  let given = case current.planPhase of
        Planning spec -> spec.pluginFields
        Refreshing _ spec -> spec.pluginFields
        Ready plan -> plan.spec.pluginFields
        _ -> Map.empty
  renderCell widgets.fieldsCell (enabledJobFields current.plugins, given)

renderJobFields :: ReadyBody -> (UiMessage -> IO ()) -> (Vector (PluginRef, Vector FieldView), Map.Map Text (Map.Map Text Text)) -> IO ()
renderJobFields body dispatch (wanted, given) = do
  let fields = [(ref, field) | (ref, plugin) <- V.toList wanted, field <- V.toList plugin]
      layout = [(ref, withValue field NoValue) | (ref, field) <- fields]
  readIORef body.fieldRows >>= \case
    Just (shown, rows) | shown == layout -> zipWithM_ (\row (ref, field) -> row.refresh (current ref field)) rows fields
    previous -> do
      forM_ previous (mapM_ (mapM_ (Adw.preferencesGroupRemove body.fieldGroup) . (.rows)) . snd)
      rows <- traverse (uncurry jobFieldRow) fields
      forM_ rows (mapM_ (Adw.preferencesGroupAdd body.fieldGroup) . (.rows))
      writeIORef body.fieldRows (Just (layout, rows))
      Gtk.widgetSetVisible body.fieldGroup (not (null rows))
  where
    withValue field value = FieldView {key = field.key, label = field.label, shape = field.shape, required = field.required, value}
    current ref field = maybe field.value Value (Map.lookup ref.id given >>= Map.lookup field.key)
    jobFieldRow ref field =
      let send value = dispatch (SetJobField ref.id field.key value)
      in fieldRow
           FieldActions
             { setText = send
             , setBool = \flag -> send (if flag then "true" else "false")
             , clear = send ""
             , pickPath = dispatch (PickJobFieldPath ref.id field.key)
             }
           (ref.name <> ": ")
           (withValue field (current ref field))

isOpen :: PlanPhase -> Bool
isOpen phase = case phase of
  Idle -> False
  _ -> True

sealSubtitle :: Wording -> JobPlan -> Text
sealSubtitle wording plan = case plan.sealPass of
  Just pass -> "reads " <> humanBytes wording pass.bytes <> " first, then writes generation " <> count (plan.generations + 1)
  Nothing
    | plan.generations == 0 -> "the media source has no history; sealing records its hashes before a byte is copied"
    | otherwise -> "the media source already holds " <> plural "generation" plan.generations <> ", which the copies are checked against"

findingRowsOf :: Wording -> JobPlan -> Vector Row
findingRowsOf wording plan =
  V.map (findingRow wording) (blockers plan)
    <> V.map (pluginFindingRow wording) (pluginBlockers plan.plugins)
    <> V.map (findingRow wording) (V.filter (\finding -> finding.severity == Warning) plan.findings)
    <> V.map (pluginFindingRow wording) (V.filter (\finding -> finding.severity == Warning) plan.plugins.findings)

pluginFindingRow :: Wording -> PluginFinding -> Row
pluginFindingRow wording finding =
  let (title, detail) = pluginFindingTexts wording finding
  in (plainRow title detail) {cssClass = Just (case finding.severity of Blocker -> "error"; Warning -> "warning")}

manifestRowsOf :: Contributions -> Vector Row
manifestRowsOf contributions =
  V.map authorRow contributions.authors <> metadataRow
  where
    authorRow author = plainRow author.name (T.intercalate " · " (catMaybes [author.role, author.email, author.phone]))
    metadataRow
      | Map.null contributions.fileMetadata && isNothing contributions.manifestMetadata = V.empty
      | otherwise =
          V.singleton
            ( plainRow
                ("Metadata for " <> plural "file" (Map.size contributions.fileMetadata) <> (if isJust contributions.manifestMetadata then " and the manifest" else ""))
                (T.intercalate ", " (V.toList (V.map (\ref -> ref.name) contributions.contributors)))
            )

summaryOf :: Wording -> JobPlan -> Vector Row
summaryOf wording plan =
  V.fromList
    [ plainRow (plural "file" (V.length plan.steps)) (humanBytes wording plan.totalBytes)
    , plainRow "Hash format" (formatText plan)
    , plainRow "Steps" (stepCounts wording plan)
    ]

formatText :: JobPlan -> Text
formatText plan = case plan.format of
  Nothing -> "not settled"
  Just fmt -> display (formatAlgo fmt) <> " · originals: " <> plan.originsUsed

stepCounts :: Wording -> JobPlan -> Text
stepCounts wording plan =
  plan.steps
    & V.foldr (\step tally -> Map.insertWith (\_ n -> n + 1) (stepName wording step) (1 :: Int) tally) Map.empty
    & Map.toList
    & map (\pair -> fst pair <> " " <> count (snd pair))
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

scriptOf :: Wording -> JobPlan -> Vector Row
scriptOf wording plan =
  V.fromList
    [ plainRow "Execution" (executionText wording plan)
    , plainRow "Instant" (formatMhlTime plan.spec.createdAt)
    , plainRow "Bytes" (count plan.bytesToRead <> " to read, " <> count plan.totalBytes <> " in the main pass")
    , plainRow "Creates" (count (V.length plan.creates) <> " directories")
    , plainRow "Directories" (directoriesText plan)
    , plainRow "Ignores" (T.intercalate " · " (V.toList plan.ignorePatterns))
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
  CopyInto copy -> processKindText wording copy.process <> " · source: " <> pathText copy.source <> " · originals: " <> plan.originsUsed <> carriedText copy.carried
  RecordAt record -> processKindText wording record.process <> " · folder: " <> pathText record.folder

carriedText :: Int -> Text
carriedText carried
  | carried <= 0 = ""
  | otherwise = " · carries " <> plural "generation" carried

generationRow :: Wording -> PlannedGeneration -> Row
generationRow wording planned =
  plainRow
    (pathText (takeFileName planned.manifest))
    (pathText planned.folder <> " · generation " <> count planned.number <> " · " <> processKindText wording planned.process)

stepRow :: Wording -> PlanStep -> Row
stepRow wording step =
  plainRow
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

overflowRow :: Int -> Vector Row
overflowRow total
  | total <= scriptSample = V.empty
  | otherwise = V.singleton (plainRow ("and " <> count (total - scriptSample) <> " more") "")

targetRow :: Wording -> Target -> Row
targetRow wording target =
  plainRow
    (pathText (takeFileName target.root))
    (pathText target.root <> " · " <> freeText wording target <> " · " <> targetStateText wording target.state)

freeText :: Wording -> Target -> Text
freeText wording target = maybe "free space unknown" (\free -> humanBytes wording free <> " free") target.freeBytes

findingRow :: Wording -> Finding -> Row
findingRow wording finding =
  (plainRow (findingText wording finding.code) finding.detail)
    { cssClass = Just (case finding.severity of Blocker -> "error"; Warning -> "warning")
    }

renderRows :: Adw.PreferencesGroup -> IORef (Vector Adw.ActionRow) -> Vector Row -> IO ()
renderRows group rowsRef wanted = do
  renderActionRows rowsRef (InGroup group) wanted
  Gtk.widgetSetVisible group (not (V.null wanted))
