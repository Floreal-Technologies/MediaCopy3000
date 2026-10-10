module MediaCopy.Gtk.Widgets.PlanSheet
  ( PlanSheet
  , newPlanSheet
  , renderPlanSheet
  ) where

import Control.Monad (forM_, void, zipWithM_)
import Data.GI.Base
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Domain.PluginCatalog (FieldValue (..), FieldView (..), enabledJobFields)
import MediaCopy.Gtk.Widgets.Bind (bind, checkRadio, closing, dialog, pair, switchRow)
import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), fieldRow)
import MediaCopy.Interface.View.PlanSheet (BodyView (..), PhaseView (..), SealView (..), SheetPage (..), phaseView, sealChoice)
import MediaCopy.Interface.View.Row (RowView)
import MediaCopy.Model

data PlanSheet = PlanSheet
  { phaseCell :: Cell (Maybe PhaseView)
  , paintOpen :: Bool -> IO ()
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
  openControl <- dialog shell.dialog window (pure ())
  paintOpen <- bind openControl (closing (dispatch DiscardPlan))
  fieldsCell <- newCell (renderJobFields body dispatch)
  pure PlanSheet {phaseCell, paintOpen, fieldsCell}

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

renderPhase :: Sheet -> Maybe PhaseView -> IO ()
renderPhase sheet = mapM_ $ \view -> do
  forM_ view.title (\title -> set sheet.dialog [#title := title])
  forM_ view.body (renderReadyBody sheet.body)
  escaped <- GLib.markupEscapeText view.problem (-1)
  set sheet.errorPage [#description := escaped]
  set sheet.startBtn [#sensitive := view.startEnabled]
  set sheet.saveBtn [#sensitive := view.saveEnabled]
  Gtk.stackSetVisibleChildName sheet.stack (sheetPageName view.page)

sheetPageName :: SheetPage -> Text
sheetPageName = \case
  PlanningPage -> "planning"
  ReadyPage -> "ready"
  ErrorPage -> "error"

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

renderReadyBody :: ReadyBody -> BodyView -> IO ()
renderReadyBody body view = do
  renderRows body.summaryGroup body.summaryRows view.summary
  renderRows body.targetGroup body.targetRows view.targets
  renderRows body.findingGroup body.findingRows view.findings
  renderRows body.manifestGroup body.manifestRows view.manifest
  renderActionRows body.scriptRows (InExpander body.scriptExpander) (V.map fromView view.script)
  renderSealChoice body.seal view.seal

data SealControls = SealControls
  { group :: Adw.PreferencesGroup
  , sealSwitch :: Adw.SwitchRow
  , policySwitch :: Adw.SwitchRow
  , existingRow :: Adw.ActionRow
  , paintChoice :: (Bool, Bool) -> IO ()
  , paintExisting :: Maybe ExistingCopy -> IO ()
  }

newSealGroup :: (UiMessage -> IO ()) -> IO SealControls
newSealGroup dispatch = do
  group <- new Adw.PreferencesGroup [#title := "Before Copying"]
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
  paintChoice <-
    bind
      (pair (switchRow sealSwitch) (switchRow policySwitch))
      (\(sealing, anyway) -> dispatch (SetSealFirst (sealChoice sealing anyway)))
  paintExisting <-
    bind
      (checkRadio [(Resume, resumeButton), (Replace, replaceButton)])
      (mapM_ (dispatch . SetExistingCopy))
  pure SealControls {group, sealSwitch, policySwitch, existingRow, paintChoice, paintExisting}

renderSealChoice :: SealControls -> SealView -> IO ()
renderSealChoice seal view = do
  set seal.sealSwitch [#subtitle := view.subtitle]
  set seal.policySwitch [#visible := view.policyShown]
  seal.paintChoice (view.sealing, view.anyway)
  seal.paintExisting view.existing
  Gtk.widgetSetVisible seal.existingRow view.existingShown
  Gtk.widgetSetVisible seal.group view.shown

renderPlanSheet :: PlanSheet -> Model -> IO ()
renderPlanSheet widgets current = do
  renderCell widgets.phaseCell (phaseView current.wording current.planPhase)
  widgets.paintOpen (isOpen current.planPhase)
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
    withValue field value = FieldView {key = field.key, label = field.label, kind = field.kind, required = field.required, value}
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

renderRows :: Adw.PreferencesGroup -> IORef (Vector Adw.ActionRow) -> Vector RowView -> IO ()
renderRows group rowsRef wanted = do
  renderActionRows rowsRef (InGroup group) (V.map fromView wanted)
  Gtk.widgetSetVisible group (not (V.null wanted))
