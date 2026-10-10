module MediaCopy.Model where

import Ascmhl.Path (pathText)
import Ascmhl.Types (MhlHistory)
import Data.Function ((&))
import Data.List (List)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Optics.Core (ix, (%), (%~), (.~), (?~), (^?), _Just)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan (..), planBlocked, planEquivalent)
import MediaCopy.Domain.Plugin (PluginFinding (..), PluginRef (..), PluginReport (..))
import MediaCopy.Domain.PluginCatalog (CatalogChange (..), Edit (..), PluginCatalog, Setting (..), emptyCatalog)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Theme
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Embedded (embeddedWording)
import MediaCopy.Interface.Wording
import MediaCopy.Report (renderPlanText, renderReport)

data FileFilter = AllFiles | FailedOnly
  deriving stock (Eq, Show)

data OffloadDraft = OffloadDraft
  { mediaSource :: Maybe OsPath
  , destinations :: List OsPath
  }
  deriving stock (Eq, Generic, Show)

draftReady :: OffloadDraft -> Bool
draftReady draft = isJust draft.mediaSource && not (null draft.destinations)

data PlanPhase = Idle | Planning JobSpec | Refreshing JobPlan JobSpec | Ready JobPlan | PlanError Text
  deriving stock (Eq, Show)

data JobEntry = JobEntry
  { state :: JobState
  , plan :: Maybe JobPlan
  , history :: Maybe MhlHistory
  }
  deriving stock (Eq, Generic, Show)

data Model = Model
  { jobs :: Map JobId JobEntry
  , queue :: List JobId
  , running :: Maybe JobId
  , selected :: Maybe JobId
  , nextId :: JobId
  , draft :: Maybe OffloadDraft
  , fileFilter :: FileFilter
  , planPhase :: PlanPhase
  , appearance :: Appearance
  , desktopBase :: PaletteMode
  , toast :: Maybe Text
  , now :: UTCTime
  , closeConfirm :: Bool
  , wording :: Wording
  , plugins :: PluginCatalog
  , pluginToasts :: Set (JobId, Text)
  , palette :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

initialModel :: UTCTime -> PaletteMode -> Model
initialModel t desktop =
  Model
    { jobs = Map.empty
    , queue = []
    , running = Nothing
    , selected = Nothing
    , nextId = JobId 1
    , draft = Nothing
    , fileFilter = AllFiles
    , planPhase = Idle
    , appearance = systemAppearance
    , desktopBase = desktop
    , toast = Nothing
    , now = t
    , closeConfirm = False
    , wording = embeddedWording English
    , plugins = emptyCatalog
    , pluginToasts = Set.empty
    , palette = Nothing
    }

data UiMessage
  = PickSource
  | AddDestination
  | RemoveDestination Int
  | CloseOffloadDialog
  | SetBase Base
  | SetPalette Theme
  | ReviewPlan
  | SetSealFirst SealFirst
  | SetExistingCopy ExistingCopy
  | ConfirmPlan
  | DiscardPlan
  | SavePlan
  | SelectJob (Maybe JobId)
  | SetFileFilter FileFilter
  | RequestClose
  | ConfirmClose
  | CancelClose
  | DismissToast
  | ChangePlugin CatalogChange
  | PickPluginPath Text Text
  | OpenPluginFolder Text
  | ReloadPlugins
  | SetJobField Text Text Text
  | PickJobFieldPath Text Text
  | ClosePalette
  | SetPaletteQuery Text
  | RunCommand Command.Command
  deriving stock (Eq, Show)

data Message
  = Ui UiMessage
  | SourcePicked OsPath
  | DestinationPicked OsPath
  | ReportTargetPicked JobId OsPath
  | PlanTargetPicked JobPlan OsPath
  | RequestPlan Job
  | PlanComputed JobSpec (Either Text JobPlan)
  | EngineEvent JobId JobEvent
  | HistoryLoaded JobId MhlHistory
  | Tick UTCTime
  | DesktopBase PaletteMode
  | ShowToast Text
  | WordingReloaded Wording
  | CatalogLoaded PluginCatalog
  deriving stock (Eq, Show)

data DialogKind = PickFolder | PickFile | SaveAs Text Text

data AppEffect
  = OpenDialog DialogKind (OsPath -> Message)
  | StartJob JobPlan
  | ComputePlan JobSpec
  | CancelRunning JobId
  | LoadHistory JobId OsPath
  | WriteFile OsPath Text
  | DestroyWindow
  | LoadCatalog
  | ApplyChange CatalogChange
  | ShowFolder Text
  | ShowChrome Chrome

data Scope = App | Win | Builtin Text
  deriving stock (Eq, Show)

data PreferencesPage = GeneralPreferences | PluginPreferences
  deriving stock (Eq, Show)

data Chrome = ShowPreferences PreferencesPage | ShowAbout | ShowShortcuts
  deriving stock (Eq, Show)

data CommandSpec = CommandSpec
  { scope :: Scope
  , accels :: List Text
  , section :: Maybe Command.Section
  , menu :: Maybe Command.Section
  , enabled :: Model -> Bool
  }

commandSpec :: Command.Command -> CommandSpec
commandSpec = \case
  Command.NewOffload -> CommandSpec Win ["<Control>n"] (Just Command.JobsSection) Nothing (const True)
  Command.VerifyFolder -> CommandSpec Win ["<Control>o"] (Just Command.JobsSection) Nothing (const True)
  Command.SealMedia -> CommandSpec Win ["<Control>l"] (Just Command.JobsSection) Nothing (const True)
  Command.SaveReport -> CommandSpec Win ["<Control>s"] (Just Command.JobsSection) Nothing canReportSelected
  Command.CancelJob -> CommandSpec Win [] Nothing Nothing canCancelSelected
  Command.ReviewJob -> CommandSpec Win [] Nothing Nothing (isJust . reviewableSpec)
  Command.ClearFinished -> CommandSpec Win [] Nothing (Just Command.JobsSection) hasFinishedJobs
  Command.NextJob -> CommandSpec Win ["<Control>Page_Down"] (Just Command.NavigationSection) Nothing (const True)
  Command.PreviousJob -> CommandSpec Win ["<Control>Page_Up"] (Just Command.NavigationSection) Nothing (const True)
  Command.CommandPalette -> CommandSpec Win ["<Control>k"] (Just Command.GeneralSection) (Just Command.GeneralSection) (const True)
  Command.Preferences -> CommandSpec App ["<Control>comma"] (Just Command.GeneralSection) (Just Command.GeneralSection) (const True)
  Command.Plugins -> CommandSpec App [] Nothing (Just Command.GeneralSection) (const True)
  Command.KeyboardShortcuts -> CommandSpec (Builtin "win.show-help-overlay") ["<Control>question"] (Just Command.GeneralSection) (Just Command.GeneralSection) (const True)
  Command.About -> CommandSpec App [] Nothing (Just Command.GeneralSection) (const True)
  Command.CloseWindow -> CommandSpec Win ["<Control>w"] (Just Command.GeneralSection) Nothing (const True)
  Command.Quit -> CommandSpec App ["<Control>q"] (Just Command.GeneralSection) Nothing (const True)

commandEnabled :: Model -> Command.Command -> Bool
commandEnabled model command = (commandSpec command).enabled model

update :: Message -> Model -> (Model, List AppEffect)
update msg model = case msg of
  Ui intent -> updateUi intent model
  SourcePicked p -> (model & #draft % _Just % #mediaSource ?~ p, [])
  DestinationPicked p
    | any (\draft -> p `elem` draft.destinations) model.draft -> (model, [])
    | otherwise ->
        (model & #draft % _Just % #destinations %~ (\dests -> dests <> [p]), [])
  ReportTargetPicked jid p -> case Map.lookup jid model.jobs of
    Nothing -> (model, [])
    Just entry -> (model, [WriteFile p (renderReport model.wording entry.state entry.plan entry.history)])
  PlanTargetPicked plan p -> (model, [WriteFile p (renderPlanText plan.spec plan)])
  RequestPlan job ->
    let jid = model.nextId
        spec = JobSpec {jobId = jid, job, createdAt = model.now, pluginFields = Map.empty}
    in (model {nextId = succ jid, planPhase = Planning spec}, [ComputePlan spec])
  PlanComputed spec outcome -> planComputed spec outcome model
  EngineEvent jid ev
    | model.running /= Just jid -> (model, [])
    | otherwise -> case Map.lookup jid model.jobs of
        Nothing -> (model, [])
        Just entry ->
          let model1 = pluginToast jid entry.state.spec.job ev model {jobs = Map.insert jid entry {state = foldEvent model.now ev entry.state} model.jobs}
              refresh = historyRefresh jid entry.state.spec.job ev
          in if isTerminalEvent ev
               then
                 let ended =
                       model1
                         { running = Nothing
                         , toast = toastMessage model.wording entry.state.spec.job ev
                         , closeConfirm = False
                         }
                     (model2, cmds) = startNext ended
                 in (model2, refresh <> cmds)
               else (model1, [])
  HistoryLoaded jid hist -> case Map.lookup jid model.jobs of
    Nothing -> (model, [])
    Just entry -> (model {jobs = Map.insert jid entry {history = Just hist} model.jobs}, [])
  Tick t -> (model {now = t}, [])
  DesktopBase wanted
    | wanted == model.desktopBase -> (model, [])
    | otherwise -> (model {desktopBase = wanted}, [])
  ShowToast message -> (model {toast = Just message}, [])
  WordingReloaded wording -> (model {wording}, [])
  CatalogLoaded catalog -> (model & #plugins .~ catalog, [])

updateUi :: UiMessage -> Model -> (Model, List AppEffect)
updateUi msg model = case msg of
  PickSource -> (model, [OpenDialog PickFolder SourcePicked])
  AddDestination -> (model, [OpenDialog PickFolder DestinationPicked])
  RemoveDestination i -> (model & #draft % _Just % #destinations %~ deleteAt i, [])
  CloseOffloadDialog -> (model {draft = Nothing}, [])
  SetBase wanted -> (model {appearance = model.appearance {base = wanted}}, [])
  SetPalette wanted -> (model {appearance = setPalette wanted model.appearance}, [])
  ReviewPlan -> reviewPlan model
  SetSealFirst choice -> replanIfChanged (\oj -> oj.sealFirst /= choice) (\oj -> oj {sealFirst = choice}) model
  SetExistingCopy choice -> replanIfChanged (\oj -> oj.existingCopy /= Just choice) (\oj -> oj {existingCopy = Just choice}) model
  ConfirmPlan -> case model.planPhase of
    Ready plan | not (planBlocked plan) -> confirmPlan plan model
    _ -> (model, [])
  DiscardPlan -> (model {planPhase = Idle}, [])
  SavePlan -> case model.planPhase of
    Ready plan -> (model, [OpenDialog (SaveAs (savePlanTitle model.wording) (jobLabel plan.spec.job <> "-plan.txt")) (PlanTargetPicked plan)])
    _ -> (model, [])
  SelectJob mjid -> (model {selected = mjid}, [])
  SetFileFilter f -> (model {fileFilter = f}, [])
  RequestClose
    | isJust model.running -> (model {closeConfirm = True}, [])
    | otherwise -> (model, [DestroyWindow])
  ConfirmClose -> confirmClose model
  CancelClose -> (model {closeConfirm = False}, [])
  DismissToast -> (model {toast = Nothing}, [])
  ChangePlugin change -> (model, [ApplyChange change])
  PickPluginPath pluginId key -> (model, [OpenDialog PickFile (\path -> pathText path & SettingText & SetSetting key & CatalogChange pluginId & ChangePlugin & Ui)])
  OpenPluginFolder folder -> (model, [ShowFolder folder])
  ReloadPlugins -> (model, [LoadCatalog])
  SetJobField pluginId key value -> setJobField pluginId key value model
  PickJobFieldPath pluginId key -> (model, [OpenDialog PickFile (\path -> pathText path & SetJobField pluginId key & Ui)])
  ClosePalette -> (model {palette = Nothing}, [])
  SetPaletteQuery query -> (model {palette = query <$ model.palette}, [])
  RunCommand command
    | commandEnabled model command -> runCommand command model {palette = Nothing}
    | otherwise -> (model, [])

runCommand :: Command.Command -> Model -> (Model, List AppEffect)
runCommand command model = case command of
  Command.NewOffload -> (model {draft = Just OffloadDraft {mediaSource = Nothing, destinations = []}}, [])
  Command.VerifyFolder -> (model, [OpenDialog PickFolder (\folder -> RequestPlan (VerifyFolder VerifyJob {folder}))])
  Command.SealMedia -> (model, [OpenDialog PickFolder (\folder -> RequestPlan (SealMediaSource SealJob {folder}))])
  Command.SaveReport -> saveSelectedReport model
  Command.CancelJob -> case model.selected of
    Nothing -> (model, [])
    Just jid -> cancelJob jid model
  Command.ReviewJob -> case reviewableSpec model of
    Just spec -> (model {planPhase = Planning spec}, [ComputePlan spec])
    Nothing -> (model, [])
  Command.ClearFinished -> clearFinished model
  Command.NextJob -> (model {selected = neighbour 1 model}, [])
  Command.PreviousJob -> (model {selected = neighbour (-1) model}, [])
  Command.CommandPalette -> (model {palette = Just ""}, [])
  Command.Preferences -> (model, [ShowChrome (ShowPreferences GeneralPreferences)])
  Command.Plugins -> (model, [ShowChrome (ShowPreferences PluginPreferences)])
  Command.KeyboardShortcuts -> (model, [ShowChrome ShowShortcuts])
  Command.About -> (model, [ShowChrome ShowAbout])
  Command.CloseWindow -> updateUi RequestClose model
  Command.Quit -> updateUi RequestClose model

reviewPlan :: Model -> (Model, List AppEffect)
reviewPlan model
  | Just draft <- model.draft
  , Just src <- draft.mediaSource
  , dest : dests <- draft.destinations =
      let job = Offload OffloadJob {source = src, destinations = dest :| dests, sealFirst = UseHistory, existingCopy = Nothing}
      in update (RequestPlan job) model {draft = Nothing}
  | otherwise = (model, [])

saveSelectedReport :: Model -> (Model, List AppEffect)
saveSelectedReport model = case selectedEntry model of
  Nothing -> (model, [])
  Just entry ->
    ( model
    ,
      [ OpenDialog
          (SaveAs (saveReportTitle model.wording) (jobLabel entry.state.spec.job <> "-report.txt"))
          (ReportTargetPicked entry.state.spec.jobId)
      ]
    )

clearFinished :: Model -> (Model, List AppEffect)
clearFinished model =
  let kept = Map.filter (\entry -> not (isTerminal entry.state.phase)) model.jobs
      selected' = case model.selected of
        Just jid | Map.member jid kept -> Just jid
        _ -> Nothing
      toasts = Set.filter (\(jid, _) -> Map.member jid kept) model.pluginToasts
  in (model {jobs = kept, selected = selected', pluginToasts = toasts}, [])

confirmClose :: Model -> (Model, List AppEffect)
confirmClose model =
  let stop = maybe [] (\jid -> [CancelRunning jid]) model.running
  in (model {closeConfirm = False, running = Nothing, queue = []}, stop <> [DestroyWindow])

setPhase :: JobId -> JobPhase -> Model -> Model
setPhase jid phase model = model & #jobs % ix jid % #state % #phase .~ phase

storePlan :: JobId -> JobPlan -> Model -> Model
storePlan jid fresh model = model & #jobs % ix jid % #plan ?~ fresh

cancelJob :: JobId -> Model -> (Model, List AppEffect)
cancelJob jid model
  | model.running == Just jid =
      let (model1, cmds) = startNext (setPhase jid Cancelled model {running = Nothing})
      in (model1, CancelRunning jid : cmds)
  | jid `elem` model.queue =
      (setPhase jid Cancelled model {queue = filter (\queued -> queued /= jid) model.queue}, [])
  | model ^? #jobs % ix jid % #state % #phase == Just NeedsReview =
      let closed = case planningSpec model of
            Just spec | spec.jobId == jid -> Idle
            _ -> model.planPhase
      in (setPhase jid Cancelled model {planPhase = closed}, [])
  | otherwise = (model, [])

neighbour :: Int -> Model -> Maybe JobId
neighbour offset model =
  let ordered = Map.keys model.jobs & V.fromList
  in if V.null ordered
       then Nothing
       else case model.selected of
         Nothing ->
           if offset > 0
             then ordered V.!? 0
             else ordered V.!? (V.length ordered - 1)
         Just jid -> case V.elemIndex jid ordered of
           Nothing -> ordered V.!? 0
           Just index -> case ordered V.!? (index + offset) of
             Nothing -> Just jid
             Just next -> Just next

historyRefresh :: JobId -> Job -> JobEvent -> List AppEffect
historyRefresh jid job ev = case ev of
  JobFinished _ -> historyFolder job & maybe [] (\folder -> [LoadHistory jid folder])
  _ -> []

planComputed :: JobSpec -> Either Text JobPlan -> Model -> (Model, List AppEffect)
planComputed spec outcome model
  | awaitingPlan model spec = case outcome of
      Right plan -> (model {planPhase = Ready plan}, [])
      Left message -> (model {planPhase = PlanError message}, [])
  | model.running == Just spec.jobId = case outcome of
      Right plan -> replanReady spec.jobId plan model
      Left message ->
        startNext
          ( setPhase
              spec.jobId
              (Failed message)
              ( model
                  { running = Nothing
                  , toast = toastMessage model.wording spec.job (JobFailed message)
                  }
              )
          )
  | otherwise = (model, [])

awaitingPlan :: Model -> JobSpec -> Bool
awaitingPlan model spec = case model.planPhase of
  Planning pending -> pending == spec
  Refreshing _ pending -> pending == spec
  _ -> False

replanIfChanged :: (OffloadJob -> Bool) -> (OffloadJob -> OffloadJob) -> Model -> (Model, List AppEffect)
replanIfChanged changed apply model = case planningSpec model of
  Just spec
    | Offload oj <- spec.job
    , changed oj ->
        let spec' = spec {job = Offload (apply oj)}
        in (model {planPhase = Planning spec'}, [ComputePlan spec'])
  _ -> (model, [])

pluginToast :: JobId -> Job -> JobEvent -> Model -> Model
pluginToast jid job ev model = case ev of
  PluginReported (Warned finding)
    | not (Set.member (jid, finding.plugin.id) model.pluginToasts) ->
        model
          { toast = Just (pluginToastText model.wording (jobLabel job) finding)
          , pluginToasts = Set.insert (jid, finding.plugin.id) model.pluginToasts
          }
  _ -> model

setJobField :: Text -> Text -> Text -> Model -> (Model, List AppEffect)
setJobField pluginId key value model = case planningSpec model of
  Just spec
    | fields /= spec.pluginFields ->
        let spec' = spec {pluginFields = fields}
            phase = case model.planPhase of
              Ready shown -> Refreshing shown spec'
              Refreshing shown _ -> Refreshing shown spec'
              _ -> Planning spec'
        in (model {planPhase = phase}, [ComputePlan spec'])
    where
      fields
        | T.null value = Map.filter (not . Map.null) (Map.adjust (Map.delete key) pluginId spec.pluginFields)
        | otherwise = Map.insertWith Map.union pluginId (Map.singleton key value) spec.pluginFields
  _ -> (model, [])

offered :: JobPlan -> PlanPhase -> PlanPhase
offered fresh phase = case phase of
  Idle -> Ready fresh
  _ -> phase

planningSpec :: Model -> Maybe JobSpec
planningSpec model = case model.planPhase of
  Planning spec -> Just spec
  Refreshing _ spec -> Just spec
  Ready plan -> Just plan.spec
  _ -> Nothing

confirmPlan :: JobPlan -> Model -> (Model, List AppEffect)
confirmPlan plan model =
  let jid = plan.spec.jobId
      idle = isNothing model.running && null model.queue
      model1 =
        model
          { jobs = Map.insert jid JobEntry {state = newJobState plan.spec, plan = Just plan, history = Nothing} model.jobs
          , selected = Just jid
          , planPhase = Idle
          }
      refresh = case plan.spec.job of
        VerifyFolder vj -> [LoadHistory jid vj.folder]
        _ -> []
  in if idle
       then
         (setPhase jid Running model1 {running = Just jid}, refresh <> [StartJob plan])
       else (model1 {queue = model1.queue <> [jid]}, refresh)

replanReady :: JobId -> JobPlan -> Model -> (Model, List AppEffect)
replanReady jid fresh model
  | model.running /= Just jid = (model, [])
  | otherwise = case Map.lookup jid model.jobs >>= \entry -> entry.plan of
      Nothing -> (model, [])
      Just stored
        | planEquivalent stored fresh ->
            (setPhase jid Running (storePlan jid fresh model), [StartJob fresh])
        | otherwise ->
            startNext
              ( setPhase
                  jid
                  NeedsReview
                  (storePlan jid fresh model) {running = Nothing, planPhase = offered fresh model.planPhase}
              )

startNext :: Model -> (Model, List AppEffect)
startNext model = case model.running of
  Just _ -> (model, [])
  Nothing -> case model.queue of
    [] -> (model, [])
    j : rest -> case Map.lookup j model.jobs of
      Nothing -> startNext model {queue = rest}
      Just entry -> (model {running = Just j, queue = rest}, [ComputePlan entry.state.spec])

toastMessage :: Wording -> Job -> JobEvent -> Maybe Text
toastMessage wording job = \case
  JobFinished result -> Just (jobFinishedToast wording (jobLabel job) (jobKind job) result)
  JobFailed msg' -> Just (jobFailedToast wording (jobLabel job) msg')
  _ -> Nothing

deleteAt :: Int -> List a -> List a
deleteAt i xs
  | i < 0 = xs
  | otherwise = case splitAt i xs of
      (before, _ : after) -> before <> after
      (before, []) -> before

selectedEntry :: Model -> Maybe JobEntry
selectedEntry model = do
  jid <- model.selected
  Map.lookup jid model.jobs

canCancelSelected :: Model -> Bool
canCancelSelected model = case selectedEntry model of
  Nothing -> False
  Just entry -> case entry.state.phase of
    (Queued; Running; NeedsReview) -> True
    _ -> False

reviewableSpec :: Model -> Maybe JobSpec
reviewableSpec model = case (model.planPhase, selectedEntry model) of
  (Idle, Just entry) | entry.state.phase == NeedsReview -> Just entry.state.spec
  _ -> Nothing

canReportSelected :: Model -> Bool
canReportSelected model = case selectedEntry model of
  Nothing -> False
  Just entry -> isTerminal entry.state.phase

hasFinishedJobs :: Model -> Bool
hasFinishedJobs model = Map.elems model.jobs & any (\entry -> isTerminal entry.state.phase)
