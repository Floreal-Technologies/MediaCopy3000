module MediaCopy.Model
  ( FileFilter (..)
  , OffloadDraft (..)
  , draftReady
  , JobEntry (..)
  , Model (..)
  , initialModel
  , PlanPhase (..)
  , UiMessage (..)
  , Message (..)
  , Command (..)
  , update
  , selectedEntry
  , canCancelSelected
  , canReportSelected
  , reviewableSpec
  , hasFinishedJobs
  ) where

import Ascmhl.Types (MhlHistory)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (List)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Optics.Core (ix, (%), (%~), (.~), (?~), _Just)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan (..), planBlocked, planEquivalent)
import MediaCopy.Interface.Theme (Appearance (..), Base, PaletteMode, Theme, setPalette, systemAppearance)
import MediaCopy.Report (renderPlanText, renderReport)

data FileFilter = AllFiles | FailedOnly
  deriving stock (Eq, Show)

data OffloadDraft = OffloadDraft
  { mediaSource :: Maybe OsPath
  , destinations :: List OsPath
  }
  deriving stock (Eq, Generic, Show)

emptyDraft :: OffloadDraft
emptyDraft = OffloadDraft {mediaSource = Nothing, destinations = []}

draftReady :: OffloadDraft -> Bool
draftReady draft = isJust draft.mediaSource && not (null draft.destinations)

data PlanPhase = Idle | Planning JobSpec | Ready JobPlan | PlanError Text
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
    }

data UiMessage
  = PickSource
  | AddDestination
  | RemoveDestination Int
  | OpenOffloadDialog
  | CloseOffloadDialog
  | SetBase Base
  | SetPalette Theme
  | ReviewPlan
  | SetSealFirst SealFirst
  | SetExistingCopy ExistingCopy
  | ConfirmPlan
  | DiscardPlan
  | PickVerifyFolder
  | PickSealFolder
  | CancelSelectedJob
  | ReviewSelectedJob
  | SaveSelectedReport
  | SavePlan
  | SelectJob (Maybe JobId)
  | SelectNextJob
  | SelectPreviousJob
  | ClearFinished
  | SetFileFilter FileFilter
  | RequestClose
  | ConfirmClose
  | CancelClose
  | DismissToast
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
  deriving stock (Eq, Show)

data Command
  = OpenFolderDialog (OsPath -> Message)
  | OpenSaveDialog Text Text (OsPath -> Message)
  | StartJob JobPlan
  | ComputePlan JobSpec
  | CancelRunning JobId
  | LoadHistory JobId OsPath
  | WriteFile OsPath Text
  | CloseWindow

update :: Message -> Model -> (Model, List Command)
update msg model = case msg of
  Ui intent -> updateUi intent model
  SourcePicked p -> (model & #draft % _Just % #mediaSource ?~ p, [])
  DestinationPicked p
    | any (\draft -> p `elem` draft.destinations) model.draft -> (model, [])
    | otherwise ->
        (model & #draft % _Just % #destinations %~ (\dests -> dests <> [p]), [])
  ReportTargetPicked jid p -> case Map.lookup jid model.jobs of
    Nothing -> (model, [])
    Just entry -> (model, [WriteFile p (renderReport entry.state entry.plan entry.history)])
  PlanTargetPicked plan p -> (model, [WriteFile p (renderPlanText plan.spec plan)])
  RequestPlan job ->
    let jid = model.nextId
        spec = JobSpec {jobId = jid, job, createdAt = model.now}
    in (model {nextId = succ jid, planPhase = Planning spec}, [ComputePlan spec])
  PlanComputed spec outcome -> planComputed spec outcome model
  EngineEvent jid ev
    | model.running /= Just jid -> (model, [])
    | otherwise -> case Map.lookup jid model.jobs of
        Nothing -> (model, [])
        Just entry ->
          let model1 = model {jobs = Map.insert jid entry {state = foldEvent model.now ev entry.state} model.jobs}
              refresh = historyRefresh jid entry.state.spec.job ev
          in if isTerminalEvent ev
               then
                 let ended = model1 {running = Nothing, toast = toastMessage entry.state.spec.job ev, closeConfirm = False}
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

updateUi :: UiMessage -> Model -> (Model, List Command)
updateUi msg model = case msg of
  PickSource -> (model, [OpenFolderDialog SourcePicked])
  AddDestination -> (model, [OpenFolderDialog DestinationPicked])
  RemoveDestination i -> (model & #draft % _Just % #destinations %~ (\dests -> deleteAt i dests), [])
  OpenOffloadDialog -> (model {draft = Just emptyDraft}, [])
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
  PickVerifyFolder -> (model, [OpenFolderDialog (\folder -> RequestPlan (VerifyFolder VerifyJob {folder}))])
  PickSealFolder -> (model, [OpenFolderDialog (\folder -> RequestPlan (SealMediaSource SealJob {folder}))])
  ReviewSelectedJob -> case reviewableSpec model of
    Just spec -> (model {planPhase = Planning spec}, [ComputePlan spec])
    Nothing -> (model, [])
  CancelSelectedJob -> case model.selected of
    Nothing -> (model, [])
    Just jid -> cancelJob jid model
  SaveSelectedReport -> saveSelectedReport model
  SavePlan -> case model.planPhase of
    Ready plan -> (model, [OpenSaveDialog "Save Plan" (jobLabel plan.spec.job <> "-plan.txt") (PlanTargetPicked plan)])
    _ -> (model, [])
  SelectJob mjid -> (model {selected = mjid}, [])
  SelectNextJob -> (model {selected = neighbour 1 model}, [])
  SelectPreviousJob -> (model {selected = neighbour (-1) model}, [])
  ClearFinished -> clearFinished model
  SetFileFilter f -> (model {fileFilter = f}, [])
  RequestClose
    | isJust model.running -> (model {closeConfirm = True}, [])
    | otherwise -> (model, [CloseWindow])
  ConfirmClose -> confirmClose model
  CancelClose -> (model {closeConfirm = False}, [])
  DismissToast -> (model {toast = Nothing}, [])

reviewPlan :: Model -> (Model, List Command)
reviewPlan model
  | Just draft <- model.draft
  , Just src <- draft.mediaSource
  , dest : dests <- draft.destinations =
      let job = Offload OffloadJob {source = src, destinations = dest :| dests, sealFirst = UseHistory, existingCopy = Nothing}
      in update (RequestPlan job) model {draft = Nothing}
  | otherwise = (model, [])

saveSelectedReport :: Model -> (Model, List Command)
saveSelectedReport model = case selectedEntry model of
  Nothing -> (model, [])
  Just entry ->
    (model, [OpenSaveDialog "Save Report" (jobLabel entry.state.spec.job <> "-report.txt") (ReportTargetPicked entry.state.spec.jobId)])

clearFinished :: Model -> (Model, List Command)
clearFinished model =
  let kept = Map.filter (\entry -> not (isTerminal entry.state.phase)) model.jobs
      selected' = case model.selected of
        Just jid | Map.member jid kept -> Just jid
        _ -> Nothing
  in (model {jobs = kept, selected = selected'}, [])

confirmClose :: Model -> (Model, List Command)
confirmClose model =
  let stop = maybe [] (\jid -> [CancelRunning jid]) model.running
  in (model {closeConfirm = False, running = Nothing, queue = []}, stop <> [CloseWindow])

setPhase :: JobId -> JobPhase -> Model -> Model
setPhase jid phase model = model & #jobs % ix jid % #state % #phase .~ phase

storePlan :: JobId -> JobPlan -> Model -> Model
storePlan jid fresh model = model & #jobs % ix jid % #plan ?~ fresh

phaseOf :: JobId -> Model -> Maybe JobPhase
phaseOf jid model = Map.lookup jid model.jobs <&> \entry -> entry.state.phase

cancelJob :: JobId -> Model -> (Model, List Command)
cancelJob jid model
  | model.running == Just jid =
      let (model1, cmds) = startNext (setPhase jid Cancelled model {running = Nothing})
      in (model1, CancelRunning jid : cmds)
  | jid `elem` model.queue =
      (setPhase jid Cancelled model {queue = filter (\queued -> queued /= jid) model.queue}, [])
  | phaseOf jid model == Just NeedsReview =
      (setPhase jid Cancelled model {planPhase = closedFor jid model}, [])
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

historyRefresh :: JobId -> Job -> JobEvent -> List Command
historyRefresh jid job ev = case ev of
  JobFinished _ -> historyFolder job & maybe [] (\folder -> [LoadHistory jid folder])
  _ -> []

planComputed :: JobSpec -> Either Text JobPlan -> Model -> (Model, List Command)
planComputed spec outcome model
  | awaitingPlan model spec = case outcome of
      Right plan -> (model {planPhase = Ready plan}, [])
      Left message -> (model {planPhase = PlanError message}, [])
  | model.running == Just spec.jobId = case outcome of
      Right plan -> replanReady spec.jobId plan model
      Left message ->
        startNext
          (setPhase spec.jobId (Failed message) model {running = Nothing, toast = toastMessage spec.job (JobFailed message)})
  | otherwise = (model, [])

awaitingPlan :: Model -> JobSpec -> Bool
awaitingPlan model spec = case model.planPhase of
  Planning pending -> pending == spec
  _ -> False

replanIfChanged :: (OffloadJob -> Bool) -> (OffloadJob -> OffloadJob) -> Model -> (Model, List Command)
replanIfChanged changed apply model = case planningSpec model of
  Just spec
    | Offload oj <- spec.job
    , changed oj ->
        let spec' = spec {job = Offload (apply oj)}
        in (model {planPhase = Planning spec'}, [ComputePlan spec'])
  _ -> (model, [])

offered :: JobPlan -> PlanPhase -> PlanPhase
offered fresh phase = case phase of
  Idle -> Ready fresh
  _ -> phase

closedFor :: JobId -> Model -> PlanPhase
closedFor jid model = case planningSpec model of
  Just spec | spec.jobId == jid -> Idle
  _ -> model.planPhase

planningSpec :: Model -> Maybe JobSpec
planningSpec model = case model.planPhase of
  Planning spec -> Just spec
  Ready plan -> Just plan.spec
  _ -> Nothing

confirmPlan :: JobPlan -> Model -> (Model, List Command)
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

replanReady :: JobId -> JobPlan -> Model -> (Model, List Command)
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

startNext :: Model -> (Model, List Command)
startNext model = case model.running of
  Just _ -> (model, [])
  Nothing -> case model.queue of
    [] -> (model, [])
    j : rest -> case Map.lookup j model.jobs of
      Nothing -> startNext model {queue = rest}
      Just entry -> (model {running = Just j, queue = rest}, [ComputePlan entry.state.spec])

toastMessage :: Job -> JobEvent -> Maybe Text
toastMessage job = \case
  JobFinished AllOk -> Just (label <> ": " <> allOkText (jobKind job))
  JobFinished (WithFailures n) -> Just (label <> ": finished with " <> T.pack (show n) <> " failures")
  JobFailed msg' -> Just (label <> ": failed – " <> msg')
  _ -> Nothing
  where
    label = jobLabel job

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
