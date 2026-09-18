module MediaCopy.ModelTest (tests) where

import Data.Function ((&))
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Hedgehog (Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Demo.Fixtures
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan (..))
import MediaCopy.Interface.Theme (PaletteMode (..))
import MediaCopy.Model

tests :: TestTree
tests =
  testGroup
    "Model"
    [ testGroup
        "enqueue"
        [ testCase "starts the first job immediately and resets the draft" startsFirstJobImmediatelyAndResetsDraft
        , testCase "queues the second job" queuesTheSecondJob
        , testCase "starts the next job when the running one finishes" startsTheNextJobWhenTheRunningOneFinishes
        , testProperty "never has more than one running job" neverHasMoreThanOneRunningJob
        ]
    , testGroup
        "cancel"
        [ testCase "cancels the running job and starts the next" cancelsTheRunningJobAndStartsTheNext
        , testCase "removes a queued job from the queue" removesAQueuedJobFromTheQueue
        , testCase "does nothing with no selection" cancelSelectedWithNoSelectionDoesNothing
        ]
    , testGroup
        "report"
        [ testCase "a selected job asks for a save location" saveSelectedReportAsksForAFile
        , testCase "no selection asks for nothing" saveSelectedReportWithNoSelectionDoesNothing
        ]
    , testGroup
        "navigation"
        [ testCase "next from no selection picks the first job" selectNextFromNoSelectionPicksTheFirst
        , testCase "next at the last job stays" selectNextAtTheLastJobStays
        , testCase "previous from no selection picks the last job" selectPreviousFromNoSelectionPicksTheLast
        , testCase "previous at the first job stays" selectPreviousAtTheFirstJobStays
        ]
    , testGroup
        "close"
        [ testCase "a close while a job runs asks first" closeWhileRunningAsksFirst
        , testCase "a confirmed close cancels the job and closes" confirmCloseCancelsTheJobAndCloses
        ]
    , testGroup
        "ClearFinished"
        [ testCase "drops finished jobs and clears selection" dropsFinishedJobsAndClearsSelection
        ]
    , testGroup
        "seal"
        [ testCase "enqueue seal asks for a plan" enqueueSealAsksForAPlan
        ]
    , testGroup
        "plan"
        [ testCase "a request asks for one plan" requestPlanAsksForOne
        , testCase "a discarded plan queues nothing" discardPlanQueuesNothing
        , testCase "a confirm on an idle engine starts at once" confirmOnEmptyQueueStartsAtOnce
        , testCase "a stale replan needs review" staleReplanNeedsReview
        , testCase "a seal choice replans the same job and drops the plan it left" sealChoiceReplansTheSameJob
        , testCase "an existing-copy choice replans the same job" existingCopyChoiceReplansTheSameJob
        , testCase "a failed replan fails that job and starts the next" failedReplanFailsTheJobAndStartsTheNext
        , testCase "a ready plan asks for a save location" savePlanAsksForAFile
        ]
    ]

m0 :: Model
m0 = initialModel at LightPalette

run :: List Message -> Model -> (Model, List Command)
run msgs m = foldl (\(mm, cs) msg -> let (mm', cs') = update msg mm in (mm', cs <> cs')) (m, []) msgs

-- | A job never gets to the engine without a plan. A test that wants a running job must
-- supply one, and it supplies the one the planner settles for that kind.
planned :: JobId -> Job -> Message
planned jid job = PlanComputed spec (Right (planOf job spec))
  where
    spec = specFor jid job

planOf :: Job -> (JobSpec -> JobPlan)
planOf = \case
  Offload _ -> readyPlan
  VerifyFolder _ -> verifyPlan
  SealMediaSource _ -> sealPlan

-- | The picked folders are the fixture's own, so the job the model mints out of the draft is the
-- job the plan was made for. 'awaitingPlan' compares the whole spec, so the two must agree.
offloadFlow' :: Int -> List Message
offloadFlow' n =
  [ Ui OpenOffloadDialog
  , SourcePicked mediaSource
  , DestinationPicked shuttle
  , DestinationPicked archive
  , Ui ReviewPlan
  , planned (JobId n) (offloadJob UseHistory)
  ]

offloadFlow :: Int -> List Message
offloadFlow n = offloadFlow' n <> [Ui ConfirmPlan]

enqueueOffload :: List Message
enqueueOffload = offloadFlow 1

-- | The plan computed before the choice changed is never the plan the operator approves.
sealChoiceReplansTheSameJob :: Assertion
sealChoiceReplansTheSameJob = do
  let (m1, _) = run (offloadFlow' 1) m0
      (m2, cmds) = update (Ui (SetSealFirst (SealBeforeCopy StopBeforeCopy))) m1
      (m3, _) = update (planned (JobId 1) (offloadJob UseHistory)) m2
  assertBool "replans the same job" (any (\case ComputePlan spec -> spec.jobId == JobId 1; _ -> False) cmds)
  m2.nextId @?= m1.nextId
  m3.planPhase @?= m2.planPhase

-- | The plan computed before the choice changed is never the plan the operator approves.
existingCopyChoiceReplansTheSameJob :: Assertion
existingCopyChoiceReplansTheSameJob = do
  let (m1, _) = run (offloadFlow' 1) m0
      (m2, cmds) = update (Ui (SetExistingCopy Resume)) m1
  assertBool "replans the same job" (any (\case ComputePlan spec -> spec.jobId == JobId 1; _ -> False) cmds)
  assertBool "the new spec carries the choice" (any (\case ComputePlan spec | Offload oj <- spec.job -> oj.existingCopy == Just Resume; _ -> False) cmds)
  m2.nextId @?= m1.nextId

isStart :: Command -> Bool
isStart = \case StartJob _ -> True; _ -> False

startsFirstJobImmediatelyAndResetsDraft :: Assertion
startsFirstJobImmediatelyAndResetsDraft = do
  let (m, cmds) = run enqueueOffload m0
  length (filter isStart cmds) @?= 1
  m.running @?= Just (JobId 1)
  m.draft @?= Nothing
  m.selected @?= Just (JobId 1)

queuesTheSecondJob :: Assertion
queuesTheSecondJob = do
  let (m, cmds) = run (offloadFlow 1 <> offloadFlow 2) m0
  length (filter isStart cmds) @?= 1
  m.queue @?= [JobId 2]

-- | The model plans the next job again before it runs, so the command is a replan and not a start.
startsTheNextJobWhenTheRunningOneFinishes :: Assertion
startsTheNextJobWhenTheRunningOneFinishes = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, cmds) = update (EngineEvent (JobId 1) (JobFinished AllOk)) m1
  m2.running @?= Just (JobId 2)
  length (filter isComputePlan cmds) @?= 1
  assertBool "toast is set" (isJust m2.toast)

neverHasMoreThanOneRunningJob :: Property
neverHasMoreThanOneRunningJob = property $ do
  n <- forAll $ Gen.int (Range.linear 0 10)
  finishes <- forAll $ Gen.list (Range.linear 0 10) (Gen.int (Range.linear 1 10))
  let msgs = concatMap (\i -> offloadFlow i) [1 .. n] <> map (\i -> EngineEvent (JobId i) (JobFinished AllOk)) finishes
      (m, _) = run msgs m0
      runningCount = length (filter (\entry -> entry.state.phase == Running) (Map.elems m.jobs))
  assert (runningCount <= 1)
  assert (maybe True (\r -> Map.member r m.jobs) m.running)

cancelsTheRunningJobAndStartsTheNext :: Assertion
cancelsTheRunningJobAndStartsTheNext = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, cmds) = run [Ui (SelectJob (Just (JobId 1))), Ui CancelSelectedJob] m1
  (m2.jobs Map.! JobId 1).state.phase @?= Cancelled
  m2.running @?= Just (JobId 2)
  assertBool "emits CancelRunning for job 1" (any (\case CancelRunning (JobId 1) -> True; _ -> False) cmds)

removesAQueuedJobFromTheQueue :: Assertion
removesAQueuedJobFromTheQueue = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob (Just (JobId 2))), Ui CancelSelectedJob] m1
  m2.queue @?= []
  (m2.jobs Map.! JobId 2).state.phase @?= Cancelled

cancelSelectedWithNoSelectionDoesNothing :: Assertion
cancelSelectedWithNoSelectionDoesNothing = do
  let (m1, _) = run (offloadFlow 1) m0
      (m2, cmds) = run [Ui (SelectJob Nothing), Ui CancelSelectedJob] m1
  m2.running @?= Just (JobId 1)
  assertBool "no cancel was ordered" (not (any (\case CancelRunning _ -> True; _ -> False) cmds))

saveSelectedReportAsksForAFile :: Assertion
saveSelectedReportAsksForAFile = do
  let (m1, _) = run (offloadFlow 1) m0
      (_, cmds) = run [Ui (SelectJob (Just (JobId 1))), Ui SaveSelectedReport] m1
  assertBool "asks for a save location" (any (\case OpenSaveDialog {} -> True; _ -> False) cmds)

savePlanAsksForAFile :: Assertion
savePlanAsksForAFile = do
  let (m1, _) = run (offloadFlow' 1) m0
      (_, cmds) = update (Ui SavePlan) m1
  assertBool "asks for a save location" (any (\case OpenSaveDialog {} -> True; _ -> False) cmds)

saveSelectedReportWithNoSelectionDoesNothing :: Assertion
saveSelectedReportWithNoSelectionDoesNothing = do
  let (m1, _) = run (offloadFlow 1) m0
      (_, cmds) = run [Ui (SelectJob Nothing), Ui SaveSelectedReport] m1
  assertBool "asks for nothing" (not (any (\case OpenSaveDialog {} -> True; _ -> False) cmds))

selectNextFromNoSelectionPicksTheFirst :: Assertion
selectNextFromNoSelectionPicksTheFirst = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob Nothing), Ui SelectNextJob] m1
  m2.selected @?= Just (JobId 1)

selectNextAtTheLastJobStays :: Assertion
selectNextAtTheLastJobStays = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob (Just (JobId 2))), Ui SelectNextJob] m1
  m2.selected @?= Just (JobId 2)

selectPreviousFromNoSelectionPicksTheLast :: Assertion
selectPreviousFromNoSelectionPicksTheLast = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob Nothing), Ui SelectPreviousJob] m1
  m2.selected @?= Just (JobId 2)

selectPreviousAtTheFirstJobStays :: Assertion
selectPreviousAtTheFirstJobStays = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob (Just (JobId 1))), Ui SelectPreviousJob] m1
  m2.selected @?= Just (JobId 1)

closeWhileRunningAsksFirst :: Assertion
closeWhileRunningAsksFirst = do
  let (m1, _) = run (offloadFlow 1) m0
      (m2, cmds) = update (Ui RequestClose) m1
  m2.closeConfirm @?= True
  assertBool "the window was not closed" (not (any (\case CloseWindow -> True; _ -> False) cmds))

confirmCloseCancelsTheJobAndCloses :: Assertion
confirmCloseCancelsTheJobAndCloses = do
  let (m1, _) = run (offloadFlow 1) m0
      (m2, cmds) = run [Ui RequestClose, Ui ConfirmClose] m1
  m2.closeConfirm @?= False
  assertBool "cancels the running job" (any (\case CancelRunning (JobId 1) -> True; _ -> False) cmds)
  assertBool "closes the window" (any (\case CloseWindow -> True; _ -> False) cmds)

dropsFinishedJobsAndClearsSelection :: Assertion
dropsFinishedJobsAndClearsSelection = do
  let (m1, _) = run (enqueueOffload <> [EngineEvent (JobId 1) (JobFinished AllOk), Ui ClearFinished]) m0
  Map.keys m1.jobs @?= []
  m1.selected @?= Nothing

enqueueSealAsksForAPlan :: Assertion
enqueueSealAsksForAPlan = do
  let (model, cmds) = update (RequestPlan (sealJob (osp "/vol"))) m0
  assertBool "asks for a plan" (any isComputePlan cmds)
  assertBool "no job was started" (not (any isStartJob cmds))
  Map.keys model.jobs @?= []
  let (confirmed, _) = run [planned (JobId 1) (sealJob (osp "/vol")), Ui ConfirmPlan] model
  map (\job -> jobKind job) (queuedJobs confirmed) @?= [SealKind]

requestPlanAsksForOne :: Assertion
requestPlanAsksForOne = do
  let (model, cmds) = update (RequestPlan (offloadJob UseHistory)) m0
  length (filter isComputePlan cmds) @?= 1
  model.planPhase @?= Planning JobSpec {jobId = JobId 1, job = offloadJob UseHistory, createdAt = at}
  Map.keys model.jobs @?= []

discardPlanQueuesNothing :: Assertion
discardPlanQueuesNothing = do
  let (model, cmds) = run [RequestPlan (offloadJob UseHistory), planned (JobId 1) (offloadJob UseHistory), Ui DiscardPlan] m0
  model.planPhase @?= Idle
  Map.keys model.jobs @?= []
  model.queue @?= []
  assertBool "no job was started" (not (any isStartJob cmds))

confirmOnEmptyQueueStartsAtOnce :: Assertion
confirmOnEmptyQueueStartsAtOnce = do
  let (model, cmds) = run (offloadFlow 1) m0
  model.running @?= Just (JobId 1)
  (model.jobs Map.! JobId 1).state.phase @?= Running
  length (filter isStart cmds) @?= 1
  -- The planPhase asked for the one plan. The confirmation must not ask for another.
  length (filter isComputePlan cmds) @?= 1

-- | A job whose plan fails must not hold the engine slot.
failedReplanFailsTheJobAndStartsTheNext :: Assertion
failedReplanFailsTheJobAndStartsTheNext = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2 <> offloadFlow 3) m0
      (m2, _) = update (EngineEvent (JobId 1) (JobFinished AllOk)) m1
      (m3, cmds) = update (PlanComputed (specFor (JobId 2) (offloadJob UseHistory)) (Left "the media source is gone")) m2
  (m3.jobs Map.! JobId 2).state.phase @?= Failed "the media source is gone"
  m3.running @?= Just (JobId 3)
  m3.queue @?= []
  assertBool "the next job is planned" (any isComputePlan cmds)
  assertBool "the failure is shown" (isJust m3.toast)

staleReplanNeedsReview :: Assertion
staleReplanNeedsReview = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = update (EngineEvent (JobId 1) (JobFinished AllOk)) m1
      moved = (readyPlan (specFor (JobId 2) (offloadJob UseHistory))) {totalBytes = 4096}
      (m3, cmds) = update (PlanComputed (specFor (JobId 2) (offloadJob UseHistory)) (Right moved)) m2
  (m3.jobs Map.! JobId 2).state.phase @?= NeedsReview
  m3.running @?= Nothing
  assertBool "no job was started" (not (any isStartJob cmds))

isComputePlan :: Command -> Bool
isComputePlan (ComputePlan _) = True
isComputePlan _ = False

queuedJobs :: Model -> List Job
queuedJobs model = model.jobs & Map.elems & map (\entry -> entry.state.spec.job)

isStartJob :: Command -> Bool
isStartJob (StartJob _) = True
isStartJob _ = False
