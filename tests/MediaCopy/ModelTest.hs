module MediaCopy.ModelTest (tests) where

import Control.Monad (foldM, forM_, unless)
import Data.Function ((&))
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Demo.Fixtures
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan (..), Target (..))
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Theme (Accent (..), PaletteMode (..))
import MediaCopy.Model

tests :: TestTree
tests =
  testGroup
    "Model"
    [ testProperty "one job runs at a time, in the order confirmed" schedulerInvariants
    , testCase "nothing runs without a confirmed plan" nothingRunsWithoutAConfirmedPlan
    , testCase "a changed plan stops for review" aChangedPlanStopsForReview
    , testCase "changing an option replans the same job" changingAnOptionReplansTheSameJob
    , testCase "closing never silently aborts a job" closingNeverSilentlyAbortsAJob
    ]

m0 :: Model
m0 = initialModel at LightPalette Blue

run :: List Message -> Model -> (Model, List AppEffect)
run msgs m = foldl (\(mm, cs) msg -> let (mm', cs') = update msg mm in (mm', cs <> cs')) (m, []) msgs

planned :: JobId -> Job -> Message
planned jid job = PlanComputed spec (Right (planOf job spec))
  where
    spec = specFor jid job

planOf :: Job -> (JobSpec -> JobPlan)
planOf = \case
  Offload _ -> readyPlan
  VerifyFolder _ -> verifyPlan
  SealMediaSource _ -> sealPlan

offloadFlow' :: Int -> List Message
offloadFlow' n =
  [ Ui (RunCommand Command.NewOffload)
  , SourcePicked mediaSource
  , DestinationPicked shuttle
  , DestinationPicked archive
  , Ui ReviewPlan
  , planned (JobId n) (offloadJob UseHistory)
  ]

offloadFlow :: Int -> List Message
offloadFlow n = offloadFlow' n <> [Ui ConfirmPlan]

data Replan = SamePlan | ChangedPlan | PlanFailed
  deriving stock (Show)

data Step
  = Enqueue Job
  | Finish Bool
  | Cancel Int
  | Answer Int Replan
  deriving stock (Show)

data World = World
  { model :: Model
  , pending :: List JobSpec
  , started :: List JobId
  , dead :: Set JobId
  }

genStep :: Gen Step
genStep =
  Gen.frequency
    [ (3, Enqueue <$> Gen.element [offloadJob UseHistory, verifyJob (osp "/vol"), sealJob (osp "/vol")])
    , (2, Finish <$> Gen.bool)
    , (1, Cancel <$> Gen.int (Range.linear 0 20))
    , (3, Answer <$> Gen.int (Range.linear 0 20) <*> Gen.element [SamePlan, ChangedPlan, PlanFailed])
    ]

perform :: Step -> World -> (Model, List AppEffect, List JobSpec)
perform step world = case step of
  Enqueue job ->
    let (asked, _) = update (RequestPlan job) model
        (confirmed, effects) = case asked.planPhase of
          Planning spec -> run [PlanComputed spec (Right (planOf job spec)), Ui ConfirmPlan] asked
          _ -> (asked, [])
    in (confirmed, effects, world.pending)
  Finish ok -> case model.running of
    Just jid
      | phaseOf jid model == Just Running ->
          let (next, effects) = update (EngineEvent jid (if ok then JobFinished AllOk else JobFailed "gone")) model
          in (next, effects, world.pending <> computed effects)
    _ -> (model, [], world.pending)
  Cancel i -> case Map.keys model.jobs of
    [] -> (model, [], world.pending)
    ids ->
      let (next, effects) = run [Ui (SelectJob (Just (ids !! (i `mod` length ids)))), Ui (RunCommand Command.CancelJob)] model
      in (next, effects, world.pending <> computed effects)
  Answer i replan -> case drop (i `mod` max 1 (length world.pending)) world.pending of
    spec : _ ->
      let stored = Map.lookup spec.jobId model.jobs >>= (.plan)
          outcome = case (replan, stored) of
            (SamePlan, Just plan) -> Right plan {targets = V.map (\target -> target {freeBytes = fmap (subtract 4096) target.freeBytes}) plan.targets}
            (ChangedPlan, Just plan) -> Right plan {totalBytes = plan.totalBytes + 4096}
            _ -> Left "gone"
          (next, effects) = update (PlanComputed spec outcome) model
      in (next, effects, world.pending <> computed effects)
    _ -> (model, [], world.pending)
  where
    model = world.model

computed :: List AppEffect -> List JobSpec
computed effects = [spec | ComputePlan spec <- effects]

phaseOf :: JobId -> Model -> Maybe JobPhase
phaseOf jid model = fmap (\entry -> entry.state.phase) (Map.lookup jid model.jobs)

withPhase :: (JobPhase -> Bool) -> Model -> List JobId
withPhase wanted model = model.jobs & Map.filter (\entry -> wanted entry.state.phase) & Map.keys

isDead :: JobPhase -> Bool
isDead = \case
  (Cancelled; Failed _) -> True
  _ -> False

schedulerInvariants :: Property
schedulerInvariants = withTests 500 $ property $ do
  steps <- forAll (Gen.list (Range.linear 1 40) genStep)
  final <- foldM advance World {model = m0, pending = [], started = [], dead = Set.empty} steps
  cover 20 "two or more jobs ran" (length final.started >= 2)
  cover 5 "a changed plan stopped for review" (not (null (withPhase (== NeedsReview) final.model)))
  cover 20 "a job was cancelled or failed" (not (Set.null final.dead))
  where
    advance world step = do
      let before = world.model
          (next, effects, pending) = perform step world
          starts = [plan.spec.jobId | StartJob plan <- effects]
          running = withPhase (== Running) next
          newlyRunning = case next.running of
            Just jid | next.running /= before.running -> [jid]
            _ -> []
      annotateShow step
      assert (length running <= 1)
      assert (all (\jid -> next.running == Just jid) running)
      forM_ next.running $ \jid -> do
        assert (Map.member jid next.jobs)
        assert (jid `notElem` next.queue)
      assert (length starts <= 1)
      unless (null starts) $ do
        withPhase (== Running) before === []
        next.running === listToMaybe starts
      assert (and (zipWith (<) next.queue (drop 1 next.queue)))
      forM_ newlyRunning $ \jid -> do
        assert (all (> jid) next.queue)
        assert (all (< jid) world.started)
      forM_ (Set.toList world.dead) $ \jid -> do
        assert (next.running /= Just jid)
        assert (jid `notElem` next.queue)
        assert (jid `notElem` starts)
        assert (maybe False isDead (phaseOf jid next))
      pure
        World
          { model = next
          , pending
          , started = world.started <> newlyRunning
          , dead = Set.union world.dead (Set.fromList (withPhase isDead next))
          }

nothingRunsWithoutAConfirmedPlan :: Assertion
nothingRunsWithoutAConfirmedPlan =
  forM_ [offloadJob UseHistory, verifyJob (osp "/vol"), sealJob (osp "/vol")] $ \job -> do
    let spec = specFor (JobId 1) job
        (asked, askCmds) = update (RequestPlan job) m0
        (shown, shownCmds) = update (PlanComputed spec (Right (planOf job spec))) asked
        (discarded, discardCmds) = update (Ui DiscardPlan) shown
        (confirmed, confirmCmds) = update (Ui ConfirmPlan) shown
    length (filter isComputePlan askCmds) @?= 1
    assertBool "no job was started" (not (any isStartJob (askCmds <> shownCmds <> discardCmds)))
    Map.keys shown.jobs @?= []
    Map.keys discarded.jobs @?= []
    discarded.queue @?= []
    map jobKind (queuedJobs confirmed) @?= [jobKind job]
    confirmed.running @?= Just (JobId 1)
    length (filter isStartJob confirmCmds) @?= 1

aChangedPlanStopsForReview :: Assertion
aChangedPlanStopsForReview = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2 <> offloadFlow 3) m0
      (m2, _) = update (EngineEvent (JobId 1) (JobFinished AllOk)) m1
      spec = specFor (JobId 2) (offloadJob UseHistory)
      stored = readyPlan spec
      lessFree = stored {targets = V.map (\target -> target {freeBytes = fmap (subtract 4096) target.freeBytes}) stored.targets}
      moved = stored {totalBytes = stored.totalBytes + 4096}
      (same, sameCmds) = update (PlanComputed spec (Right lessFree)) m2
      (changed, changedCmds) = update (PlanComputed spec (Right moved)) m2
  assertBool "the free space moved" (lessFree /= stored)
  phaseOf (JobId 2) same @?= Just Running
  same.running @?= Just (JobId 2)
  length (filter isStartJob sameCmds) @?= 1
  phaseOf (JobId 2) changed @?= Just NeedsReview
  assertBool "no job was started" (not (any isStartJob changedCmds))
  changed.running @?= Just (JobId 3)
  assertBool "the next job is planned" (any (replans (JobId 3)) changedCmds)

changingAnOptionReplansTheSameJob :: Assertion
changingAnOptionReplansTheSameJob = do
  let (m1, _) = run (offloadFlow' 1) m0
      (sealing, sealCmds) = update (Ui (SetSealFirst (SealBeforeCopy StopBeforeCopy))) m1
      (late, _) = update (planned (JobId 1) (offloadJob UseHistory)) sealing
      (resuming, resumeCmds) = update (Ui (SetExistingCopy Resume)) m1
  assertBool "seal-first replans job 1" (any (replansWith (JobId 1) (\oj -> oj.sealFirst == SealBeforeCopy StopBeforeCopy)) sealCmds)
  assertBool "an existing-copy choice replans job 1" (any (replansWith (JobId 1) (\oj -> oj.existingCopy == Just Resume)) resumeCmds)
  sealing.nextId @?= m1.nextId
  resuming.nextId @?= m1.nextId
  late.planPhase @?= sealing.planPhase

closingNeverSilentlyAbortsAJob :: Assertion
closingNeverSilentlyAbortsAJob = do
  let (m1, _) = run (offloadFlow 1) m0
      (asked, askCmds) = update (Ui RequestClose) m1
      (_, closeCmds) = update (Ui ConfirmClose) asked
  asked.closeConfirm @?= True
  asked.running @?= Just (JobId 1)
  assertBool "nothing was stopped" (not (any isDestroyWindow askCmds || any isCancelRunning askCmds))
  assertBool "cancels the running job" (any (cancels (JobId 1)) closeCmds)
  assertBool "closes the window" (any isDestroyWindow closeCmds)

isComputePlan :: AppEffect -> Bool
isComputePlan (ComputePlan _) = True
isComputePlan _ = False

queuedJobs :: Model -> List Job
queuedJobs model = model.jobs & Map.elems & map (.state.spec.job)

isStartJob :: AppEffect -> Bool
isStartJob (StartJob _) = True
isStartJob _ = False

isCancelRunning :: AppEffect -> Bool
isCancelRunning (CancelRunning _) = True
isCancelRunning _ = False

cancels :: JobId -> AppEffect -> Bool
cancels wanted (CancelRunning jid) = jid == wanted
cancels _ _ = False

replans :: JobId -> AppEffect -> Bool
replans wanted (ComputePlan spec) = spec.jobId == wanted
replans _ _ = False

replansWith :: JobId -> (OffloadJob -> Bool) -> AppEffect -> Bool
replansWith wanted holds (ComputePlan spec)
  | Offload oj <- spec.job = spec.jobId == wanted && holds oj
replansWith _ _ _ = False

isDestroyWindow :: AppEffect -> Bool
isDestroyWindow DestroyWindow = True
isDestroyWindow _ = False
