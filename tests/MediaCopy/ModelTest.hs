module MediaCopy.ModelTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Demo.Fixtures
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Theme (Accent (..), PaletteMode (..))
import MediaCopy.Model

tests :: TestTree
tests =
  testGroup
    "Model"
    [ testProperty "one job runs at a time, in the order confirmed" schedulerInvariants
    , testCase "a late event of a cancelled job changes nothing" aLateEventOfACancelledJobChangesNothing
    ]

m0 :: Model
m0 = initialModel at LightPalette Blue

run :: List Message -> Model -> (Model, List AppEffect)
run msgs m = foldl (\(mm, cs) msg -> let (mm', cs') = update msg mm in (mm', cs <> cs')) (m, []) msgs

planned :: JobId -> Job -> Message
planned jid job = PlanComputed spec (Right (planOf job spec))
  where
    spec = specFor jid job

planOf :: Job -> (JobSpec -> Plan)
planOf = \case
  Job.Offload _ -> readyPlan
  Job.VerifyFolder _ -> verifyPlan
  Job.SealMediaSource _ -> sealPlan

offloadFlow' :: Int -> List Message
offloadFlow' n =
  [ Ui (RunCommand Command.NewOffload)
  , SourcePicked mediaSource
  , DestinationPicked shuttle
  , DestinationPicked archive
  , Ui ReviewPlan
  , planned (JobId.JobId n) (offloadJob Offload.UseHistory)
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
    [ (3, Enqueue <$> Gen.element [offloadJob Offload.UseHistory, verifyJob (osp "/vol"), sealJob (osp "/vol")])
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
      | phaseOf jid model == Just JobState.Running ->
          let (next, effects) = update (EngineEvent jid (if ok then JobEvent.Finished JobResult.AllOk else JobEvent.Failed "gone")) model
          in (next, effects, world.pending <> computed effects)
    _ -> (model, [], world.pending)
  Cancel i -> case Map.keys model.jobs of
    [] -> (model, [], world.pending)
    ids ->
      let (next, effects) = run [Ui (SelectJob (listToMaybe (drop (i `mod` length ids) ids))), Ui (RunCommand Command.CancelJob)] model
      in (next, effects, world.pending <> computed effects)
  Answer i replan -> case drop (i `mod` max 1 (length world.pending)) world.pending of
    spec : _ ->
      let stored = Map.lookup spec.id model.jobs >>= (.plan)
          outcome = case (replan, stored) of
            (SamePlan, Just plan) -> Right plan {Plan.targets = V.map (\target -> target {Target.freeBytes = fmap (subtract 4096) target.freeBytes}) plan.targets}
            (ChangedPlan, Just plan) -> Right plan {Plan.totalBytes = plan.totalBytes + 4096}
            _ -> Left "gone"
          (next, effects) = update (PlanComputed spec outcome) model
      in (next, effects, world.pending <> computed effects)
    _ -> (model, [], world.pending)
  where
    model = world.model

computed :: List AppEffect -> List JobSpec
computed effects = [spec | ComputePlan spec <- effects]

phaseOf :: JobId -> Model -> Maybe JobState.Phase
phaseOf jid model = fmap (\entry -> entry.state.phase) (Map.lookup jid model.jobs)

withPhase :: (JobState.Phase -> Bool) -> Model -> List JobId
withPhase wanted model = model.jobs & Map.filter (\entry -> wanted entry.state.phase) & Map.keys

isDead :: JobState.Phase -> Bool
isDead = \case
  (JobState.Cancelled; JobState.Failed _) -> True
  _ -> False

schedulerInvariants :: Property
schedulerInvariants = withTests 500 $ property $ do
  steps <- forAll (Gen.list (Range.linear 1 40) genStep)
  final <- foldM advance World {model = m0, pending = [], started = [], dead = Set.empty} steps
  cover 20 "two or more jobs ran" (length final.started >= 2)
  cover 5 "a changed plan stopped for review" (not (null (withPhase (== JobState.NeedsReview) final.model)))
  cover 20 "a job was cancelled or failed" (not (Set.null final.dead))
  where
    advance world step = do
      let before = world.model
          (next, effects, pending) = perform step world
          starts = [plan.spec.id | StartJob plan <- effects]
          running = withPhase (== JobState.Running) next
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
        withPhase (== JobState.Running) before === []
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

aLateEventOfACancelledJobChangesNothing :: Assertion
aLateEventOfACancelledJobChangesNothing = do
  let (m1, _) = run (offloadFlow 1 <> offloadFlow 2) m0
      (m2, _) = run [Ui (SelectJob (Just (JobId.JobId 1))), Ui (RunCommand Command.CancelJob)] m1
  phaseOf (JobId.JobId 1) m2 @?= Just JobState.Cancelled
  m2.running @?= Just (JobId.JobId 2)
  forM_ [JobEvent.Finished JobResult.AllOk, JobEvent.Failed "late", JobEvent.Progress 10] $ \ev -> do
    let (m3, effects) = update (EngineEvent (JobId.JobId 1) ev) m2
    assertBool "no effect" (null effects)
    m3.running @?= Just (JobId.JobId 2)
    m3.queue @?= m2.queue
    phaseOf (JobId.JobId 1) m3 @?= Just JobState.Cancelled
    phaseOf (JobId.JobId 2) m3 @?= phaseOf (JobId.JobId 2) m2
    m3.toast @?= m2.toast
