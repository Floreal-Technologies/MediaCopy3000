{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.FlowTest (tests) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.FileEntry qualified as FileEntry
import Ascmhl.Hash qualified as Hash
import Ascmhl.History qualified as History
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.ByteString qualified as BS
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful (runEff)
import Hedgehog (Gen, Property, assert, cover, evalIO, evalMaybe, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (makeRelative, unsafeEncodeUtf, (</>))
import splice System.OsPath (osp)
import System.OsString (isPrefixOf, isSuffixOf)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.JobState qualified as JobState
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Part qualified as Part
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Effects.Hasher qualified as Hasher
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Model (JobEntry (..), Message (..), Model (..), PlanPhase (..), UiMessage (..), toastMessage)
import MediaCopy.Test.Harness (Act, Settled)
import MediaCopy.Test.Harness qualified as Harness
import MediaCopy.Test.InMemoryFS (MemFS)
import MediaCopy.Test.InMemoryFS qualified as InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Flows"
    [ testCase "offload to two destinations" offloadToTwoDestinations
    , testProperty "the plan and the engine agree" planAndEngineAgree
    , testCase "a queued job is replanned when its turn comes" queuedJobIsReplanned
    , testCase "cancel during a copy" cancelDuringACopy
    , testCase "closing never silently aborts a job" closingNeverAbortsAJob
    , testCase "a fault never loses a good copy nor leaves a partial" aFaultNeverLosesAGoodCopy
    , testCase "sealing records original, then verified or failed" sealingRecordsOriginalThenVerifiedOrFailed
    , testCase "verify reports each kind of difference, and the report is saved" verifyReportsEachDifference
    , testCase "an offload continues the source history" anOffloadContinuesTheSourceHistory
    , testCase "seal-first" sealFirst
    , testCase "a failed copy never passes a later verify" aFailedCopyNeverPassesALaterVerify
    ]

offloadToTwoDestinations :: Assertion
offloadToTwoDestinations = do
  discarded <- Harness.drive (Harness.rig cardFS) (draft <> [Harness.user DiscardPlan])
  Harness.expectClean discarded
  Map.keys discarded.model.jobs @?= []
  discarded.disk.files @?= cardFS.files
  done <- Harness.drive (Harness.rig cardFS) (draft <> [Harness.user ConfirmPlan])
  Harness.expectClean done
  forM_ [[osp|/ssd1/card|], [osp|/ssd2/card|]] $ \root -> do
    forM_ copied $ \rel ->
      bytesAt (joined root rel) done.disk @?= bytesAt (joined card rel) cardFS
    Map.member (joined root [osp|.DS_Store|]) done.disk.files @?= False
    length (manifestsIn root done.disk) @?= 1
    actionsIn root done.disk @?= Just [FileEntry.Original, FileEntry.Original, FileEntry.Original]
    chainIn root done.disk @?= Right [1]
  Map.filterWithKey (\path _ -> under card path) done.disk.files @?= cardFS.files
  phaseOf (JobId.JobId 1) done @?= Just (JobState.Finished JobResult.AllOk)
  fmap (\entry -> V.length entry.state.mhlPaths) (Map.lookup (JobId.JobId 1) done.model.jobs) @?= Just 2
  done.model.running @?= Nothing
  done.model.queue @?= []
  assertBool "a toast is set" (isJust done.model.toast)
  where
    draft = offload card [ssd1, ssd2]
    copied = [[osp|A001.mxf|], [osp|B/A002.mxf|], [osp|notes.txt|]]

cardFS :: MemFS
cardFS =
  InMemoryFS.emptyMemFS
    & InMemoryFS.withFile [osp|/card/A001.mxf|] "A001 essence"
    & InMemoryFS.withFile [osp|/card/B/A002.mxf|] "A002 essence"
    & InMemoryFS.withFile [osp|/card/notes.txt|] "notes"
    & InMemoryFS.withFile [osp|/card/.DS_Store|] "finder"

data Held = NotHeld | PartOf Int | Same | Flipped | OtherSize
  deriving stock (Eq, Show)

data HeldCopy = HeldCopy {parent :: OsPath, held :: Maybe (List Held)}
  deriving stock (Show)

data Generated = Generated
  { files :: List (RelPath, ByteString)
  , destinations :: List HeldCopy
  , choice :: Maybe Offload.ExistingCopy
  , sealed :: Bool
  }
  deriving stock (Show)

planAndEngineAgree :: Property
planAndEngineAgree = withTests 400 $ property $ do
  generated <- forAll genGenerated
  cover 0.5 "all reuse with a recorded original" (allReuseWithRecordedOriginal generated)
  let parents = fmap (.parent) generated.destinations
      sealing = if generated.sealed then sealVol else []
      choosing = maybe [] (\choice -> [Harness.user (SetExistingCopy choice)]) generated.choice
      setup = (Harness.rig (seedGenerated generated)) {Harness.picks = if generated.sealed then [card] else []}
      offloadId = JobId.JobId (if generated.sealed then 2 else 1)
  settled <- evalIO (Harness.drive setup (sealing <> offload card parents <> choosing <> [Harness.user ConfirmPlan]))
  evalIO (Harness.expectClean settled)
  when generated.sealed (phaseOf (JobId.JobId 1) settled === Just (JobState.Finished JobResult.AllOk))
  entry <- evalMaybe (Map.lookup offloadId settled.model.jobs)
  plan <- evalMaybe entry.plan
  assert (not (Plan.blocked plan))
  when generated.sealed (assert (V.any isRecordedCopy plan.steps))
  entry.state.phase === JobState.Finished JobResult.AllOk
  entry.state.bytesDone === plan.bytesToRead + rewriteBytes generated
  forM_ generated.destinations $ \destination -> do
    let root = joined destination.parent [osp|card|]
    forM_ generated.files $ \file -> do
      bytesAt (relAt root (fst file)) settled.disk === Just (snd file)
      Map.member (Part.path (relAt root (fst file))) settled.disk.files === False
    manifestPaths root settled.disk === Just (sort (fmap fst generated.files))
  let statuses rel =
        settled.events
          & mapMaybe (\pair -> if fst pair == offloadId then statusOf rel (snd pair) else Nothing)
  forM_ generated.files $ \file -> do
    let replaced = statuses (fst file) & filter InMemoryFS.isReplaced & length
    replaced === (if fst file `elem` reusedMismatches generated then 1 else 0)

genGenerated :: Gen Generated
genGenerated = do
  names <- Gen.subsequence candidatePaths & Gen.filter (not . null)
  files <- traverse (\path -> Gen.bytes (Range.linear 0 9000) <&> \bytes -> (path, bytes)) names
  count <- Gen.int (Range.linear 1 2)
  destinations <- traverse (genHeldCopy files) [1 .. count]
  let anyPartial = any (\destination -> maybe False (any (/= NotHeld)) destination.held) destinations
  choice <- if anyPartial then Just <$> Gen.element [Offload.Resume, Offload.Replace] else pure Nothing
  sealed <- Gen.bool
  pure Generated {files, destinations, choice, sealed}

candidatePaths :: List RelPath
candidatePaths = fmap RelPath.RelPath ["a.mxf", "B/1.mxf", "B/C/2.mxf", "d.txt", "B/e.mxf", "f.mov"]

genHeldCopy :: List (RelPath, ByteString) -> Int -> Gen HeldCopy
genHeldCopy files index = do
  held <- Gen.maybe (traverse (genHeld . snd) files)
  pure HeldCopy {parent = unsafeEncodeUtf ("/ssd" <> show index), held}

genHeld :: ByteString -> Gen Held
genHeld bytes =
  Gen.frequency
    [ (3, pure NotHeld)
    , (1, PartOf <$> Gen.int (Range.linear 0 100))
    , (4, pure Same)
    , (1, pure (if BS.null bytes then Same else Flipped))
    , (1, pure OtherSize)
    ]

seedGenerated :: Generated -> MemFS
seedGenerated generated = foldr seedDestination (foldr seedSource InMemoryFS.emptyMemFS generated.files) generated.destinations
  where
    seedSource file = InMemoryFS.withFile (relAt card (fst file)) (snd file)
    seedDestination destination fs = case destination.held of
      Nothing -> fs
      Just helds ->
        let root = joined destination.parent [osp|card|]
        in foldr (seedHeld root) (InMemoryFS.withDir root fs) (zip generated.files helds)
    seedHeld root (file, held) fs =
      let target = relAt root (fst file)
          bytes = snd file
      in case held of
           NotHeld -> fs
           PartOf percent -> InMemoryFS.withFile (Part.path target) (BS.take (BS.length bytes * percent `div` 100) bytes) fs
           Same -> InMemoryFS.withFile target bytes fs
           Flipped -> InMemoryFS.withFile target (InMemoryFS.corruptFirstByte bytes) fs
           OtherSize -> InMemoryFS.withFile target (bytes <> "x") fs

flippedReuses :: Generated -> List (RelPath, ByteString)
flippedReuses generated
  | generated.choice /= Just Offload.Resume = []
  | otherwise =
      generated.destinations
        & concatMap (\destination -> maybe [] (zip generated.files) destination.held)
        & filter (\pair -> snd pair == Flipped)
        & fmap fst

reusedMismatches :: Generated -> List RelPath
reusedMismatches generated = fmap fst (flippedReuses generated)

rewriteBytes :: Generated -> Int64
rewriteBytes generated = sum (fmap (\file -> 2 * fromIntegral (BS.length (snd file))) (flippedReuses generated))

allReuseWithRecordedOriginal :: Generated -> Bool
allReuseWithRecordedOriginal generated =
  generated.sealed
    && generated.choice == Just Offload.Resume
    && all (\destination -> maybe False (all (\held -> held == Same || held == Flipped)) destination.held) generated.destinations

isRecordedCopy :: PlanStep -> Bool
isRecordedCopy step = case step.op of
  PlanStep.Copy (PlanStep.Recorded _) -> True
  _ -> False

manifestPaths :: OsPath -> MemFS -> Maybe (List RelPath)
manifestPaths root fs =
  InMemoryFS.newestManifestIn root fs <&> \manifest ->
    manifest.entries & ManifestEntry.files & V.toList & fmap (.path) & sort

queuedJobIsReplanned :: Assertion
queuedJobIsReplanned = do
  same <- Harness.drive (Harness.rig twoFiles) (behindJob1 (offload card [ssd2] <> [Harness.user ConfirmPlan]))
  Harness.expectClean same
  let terminal = findIndex (\pair -> fst pair == JobId.JobId 1 && JobEvent.isTerminal (snd pair)) same.events
      firstOfJob2 = findIndex (\pair -> fst pair == JobId.JobId 2) same.events
  assertBool "no job 2 event comes before job 1's terminal event" (fromMaybe False ((<) <$> terminal <*> firstOfJob2))
  phaseOf (JobId.JobId 1) same @?= Just (JobState.Finished JobResult.AllOk)
  phaseOf (JobId.JobId 2) same @?= Just (JobState.Finished JobResult.AllOk)
  forM_ [[osp|a.mxf|], [osp|b.mxf|]] $ \rel ->
    bytesAt (joined [osp|/ssd2/card|] rel) same.disk @?= bytesAt (joined card rel) twoFiles
  -- Only the free space moves between the plan and the run: the job goes on.
  drifted <- Harness.drive (Harness.rig twoFiles) (behindJob1 (offload card [ssd2] <> [Harness.user ConfirmPlan, Harness.Alter (InMemoryFS.withFreeSpace ssd2 5000)]))
  Harness.expectClean drifted
  phaseOf (JobId.JobId 2) drifted @?= Just (JobState.Finished JobResult.AllOk)
  let changed =
        behindJob1
          ( offload card [ssd2]
              <> [Harness.user ConfirmPlan, Harness.Alter (InMemoryFS.withFile [osp|/card/c.mxf|] "new")]
              <> offload card [ssd3]
              <> [Harness.user ConfirmPlan]
          )
  moved <- Harness.drive (Harness.rig twoFiles) changed
  Harness.expectClean moved
  phaseOf (JobId.JobId 1) moved @?= Just (JobState.Finished JobResult.AllOk)
  phaseOf (JobId.JobId 2) moved @?= Just JobState.NeedsReview
  filesUnder [osp|/ssd2/card|] moved.disk @?= []
  -- The queue goes on behind the job that stopped for review.
  phaseOf (JobId.JobId 3) moved @?= Just (JobState.Finished JobResult.AllOk)
  forM_ [[osp|a.mxf|], [osp|b.mxf|], [osp|c.mxf|]] $ \rel ->
    bytesAt (joined [osp|/ssd3/card|] rel) moved.disk @?= bytesAt (joined card rel) (InMemoryFS.withFile [osp|/card/c.mxf|] "new" twoFiles)
  reviewed <- Harness.drive (Harness.rig twoFiles) (changed <> [Harness.user (SelectJob (Just (JobId.JobId 2))), Harness.user (RunCommand Command.ReviewJob), Harness.user ConfirmPlan])
  Harness.expectClean reviewed
  phaseOf (JobId.JobId 2) reviewed @?= Just (JobState.Finished JobResult.AllOk)
  forM_ [[osp|a.mxf|], [osp|b.mxf|], [osp|c.mxf|]] $ \rel ->
    bytesAt (joined [osp|/ssd2/card|] rel) reviewed.disk @?= bytesAt (joined card rel) (InMemoryFS.withFile [osp|/card/c.mxf|] "new" twoFiles)
  where
    behindJob1 later = offload card [ssd1] <> [Harness.user ConfirmPlan, Harness.During (JobId.JobId 1) (Harness.onStatus FileStatus.Copying) later]

twoFiles :: MemFS
twoFiles =
  InMemoryFS.emptyMemFS
    & InMemoryFS.withFile [osp|/card/a.mxf|] "a essence"
    & InMemoryFS.withFile [osp|/card/b.mxf|] "b essence"

cancelDuringACopy :: Assertion
cancelDuringACopy = do
  beforeRename <- cancelledAt FileStatus.Flushing
  bytesAt [osp|/ssd1/card/a.mxf|] beforeRename.disk @?= Nothing
  afterRename <- cancelledAt FileStatus.Verifying
  bytesAt [osp|/ssd1/card/a.mxf|] afterRename.disk @?= bytesAt [osp|/card/a.mxf|] twoFiles

-- | Cancel job 1 at this status of @a.mxf@, with job 2 queued behind it.
cancelledAt :: FileStatus -> IO Settled
cancelledAt status = do
  settled <- Harness.drive (Harness.rig twoFiles) (offload card [ssd1] <> [Harness.user ConfirmPlan, Harness.During (JobId.JobId 1) (== atA) (offload card [ssd2] <> cancel)])
  Harness.expectClean settled
  phaseOf (JobId.JobId 1) settled @?= Just JobState.Cancelled
  lastEventOf (JobId.JobId 1) settled @?= Just atA
  manifestsIn [osp|/ssd1/card|] settled.disk @?= []
  phaseOf (JobId.JobId 2) settled @?= Just (JobState.Finished JobResult.AllOk)
  let finishedToast = Map.lookup (JobId.JobId 2) settled.model.jobs >>= \entry -> toastMessage settled.model.wording entry.state.spec.job (JobEvent.Finished JobResult.AllOk)
  assertBool "a toast is set" (isJust settled.model.toast)
  assertEqual "no failure toast after job 2's" finishedToast settled.model.toast
  pure settled
  where
    atA = JobEvent.FileStatusChanged (RelPath.RelPath "a.mxf") status
    cancel = [Harness.user ConfirmPlan, Harness.user (SelectJob (Just (JobId.JobId 1))), Harness.user (RunCommand Command.CancelJob)]

closingNeverAbortsAJob :: Assertion
closingNeverAbortsAJob = do
  asked <- Harness.drive (Harness.rig cardFS) (started <> [Harness.During (JobId.JobId 1) (Harness.onStatus FileStatus.Copying) [Harness.user RequestClose]])
  Harness.expectClean asked
  asked.closed @?= False
  assertBool "the window asked while job 1 ran" (any (\shown -> shown.closeConfirm && shown.running == Just (JobId.JobId 1)) asked.models)
  phaseOf (JobId.JobId 1) asked @?= Just (JobState.Finished JobResult.AllOk)
  -- The end of the job takes the question back.
  asked.model.closeConfirm @?= False
  closed <- Harness.drive (Harness.rig cardFS) (started <> [Harness.During (JobId.JobId 1) (Harness.onStatus FileStatus.Copying) [Harness.user RequestClose, Harness.user ConfirmClose]])
  Harness.expectClean closed
  closed.closed @?= True
  let closedAt = closed.events & find (\pair -> fst pair == JobId.JobId 1 && Harness.onStatus FileStatus.Copying (snd pair)) <&> snd
  assertBool "the close happened" (isJust closedAt)
  assertEqual "no job 1 event after the close" closedAt (lastEventOf (JobId.JobId 1) closed)
  assertBool "no JobEvent.Finished" (not (any (JobEvent.isTerminal . snd) closed.events))
  phaseOf (JobId.JobId 1) closed @?= Just JobState.Running
  closed.model.running @?= Nothing
  Map.keys closed.disk.files & filter (not . under card) & (@?= [])
  where
    started = offload card [ssd1] <> [Harness.user ConfirmPlan]

data Fault = Fault
  { label :: String
  , seed :: MemFS
  , destinations :: List OsPath
  , replace :: Bool
  -- ^ The destination holds an old copy, and the operator chooses Replace.
  , finals :: List (OsPath, Maybe ByteString)
  , manifests :: List (OsPath, Maybe (List FileEntry.Action))
  -- ^ A destination root and the actions of its newest manifest.
  , ended :: JobState.Phase -> Bool
  , statuses :: List (RelPath, FileStatus -> Bool)
  , empty :: List OsPath
  -- ^ Folders that must hold no file.
  }

aFaultNeverLosesAGoodCopy :: Assertion
aFaultNeverLosesAGoodCopy =
  forM_ faults $ \fault -> do
    let named what = fault.label <> ": " <> what
        flow = offload card fault.destinations
        choosing = if fault.replace then [Harness.user (SetExistingCopy Offload.Replace)] else []
    when fault.replace $ do
      asked <- Harness.drive (Harness.rig fault.seed) flow
      case asked.model.planPhase of
        Ready plan -> do
          assertBool (named "blocked") (Plan.blocked plan)
          assertBool (named "DestinationPartial") (Finding.DestinationPartial `elem` codesOf plan)
        other -> assertFailure (named ("no plan: " <> show other))
    settled <- Harness.drive (Harness.rig fault.seed) (flow <> choosing <> [Harness.user ConfirmPlan])
    Harness.expectClean settled
    assertEqual (named "the job keeps its number") (JobId.JobId 2) settled.model.nextId
    forM_ fault.finals $ \(path, bytes) ->
      assertEqual (named (show path)) bytes (bytesAt path settled.disk)
    forM_ fault.manifests $ \(root, actions) ->
      assertEqual (named ("manifest " <> show root)) actions (actionsIn root settled.disk)
    let entry = Map.lookup (JobId.JobId 1) settled.model.jobs
    assertBool (named ("phase " <> show (fmap (\found -> found.state.phase) entry))) (maybe False (\found -> fault.ended found.state.phase) entry)
    forM_ fault.empty $ \folder ->
      assertEqual (named ("nothing under " <> show folder)) [] (filesUnder folder settled.disk)
    forM_ fault.statuses $ \(rel, wanted) -> do
      let status = entry >>= \found -> Map.lookup rel found.state.files <&> (.status)
      assertBool (named ("status " <> show status)) (maybe False wanted status)
    assertBool (named "a toast is set") (isJust settled.model.toast)

faults :: List Fault
faults =
  [ (overwrite "the open of the part copy fails on an overwrite" (InMemoryFS.withFailOnOpen [osp|/ssd1/card/b.txt.mc3k-part|])) {finals = [(oldB, Just "old")]}
  , (overwrite "the rename fails on an overwrite" (InMemoryFS.withFailOnRename oldB)) {finals = [(oldB, Just "old")]}
  , (overwrite "the read-back fails on an overwrite" (InMemoryFS.withFailOnOpen oldB)) {finals = [(oldB, Just "hi")]}
  , (fresh "the rename fails on a new file" (InMemoryFS.withFailOnRename newA)) {finals = [(newA, Nothing)]}
  , (fresh "the read-back fails on a new file" (InMemoryFS.withFailOnOpen newA)) {finals = [(newA, Just "data")]}
  , (fresh "the rename fails at the second destination" (InMemoryFS.withFailOnRename [osp|/ssd2/card/a.mxf|]))
      { destinations = [ssd1, ssd2]
      , finals = [(newA, Just "data"), ([osp|/ssd2/card/a.mxf|], Nothing)]
      , manifests = [([osp|/ssd1/card|], Just [FileEntry.Failed]), ([osp|/ssd2/card|], Just [FileEntry.Failed])]
      }
  , (fresh "the read-back gives another hash" (InMemoryFS.withCorruptRead newA))
      { finals = [(newA, Just "data")]
      , statuses = [(RelPath.RelPath "a.mxf", isMismatch)]
      }
  , (fresh "one source file of two cannot be read" (\fs -> fs & InMemoryFS.withFile [osp|/card/b.mxf|] "2" & InMemoryFS.withFailOnOpen [osp|/card/a.mxf|]))
      { finals = [([osp|/ssd1/card/b.mxf|], Just "2")]
      , manifests = [([osp|/ssd1/card|], Just [FileEntry.Original])]
      }
  , (fresh "the manifest cannot be written" InMemoryFS.withFailOnManifestWrite)
      { finals = [(newA, Just "data")]
      , manifests = [([osp|/ssd1/card|], Nothing)]
      , empty = [[osp|/ssd1/card/ascmhl|]]
      , ended = \case
          JobState.Failed _ -> True
          _ -> False
      }
  ]
  where
    oldB = [osp|/ssd1/card/b.txt|]
    newA = [osp|/ssd1/card/a.mxf|]
    overwrite label inject =
      (base label (InMemoryFS.emptyMemFS & InMemoryFS.withFile [osp|/card/b.txt|] "hi" & InMemoryFS.withFile oldB "old" & inject)) {replace = True}
    fresh label inject = base label (InMemoryFS.emptyMemFS & InMemoryFS.withFile [osp|/card/a.mxf|] "data" & inject)
    base label seed =
      Fault
        { label
        , seed
        , destinations = [ssd1]
        , replace = False
        , finals = []
        , manifests = [([osp|/ssd1/card|], Just [FileEntry.Failed])]
        , ended = (== JobState.Finished (JobResult.WithFailures 1))
        , statuses = []
        , empty = []
        }
    isMismatch = \case
      FileStatus.Done (FileOutcome.HashMismatch _) -> True
      _ -> False

sealingRecordsOriginalThenVerifiedOrFailed :: Assertion
sealingRecordsOriginalThenVerifiedOrFailed = do
  once <- Harness.drive setup sealVol
  Harness.expectClean once
  actionsIn vol once.disk @?= Just [FileEntry.Original, FileEntry.Original]
  chainIn vol once.disk @?= Right [1]
  generationsOf (JobId.JobId 1) once @?= Just 1
  twice <- Harness.drive setup (sealVol <> [Harness.Alter (InMemoryFS.withFile volFile "HELLO")] <> sealVol)
  Harness.expectClean twice
  let plan = Map.lookup (JobId.JobId 2) twice.model.jobs >>= \entry -> entry.plan
  fmap (\found -> Finding.AlreadySealed `elem` codesOf found) plan @?= Just True
  fmap Plan.blocked plan @?= Just False
  actionsIn vol twice.disk @?= Just [FileEntry.Failed, FileEntry.Verified]
  chainIn vol twice.disk @?= Right [1, 2]
  generationsOf (JobId.JobId 2) twice @?= Just 2
  -- A third seal still compares with the original record, and the failed entry holds the hash the
  -- file has now.
  thrice <- Harness.drive setup {Harness.picks = [vol, vol, vol]} (sealVol <> [Harness.Alter (InMemoryFS.withFile volFile "HELLO")] <> sealVol <> sealVol)
  Harness.expectClean thrice
  actionsIn vol thrice.disk @?= Just [FileEntry.Failed, FileEntry.Verified]
  chainIn vol thrice.disk @?= Right [1, 2, 3]
  changed <- runEff (Hasher.run (Hasher.hashBytes Algo.XXH64 "HELLO"))
  (hashesIn vol thrice.disk >>= listToMaybe <&> (.hash)) @?= Just changed
  where
    setup =
      (Harness.rig (hello & InMemoryFS.withFile [osp|/vol/Sidecar/notes.txt|] ""))
        { Harness.picks = [vol, vol]
        }

verifyReportsEachDifference :: Assertion
verifyReportsEachDifference = do
  settled <- Harness.drive setup (sealVol <> [Harness.Alter changes, Harness.user (RunCommand Command.VerifyFolder), Harness.user ConfirmPlan, Harness.user (RunCommand Command.SaveReport)])
  Harness.expectClean settled
  let status rel = Map.lookup (JobId.JobId 2) settled.model.jobs >>= \entry -> Map.lookup (RelPath.RelPath rel) entry.state.files <&> (.status)
  status "same.mxf" @?= Just (FileStatus.Done FileOutcome.Ok)
  status "gone.mxf" @?= Just (FileStatus.Done FileOutcome.Missing)
  status "extra.txt" @?= Just (FileStatus.Done FileOutcome.New)
  case status "changed.mxf" of
    Just (FileStatus.Done (FileOutcome.HashMismatch _)) -> pure ()
    other -> assertFailure ("changed.mxf: " <> show other)
  phaseOf (JobId.JobId 2) settled @?= Just (JobState.Finished (JobResult.WithFailures 2))
  length (manifestsIn vol settled.disk) @?= 2
  generationsOf (JobId.JobId 2) settled @?= Just 2
  assertBool "the report is saved" (maybe False (not . BS.null) (bytesAt [osp|/out/report.txt|] settled.disk))
  where
    setup =
      ( Harness.rig
          ( InMemoryFS.emptyMemFS
              & InMemoryFS.withFile [osp|/vol/same.mxf|] "same"
              & InMemoryFS.withFile [osp|/vol/changed.mxf|] "before"
              & InMemoryFS.withFile [osp|/vol/gone.mxf|] "gone"
          )
      )
        { Harness.picks = [vol, vol, [osp|/out/report.txt|]]
        }
    changes fs =
      fs
        & withoutFile [osp|/vol/gone.mxf|]
        & InMemoryFS.withFile [osp|/vol/changed.mxf|] "after"
        & InMemoryFS.withFile [osp|/vol/extra.txt|] "new"

anOffloadContinuesTheSourceHistory :: Assertion
anOffloadContinuesTheSourceHistory = do
  continued <- Harness.drive (sealedVol hello) (sealVol <> offload vol [dest] <> [Harness.user ConfirmPlan])
  Harness.expectClean continued
  actionsIn destVol continued.disk @?= Just [FileEntry.Verified]
  let sourceManifests = manifestsIn vol continued.disk
  length sourceManifests @?= 1
  forM_ sourceManifests $ \path ->
    bytesAt (joined [osp|/dest/vol/ascmhl|] (makeRelative [osp|/vol/ascmhl|] path)) continued.disk @?= bytesAt path continued.disk
  chainIn destVol continued.disk @?= Right [1, 2]

  changed <- Harness.drive (sealedVol hello) (sealVol <> [Harness.Alter (InMemoryFS.withFile volFile "HELLO")] <> offload vol [dest] <> [Harness.user ConfirmPlan])
  Harness.expectClean changed
  phaseOf (JobId.JobId 2) changed @?= Just (JobState.Finished (JobResult.WithFailures 1))
  actionsIn destVol changed.disk @?= Just [FileEntry.Failed]
  expected <- runEff (Hasher.run (Hasher.hashBytes Algo.XXH64 "HELLO"))
  (hashesIn destVol changed.disk <&> fmap (.hash)) @?= Just [expected]

  md5 <- Harness.drive (Harness.rig (hello & InMemoryFS.withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] md5SealFixture)) (offload vol [dest] <> [Harness.user ConfirmPlan])
  Harness.expectClean md5
  (hashesIn destVol md5.disk <&> fmap (\found -> found.hash.algo)) @?= Just [Algo.MD5]
  actionsIn destVol md5.disk @?= Just [FileEntry.Verified]
  -- Another run seals the source between the plan and the run: the plan is stale, the job fails
  -- before it writes anything.
  raced <- Harness.drive (Harness.rig hello) (offload vol [dest] <> [Harness.Alter (InMemoryFS.withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] md5SealFixture), Harness.user ConfirmPlan])
  Harness.expectClean raced
  case phaseOf (JobId.JobId 1) raced of
    Just (JobState.Failed _) -> pure ()
    other -> assertFailure ("raced: " <> show other)
  filesUnder dest raced.disk @?= []
  where
    sealedVol disk = (Harness.rig disk) {Harness.picks = [vol]}

sealFirst :: Assertion
sealFirst = do
  unsealed <- Harness.drive (Harness.rig hello) (offload vol [dest] <> sealingFirst)
  Harness.expectClean unsealed
  unsealed.model.nextId @?= JobId.JobId 2
  phaseOf (JobId.JobId 1) unsealed @?= Just (JobState.Finished JobResult.AllOk)
  length (filesUnder [osp|/vol/ascmhl|] unsealed.disk) @?= 2
  actionsIn destVol unsealed.disk @?= Just [FileEntry.Verified]
  chainIn destVol unsealed.disk @?= Right [1, 2]

  sealed <- Harness.drive ((Harness.rig hello) {Harness.picks = [vol]}) (sealVol <> offload vol [dest] <> sealingFirst)
  Harness.expectClean sealed
  sealed.model.nextId @?= JobId.JobId 3
  phaseOf (JobId.JobId 2) sealed @?= Just (JobState.Finished JobResult.AllOk)
  chainIn vol sealed.disk @?= Right [1, 2]
  chainIn destVol sealed.disk @?= Right [1, 2, 3]

  wrong <- Harness.drive (Harness.rig (hello & InMemoryFS.withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] wrongSealFixture)) (offload vol [dest] <> sealingFirst)
  Harness.expectClean wrong
  case phaseOf (JobId.JobId 1) wrong of
    Just (JobState.Failed _) -> pure ()
    other -> assertFailure ("wrong seal: " <> show other)
  filesUnder dest wrong.disk @?= []
  length (filesUnder [osp|/vol/ascmhl|] wrong.disk) @?= 3
  where
    sealingFirst = [Harness.user (SetSealFirst (Offload.SealBeforeCopy Offload.StopBeforeCopy)), Harness.user ConfirmPlan]

data Expect = Blocks | OnlyWarns | NotRaised
  deriving stock (Show)

data Raised = Code Finding.Code | PluginBlocker
  deriving stock (Show)

-- | The read-back of the copy gives another hash, and the bad copy stays. Each later verify
-- records a @failed@ entry with the hash it computed. The second verify must still compare with
-- the first hash, so the copy never passes.
aFailedCopyNeverPassesALaterVerify :: Assertion
aFailedCopyNeverPassesALaterVerify = do
  settled <- Harness.drive setup (offload card [ssd1] <> [Harness.user ConfirmPlan] <> verify <> verify)
  Harness.expectClean settled
  phaseOf (JobId.JobId 1) settled @?= Just (JobState.Finished (JobResult.WithFailures 1))
  phaseOf (JobId.JobId 2) settled @?= Just (JobState.Finished (JobResult.WithFailures 1))
  phaseOf (JobId.JobId 3) settled @?= Just (JobState.Finished (JobResult.WithFailures 1))
  let status = Map.lookup (JobId.JobId 3) settled.model.jobs >>= \entry -> Map.lookup (RelPath.RelPath "a.mxf") entry.state.files <&> (.status)
  case status of
    Just (FileStatus.Done (FileOutcome.HashMismatch _)) -> pure ()
    other -> assertFailure ("a.mxf at the second verify: " <> show other)
  chainIn copy settled.disk @?= Right [1, 2, 3]
  actionsIn copy settled.disk @?= Just [FileEntry.Failed]
  where
    copy = [osp|/ssd1/card|]
    setup =
      ( Harness.rig
          ( InMemoryFS.emptyMemFS
              & InMemoryFS.withFile [osp|/card/a.mxf|] "data"
              & InMemoryFS.withCorruptRead [osp|/ssd1/card/a.mxf|]
          )
      )
        { Harness.picks = [copy, copy]
        }
    verify = [Harness.user (RunCommand Command.VerifyFolder), Harness.user ConfirmPlan]

-- * Shared

card, ssd1, ssd2, ssd3, vol, dest, destVol, volFile :: OsPath
card = [osp|/card|]
ssd1 = [osp|/ssd1|]
ssd2 = [osp|/ssd2|]
ssd3 = [osp|/ssd3|]
vol = [osp|/vol|]
dest = [osp|/dest|]
destVol = [osp|/dest/vol|]
volFile = [osp|/vol/A002C001.MXF|]

hello :: MemFS
hello = InMemoryFS.emptyMemFS & InMemoryFS.withFile volFile "hello"

-- | A new offload from the source to the destinations, up to the plan on offer.
offload :: OsPath -> List OsPath -> List Act
offload source destinations =
  [Harness.user (RunCommand Command.NewOffload), Harness.Send (SourcePicked source)]
    <> fmap (Harness.Send . DestinationPicked) destinations
    <> [Harness.user ReviewPlan]

-- | A seal of the folder that the next pick names, up to the end of the job.
sealVol :: List Act
sealVol = [Harness.user (RunCommand Command.SealMedia), Harness.user ConfirmPlan]

phaseOf :: JobId -> Settled -> Maybe JobState.Phase
phaseOf jobId settled = Map.lookup jobId settled.model.jobs <&> \entry -> entry.state.phase

generationsOf :: JobId -> Settled -> Maybe Int
generationsOf jobId settled =
  Map.lookup jobId settled.model.jobs >>= \entry -> entry.history <&> \history -> V.length history.generations

lastEventOf :: JobId -> Settled -> Maybe JobEvent
lastEventOf jobId settled =
  settled.events
    & filter (\pair -> fst pair == jobId)
    & reverse
    & listToMaybe
    <&> snd

statusOf :: RelPath -> JobEvent -> Maybe FileStatus
statusOf rel = \case
  JobEvent.FileStatusChanged path status | path == rel -> Just status
  _ -> Nothing

codesOf :: Plan -> List Finding.Code
codesOf plan = plan.findings & V.toList & fmap (.code)

-- | A path below a root, with the slashes of the in-memory disk.
joined :: OsPath -> OsPath -> OsPath
joined root rel = InMemoryFS.slashedPath (root </> rel)

relAt :: OsPath -> RelPath -> OsPath
relAt root (RelPath.RelPath rel) = joined root (unsafeEncodeUtf (T.unpack rel))

under :: OsPath -> OsPath -> Bool
under root path = (root <> [osp|/|]) `isPrefixOf` path

filesUnder :: OsPath -> MemFS -> List OsPath
filesUnder root fs = Map.keys fs.files & filter (under root)

bytesAt :: OsPath -> MemFS -> Maybe ByteString
bytesAt path fs = Map.lookup path fs.files <&> fst

withoutFile :: OsPath -> MemFS -> MemFS
withoutFile path fs = fs {InMemoryFS.files = Map.delete path fs.files}

manifestsIn :: OsPath -> MemFS -> List OsPath
manifestsIn root fs =
  filesUnder (joined root [osp|ascmhl|]) fs
    & filter (\path -> [osp|.mhl|] `isSuffixOf` path)

hashesIn :: OsPath -> MemFS -> Maybe (List FileEntry.HashRecord)
hashesIn root fs =
  InMemoryFS.newestManifestIn root fs <&> \manifest ->
    manifest.entries
      & ManifestEntry.files
      & V.toList
      & sortOn (.path)
      & mapMaybe (\entry -> entry.hashes V.!? 0)

actionsIn :: OsPath -> MemFS -> Maybe (List FileEntry.Action)
actionsIn root fs = hashesIn root fs <&> fmap (.action)

chainIn :: OsPath -> MemFS -> Either Text (List Int)
chainIn root fs = case Map.lookup (joined root [osp|ascmhl/ascmhl_chain.xml|]) fs.files of
  Nothing -> Left "no chain"
  Just (bytes, _) ->
    TE.decodeUtf8 bytes
      & Chain.parse
      & second (\chain -> chain.entries & V.map (.sequenceNr) & V.toList)

wrongSealFixture :: Text
wrongSealFixture =
  """
  <?xml version="1.0" encoding="UTF-8"?>
  <hashlist version="2.0" xmlns="urn:ASC:MHL:v2.0">
    <creatorinfo>
      <creationdate>2026-05-09T17:02:00+00:00</creationdate>
      <hostname>dit-laptop</hostname>
      <tool version="0.4.0">ascmhl</tool>
    </creatorinfo>
    <processinfo>
      <process>in-place</process>
      <ignore>
        <pattern>.DS_Store</pattern>
        <pattern>ascmhl</pattern>
      </ignore>
    </processinfo>
    <hashes>
      <hash>
        <path size="5" lastmodificationdate="1970-01-01T00:00:00+00:00">A002C001.MXF</path>
        <md5 action="original" hashdate="2026-05-09T17:02:00+00:00">00000000000000000000000000000000</md5>
      </hash>
    </hashes>
  </hashlist>
  """
    <> "\n"

md5SealFixture :: Text
md5SealFixture =
  """
  <?xml version="1.0" encoding="UTF-8"?>
  <hashlist version="2.0" xmlns="urn:ASC:MHL:v2.0">
    <creatorinfo>
      <creationdate>2026-05-09T17:02:00+00:00</creationdate>
      <hostname>dit-laptop</hostname>
      <tool version="0.4.0">ascmhl</tool>
    </creatorinfo>
    <processinfo>
      <process>in-place</process>
      <ignore>
        <pattern>.DS_Store</pattern>
        <pattern>ascmhl</pattern>
      </ignore>
    </processinfo>
    <hashes>
      <hash>
        <path size="5" lastmodificationdate="1970-01-01T00:00:00+00:00">A002C001.MXF</path>
        <md5 action="original" hashdate="2026-05-09T17:02:00+00:00">5d41402abc4b2a76b9719d911017c592</md5>
      </hash>
    </hashes>
  </hashlist>
  """
    <> "\n"
