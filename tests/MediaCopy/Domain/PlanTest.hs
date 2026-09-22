{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Domain.PlanTest (tests) where

import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Layout (chainPath)
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read (parseManifest)
import Ascmhl.Types (Chain (..), ChainEntry (..), HashEntry (..), Manifest (..), ProcessKind (..), fileEntries)
import Ascmhl.Write (renderChain)
import Control.Monad (forM_, when)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.List (List, sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff)
import Effectful.Time (Time, runTime)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (OsPath, unsafeEncodeUtf)
import splice System.OsPath (osp)
import System.OsString (isPrefixOf, isSuffixOf)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.FileSystem (Tree (..), partPath)
import MediaCopy.Domain.History (HistoryError (..))
import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat.Internal (JobFormat (..))
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Preflight
import MediaCopy.Effects.Emit (Emit, runEmitCollect)
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.Hasher (Hasher, runHasherIO)
import MediaCopy.Engine (defaultToolInfo, executePlan, planJob, runJob)
import MediaCopy.Test.InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Domain.Plan"
    [ testCase "a warning alone does not block a plan" warningDoesNotBlock
    , testCase "free space alone does not make two plans differ" freeSpaceIsNotAChange
    , testCase "a missing source blocks an offload" sourceMissingBlocks
    , testCase "a destination holding a foreign file blocks an offload" destinationWithForeignFileBlocks
    , testCase "too little space blocks an offload" insufficientSpaceBlocks
    , testCase "a chain that names no manifest blocks an offload" chainNamesNoManifestBlocks
    , testCase "a chain file that names nothing blocks an offload, read through the store" chainNamesNoManifestBlocksThroughTheStore
    , testCase "a folder with no history blocks a verify" verifyWithoutHistoryBlocks
    , testCase "a seal of a sealed folder warns and does not block" sealOfSealedFolderWarns
    , testCase "a seal of a sealed folder warns, read through the store" sealOfSealedFolderWarnsThroughTheStore
    , testCase "a seal-first offload plans a seal pass and leaves the payload alone" sealFirstPlansASealPass
    , testCase "two walks over one file system give one plan" twoWalksOverOneFileSystemAgree
    , testCase "a destination's generation follows the media source's history" destinationGenerationFollowsTheSourceHistory
    , testCase "a foreign file at the destination blocks an offload" foreignFileBlocks
    , testCase "a foreign directory at the destination blocks an offload" foreignDirectoryBlocks
    , testCase "a destination generation the source does not hold blocks an offload" extraGenerationBlocks
    , testCase "a clean partial destination blocks until the operator chooses" partialDestinationAwaitsAChoice
    , testCase "a shared generation with another c4 blocks an offload" otherSourceBlocks
    , testCase "a destination chain that is a prefix of the source chain passes" prefixChainPasses
    , testCase "write modes follow the choice and the size" writeModesFollowTheChoiceAndTheSize
    , testCase "an all-reuse step with a recorded original reads no source byte" reuseWithRecordedOriginalReadsNoSource
    , testGroup
        "planVerify"
        [ testCase "classifies manifest-only, disk-only, and shared paths" classifiesManifestOnlyDiskOnlyAndSharedPaths
        ]
    , testGroup
        "properties"
        [ testProperty "the plan's bytes are the engine's bytes, and the manifest names every file" planAndEngineAgree
        ]
    ]

classifiesManifestOnlyDiskOnlyAndSharedPaths :: Assertion
classifiesManifestOnlyDiskOnlyAndSharedPaths = do
  let manifest = Map.fromList [(RelPath "a", sampleHash), (RelPath "gone", sampleHash)]
      disk = V.fromList [RelPath "a", RelPath "extra"]
  planVerify manifest disk
    @?= V.fromList
      [(RelPath "a", VerifyAgainst sampleHash), (RelPath "extra", ReportNew), (RelPath "gone", ReportMissing)]

sampleHash :: Hash
sampleHash = Hash XXH64 "ef46db3751d8e999"

samplePlan :: Vector Finding -> JobPlan
samplePlan = samplePlanWithFree 1_000_000

samplePlanWithFree :: Int64 -> Vector Finding -> JobPlan
samplePlanWithFree free findings =
  JobPlan
    { spec = verifySpec [osp|/media-source|]
    , execution =
        RecordAt
          RecordPass
            { folder = [osp|/media-source|]
            , process = ProcessInPlace
            , generation = PlannedGeneration {folder = [osp|/media-source|], number = 1, manifest = [osp|/manifest|], process = ProcessInPlace, directories = V.empty}
            }
    , targets = V.singleton Target {root = [osp|/media-source|], freeBytes = Just free, state = Fresh}
    , format = Just (JobFormat XXH64)
    , originsUsed = "none"
    , steps = V.empty
    , totalBytes = 0
    , bytesToRead = 0
    , creates = V.empty
    , ignorePatterns = V.empty
    , sealPass = Nothing
    , generations = 0
    , findings
    }

warningDoesNotBlock :: Assertion
warningDoesNotBlock = do
  let plan = samplePlan (V.singleton Finding {severity = Warning, code = AlreadySealed, detail = "generation 3"})
  planBlocked plan @?= False
  V.null (blockers plan) @?= True

freeSpaceIsNotAChange :: Assertion
freeSpaceIsNotAChange = do
  let a = samplePlan V.empty
      b = samplePlanWithFree (1_000_000 - 4096) V.empty
  planEquivalent a b @?= True

sourceMissingBlocks :: Assertion
sourceMissingBlocks = do
  let job = offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory
      plan = decideOffload (specOf (Offload job)) job (offloadFacts Nothing (V.singleton (freshTarget [osp|/ssd1|] 1_000_000)))
  assertBool "expected SourceMissing" (SourceMissing `elem` codesOf plan)
  planBlocked plan @?= True

destinationWithForeignFileBlocks :: Assertion
destinationWithForeignFileBlocks = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "hi"
          & withFile [osp|/ssd1/media-source/already.txt|] "there"
      )
  plan <- runEff (runFileSystemMem ref (planJob (specOf (Offload (offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory)))))
  assertBool "expected DestinationForeign" (DestinationForeign `elem` codesOf plan)
  planBlocked plan @?= True

insufficientSpaceBlocks :: Assertion
insufficientSpaceBlocks = do
  let job = offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory
      facts = offloadFacts (Just (treeOf [(RelPath "big.mxf", 9000)] [])) (V.singleton (freshTarget [osp|/ssd1|] 10))
      plan = decideOffload (specOf (Offload job)) job facts
  assertBool "expected InsufficientSpace" (InsufficientSpace `elem` codesOf plan)
  planBlocked plan @?= True

chainNamesNoManifestBlocks :: Assertion
chainNamesNoManifestBlocks = do
  let job = offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory
      facts =
        OffloadFacts
          { sourceTree = Just (treeOf [(RelPath "a.mxf", 2)] [])
          , targets = V.singleton (freshTarget [osp|/ssd1|] 1_000_000)
          , originals = Left (ChainEmpty (chainPath [osp|/media-source|]))
          , sourceHistory = Right Nothing
          }
      plan = decideOffload (specOf (Offload job)) job facts
  assertBool "expected ChainNamesNoManifest" (ChainNamesNoManifest `elem` codesOf plan)
  planBlocked plan @?= True

verifyWithoutHistoryBlocks :: Assertion
verifyWithoutHistoryBlocks = do
  let facts = generationFacts (Just (treeOf [(RelPath "a.mxf", 2)] [])) emptyChain Map.empty
      plan = decideGeneration (verifySpec [osp|/media-source|]) RequireHistory facts
  assertBool "expected NoSeal" (NoSeal `elem` codesOf plan)
  planBlocked plan @?= True

sealOfSealedFolderWarns :: Assertion
sealOfSealedFolderWarns = do
  let facts = generationFacts (Just (treeOf [(RelPath "a.mxf", 2)] [])) sealedChain (Map.singleton (RelPath "a.mxf") sampleHash)
      plan = decideGeneration (sealSpec [osp|/media-source|]) AllowFresh facts
  assertBool "expected AlreadySealed" (AlreadySealed `elem` codesOf plan)
  planBlocked plan @?= False

chainNamesNoManifestBlocksThroughTheStore :: Assertion
chainNamesNoManifestBlocksThroughTheStore = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "hi"
          & withTextFile (chainPath [osp|/media-source|]) (renderChain emptyChain)
      )
  plan <- runEff (runFileSystemMem ref (planJob (specOf (Offload (offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory)))))
  assertBool "expected ChainNamesNoManifest" (ChainNamesNoManifest `elem` codesOf plan)
  planBlocked plan @?= True

sealOfSealedFolderWarnsThroughTheStore :: Assertion
sealOfSealedFolderWarnsThroughTheStore = do
  manifest <- TIO.readFile "tests/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"
  chain <- TIO.readFile "tests/fixtures/ascmhl/ascmhl_chain.xml"
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/A002C001.MXF|] "Nobody inspects the spammish repetition"
          & withTextFile [osp|/vol/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl|] manifest
          & withTextFile [osp|/vol/ascmhl/ascmhl_chain.xml|] chain
      )
  plan <- runEff (runFileSystemMem ref (planJob (sealSpec [osp|/vol|])))
  assertBool "expected AlreadySealed" (AlreadySealed `elem` codesOf plan)
  planBlocked plan @?= False

sealFirstPlansASealPass :: Assertion
sealFirstPlansASealPass = do
  let source = [osp|/media-source|]
      facts = offloadFacts (Just (treeOf [(RelPath "a.mxf", 900)] [])) (V.singleton (freshTarget [osp|/ssd1|] 1_000_000))
      plainJob = offloadJob source [[osp|/ssd1|]] UseHistory
      sealingJob = offloadJob source [[osp|/ssd1|]] (SealBeforeCopy StopBeforeCopy)
      plain = decideOffload (specOf (Offload plainJob)) plainJob facts
      sealing = decideOffload (specOf (Offload sealingJob)) sealingJob facts
  plain.sealPass @?= Nothing
  fmap (\pass -> pass.bytes) sealing.sealPass @?= Just 900
  fmap (\pass -> V.length pass.steps) sealing.sealPass @?= Just 1
  fmap (\pass -> pass.onFailure) sealing.sealPass @?= Just StopBeforeCopy
  sealing.totalBytes @?= plain.totalBytes

twoWalksOverOneFileSystemAgree :: Assertion
twoWalksOverOneFileSystemAgree = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/b.txt|] "2"
          & withFile [osp|/media-source/A/1.mxf|] "hello"
          & withFile [osp|/media-source/A/Sub/2.mxf|] "world"
          & withFile [osp|/media-source/a.txt|] "1"
      )
  let spec = specOf (Offload (offloadJob [osp|/media-source|] [[osp|/ssd1|], [osp|/ssd2|]] UseHistory))
  first <- runEff (runFileSystemMem ref (planJob spec))
  second <- runEff (runFileSystemMem ref (planJob spec))
  first @?= second
  V.length first.steps @?= 4

destinationGenerationFollowsTheSourceHistory :: Assertion
destinationGenerationFollowsTheSourceHistory = do
  let source = [osp|/media-source|]
      recorded = Map.singleton (RelPath "a.mxf") sampleHash
      facts =
        OffloadFacts
          { sourceTree = Just (treeOf [(RelPath "a.mxf", 2)] [])
          , targets = V.singleton (freshTarget [osp|/ssd1|] 1_000_000)
          , originals = Right (Just recorded)
          , sourceHistory = Right (Just (sealedChain, recorded))
          }
      plainJob = offloadJob source [[osp|/ssd1|]] UseHistory
      sealingJob = offloadJob source [[osp|/ssd1|]] (SealBeforeCopy StopBeforeCopy)
      plain = decideOffload (specOf (Offload plainJob)) plainJob facts
      sealing = decideOffload (specOf (Offload sealingJob)) sealingJob facts
  destinationNumbers plain @?= [2]
  destinationNumbers sealing @?= [3]
  fmap (\pass -> pass.generation.number) sealing.sealPass @?= Just 2
  planRaceChecks plain @?= V.singleton (source, 2)
  planRaceChecks sealing @?= V.singleton (source, 2)

destinationNumbers :: JobPlan -> List Int
destinationNumbers plan = case plan.execution of
  CopyInto copy -> copy.generations & V.toList & map (\planned -> planned.number)
  RecordAt _ -> []

partialSource :: Tree
partialSource = treeOf [(RelPath "A/1.mxf", 9000), (RelPath "b.txt", 2)] [RelPath "A"]

partialTarget :: Tree -> Chain -> TargetFacts
partialTarget held chain =
  TargetFacts
    { root = destinationPath [osp|/ssd1|] [osp|/media-source|]
    , freeBytes = Just 1_000_000
    , history = Right (Just chain)
    , existing = Just held
    }

partialPlan :: Maybe ExistingCopy -> Tree -> Chain -> Chain -> JobPlan
partialPlan choice held destChain sourceChain =
  decideOffload (specOf (Offload job)) job facts
  where
    job = (offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory) {existingCopy = choice}
    facts =
      OffloadFacts
        { sourceTree = Just partialSource
        , targets = V.singleton (partialTarget held destChain)
        , originals = Right Nothing
        , sourceHistory = Right (Just (sourceChain, Map.empty))
        }

foreignFileBlocks :: Assertion
foreignFileBlocks = do
  let plan = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2), (RelPath "stray.txt", 1)] []) emptyChain emptyChain
  assertBool "expected DestinationForeign" (DestinationForeign `elem` codesOf plan)
  detailsOf DestinationForeign plan @?= ["stray.txt"]

foreignDirectoryBlocks :: Assertion
foreignDirectoryBlocks = do
  let plan = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2)] [RelPath "Other"]) emptyChain emptyChain
  assertBool "expected DestinationForeign" (DestinationForeign `elem` codesOf plan)
  detailsOf DestinationForeign plan @?= ["Other"]

extraGenerationBlocks :: Assertion
extraGenerationBlocks = do
  let plan = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2)] []) sealedChain emptyChain
  assertBool "expected DestinationForeign" (DestinationForeign `elem` codesOf plan)
  detailsOf DestinationForeign plan @?= ["ascmhl/0001_media-source_2020-01-01_000000.mhl"]

partialDestinationAwaitsAChoice :: Assertion
partialDestinationAwaitsAChoice = do
  let undecided = partialPlan Nothing (treeOf [(RelPath "b.txt", 2), (RelPath "A/1.mxf.mc3k-part", 400)] []) emptyChain emptyChain
      decided = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2), (RelPath "A/1.mxf.mc3k-part", 400)] []) emptyChain emptyChain
  map (\target -> target.state) (V.toList undecided.targets) @?= [Partial]
  assertBool "expected DestinationPartial" (DestinationPartial `elem` codesOf undecided)
  planBlocked undecided @?= True
  assertBool "no DestinationPartial once chosen" (DestinationPartial `notElem` codesOf decided)
  planBlocked decided @?= False

otherSourceBlocks :: Assertion
otherSourceBlocks = do
  let plan = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2)] []) (chainWithC4 "aaaa") (chainWithC4 "bbbb")
  assertBool "expected DestinationOtherSource" (DestinationOtherSource `elem` codesOf plan)
  detailsOf DestinationOtherSource plan @?= ["0001_media-source_2020-01-01_000000.mhl"]

prefixChainPasses :: Assertion
prefixChainPasses = do
  let twoGenerations = Chain {entries = sealedChain.entries <> V.singleton ChainEntry {sequenceNr = 2, path = RelPath "0002_media-source_2020-01-02_000000.mhl", c4 = Nothing, unknown = V.empty}}
      plan = partialPlan (Just Resume) (treeOf [(RelPath "b.txt", 2)] []) sealedChain twoGenerations
  assertBool "no DestinationForeign" (DestinationForeign `notElem` codesOf plan)
  assertBool "no DestinationOtherSource" (DestinationOtherSource `notElem` codesOf plan)

writeModesFollowTheChoiceAndTheSize :: Assertion
writeModesFollowTheChoiceAndTheSize = do
  let held = treeOf [(RelPath "A/1.mxf", 10), (RelPath "b.txt", 2)] [RelPath "A"]
      resume = partialPlan (Just Resume) held emptyChain emptyChain
      replace = partialPlan (Just Replace) held emptyChain emptyChain
      absent = partialPlan (Just Resume) (treeOf [(RelPath "A/1.mxf.mc3k-part", 5)] [RelPath "A"]) emptyChain emptyChain
  modesOf resume @?= [(RelPath "A/1.mxf", Overwrite), (RelPath "b.txt", Reuse)]
  modesOf replace @?= [(RelPath "A/1.mxf", Overwrite), (RelPath "b.txt", Overwrite)]
  modesOf absent @?= [(RelPath "A/1.mxf", WriteNew), (RelPath "b.txt", WriteNew)]

reuseWithRecordedOriginalReadsNoSource :: Assertion
reuseWithRecordedOriginalReadsNoSource = do
  let held = treeOf [(RelPath "A/1.mxf", 9000), (RelPath "b.txt", 2)] [RelPath "A"]
      recorded = Map.fromList [(RelPath "A/1.mxf", sampleHash), (RelPath "b.txt", sampleHash)]
      job = (offloadJob [osp|/media-source|] [[osp|/ssd1|]] UseHistory) {existingCopy = Just Resume}
      facts =
        OffloadFacts
          { sourceTree = Just partialSource
          , targets = V.singleton (partialTarget held emptyChain)
          , originals = Right (Just recorded)
          , sourceHistory = Right (Just (sealedChain, recorded))
          }
      plan = decideOffload (specOf (Offload job)) job facts
  plan.bytesToRead @?= 9002
  plan.totalBytes @?= 9002

modesOf :: JobPlan -> List (RelPath, WriteMode)
modesOf plan =
  plan.steps
    & V.toList
    & concatMap (\step -> map (\w -> (step.path, w.mode)) (V.toList step.writes))

detailsOf :: FindingCode -> JobPlan -> List Text
detailsOf wanted plan =
  plan.findings
    & V.toList
    & filter (\finding -> finding.code == wanted)
    & map (\finding -> finding.detail)

chainWithC4 :: Text -> Chain
chainWithC4 value =
  Chain {entries = V.singleton ChainEntry {sequenceNr = 1, path = RelPath "0001_media-source_2020-01-01_000000.mhl", c4 = Just (Hash C4 value), unknown = V.empty}}

codesOf :: JobPlan -> List FindingCode
codesOf plan = plan.findings & V.toList & map (\finding -> finding.code)

treeOf :: List (RelPath, FileSize) -> List RelPath -> Tree
treeOf files dirs = Tree {files = V.fromList files, dirs = V.fromList dirs}

emptyChain :: Chain
emptyChain = Chain {entries = V.empty}

sealedChain :: Chain
sealedChain =
  Chain {entries = V.singleton ChainEntry {sequenceNr = 1, path = RelPath "0001_media-source_2020-01-01_000000.mhl", c4 = Nothing, unknown = V.empty}}

freshTarget :: OsPath -> Int64 -> TargetFacts
freshTarget parent free =
  TargetFacts
    { root = destinationPath parent [osp|/media-source|]
    , freeBytes = Just free
    , history = Right Nothing
    , existing = Nothing
    }

offloadFacts :: Maybe Tree -> Vector TargetFacts -> OffloadFacts
offloadFacts sourceTree targets =
  OffloadFacts {sourceTree, targets, originals = Right Nothing, sourceHistory = Right Nothing}

generationFacts :: Maybe Tree -> Chain -> Map.Map RelPath Hash -> GenerationFacts
generationFacts tree chain hashes =
  GenerationFacts {tree, freeBytes = Just 1_000_000, history = Right (Just (chain, hashes))}

offloadJob :: OsPath -> List OsPath -> SealFirst -> OffloadJob
offloadJob source dests sealFirst =
  OffloadJob {source, destinations = NE.fromList dests, sealFirst, existingCopy = Nothing}

specOf :: Job -> JobSpec
specOf job = JobSpec {jobId = JobId 1, job, createdAt = epoch}

verifySpec :: OsPath -> JobSpec
verifySpec folder = specOf (VerifyFolder VerifyJob {folder})

sealSpec :: OsPath -> JobSpec
sealSpec folder = specOf (SealMediaSource SealJob {folder})

data Held = NotHeld | PartOf Int | Same | Flipped | OtherSize
  deriving stock (Eq, Show)

data Destination = Destination {parent :: OsPath, held :: Maybe (List Held)}
  deriving stock (Show)

data Scenario = Scenario
  { files :: List (RelPath, ByteString)
  , destinations :: List Destination
  , choice :: Maybe ExistingCopy
  , sealed :: Bool
  }
  deriving stock (Show)

genScenario :: Gen Scenario
genScenario = do
  names <- Gen.subsequence candidatePaths & Gen.filter (\ps -> not (null ps))
  files <- traverse (\p -> Gen.bytes (Range.linear 0 9000) <&> \bs -> (p, bs)) names
  destCount <- Gen.int (Range.linear 1 2)
  destinations <- traverse (genDestination files) [1 .. destCount]
  let anyPartial = any (\d -> maybe False (any (/= NotHeld)) d.held) destinations
  choice <- if anyPartial then Just <$> Gen.element [Resume, Replace] else pure Nothing
  sealed <- Gen.bool
  pure Scenario {files, destinations, choice, sealed}

candidatePaths :: List RelPath
candidatePaths = map RelPath ["a.mxf", "B/1.mxf", "B/C/2.mxf", "d.txt", "B/e.mxf", "f.mov"]

genDestination :: List (RelPath, ByteString) -> Int -> Gen Destination
genDestination files index = do
  held <- Gen.maybe (traverse (\file -> genHeld (snd file)) files)
  pure Destination {parent = unsafeEncodeUtf ("/ssd" <> show index), held}

genHeld :: ByteString -> Gen Held
genHeld bs =
  Gen.frequency
    [ (3, pure NotHeld)
    , (1, PartOf <$> Gen.int (Range.linear 0 100))
    , (4, pure Same)
    , (1, pure (if BS.null bs then Same else Flipped))
    , (1, pure OtherSize)
    ]

seedScenario :: Scenario -> MemFS
seedScenario scenario = foldr seedDestination (foldr seedSource emptyMemFS scenario.files) scenario.destinations
  where
    seedSource file fs = withFile (memPath mediaSource (fst file)) (snd file) fs
    seedDestination dest fs = case dest.held of
      Nothing -> fs
      Just helds -> foldr (seedHeld (destinationRoot dest.parent)) (withDir (destinationRoot dest.parent) fs) (zip scenario.files helds)
    seedHeld root (file, h) fs =
      let target = memPath root (fst file)
          bs = snd file
      in case h of
           NotHeld -> fs
           PartOf percent -> withFile (partPath target) (BS.take (BS.length bs * percent `div` 100) bs) fs
           Same -> withFile target bs fs
           Flipped -> withFile target (flipFirst bs) fs
           OtherSize -> withFile target (bs <> "x") fs

flipFirst :: ByteString -> ByteString
flipFirst bs = case BS.uncons bs of
  Nothing -> "y"
  Just (b, rest) -> BS.cons (b `xor` 0xFF) rest

mediaSource :: OsPath
mediaSource = [osp|/media-source|]

memPath :: OsPath -> RelPath -> OsPath
memPath root (RelPath t) = root <> [osp|/|] <> unsafeEncodeUtf (T.unpack t)

destinationRoot :: OsPath -> OsPath
destinationRoot parent = parent <> [osp|/media-source|]

planAndEngineAgree :: Property
planAndEngineAgree = withTests 400 $ property $ do
  scenario <- forAll genScenario
  cover 0.5 "all reuse with a recorded original" (allReuseWithRecordedOriginal scenario)
  ref <- evalIO (newIORef (seedScenario scenario))
  when scenario.sealed $ do
    (_, sealEvents) <- evalIO (runEngine ref (runJob defaultToolInfo (sealSpec mediaSource)))
    annotateShow sealEvents
    lastEvent sealEvents === Just (JobFinished AllOk)
  let parents = map (\d -> d.parent) scenario.destinations
      job = (offloadJob mediaSource parents UseHistory) {existingCopy = scenario.choice}
  plan <- evalIO (runEff (runFileSystemMem ref (planJob (specOf (Offload job)))))
  annotateShow plan.findings
  annotateShow plan.steps
  assert (not (planBlocked plan))
  when scenario.sealed (assert (V.any isRecordedCopy plan.steps))
  (_, evs) <- evalIO (runEngine ref (executePlan defaultToolInfo plan))
  annotateShow evs
  lastProgress evs === Just (plan.bytesToRead + rewriteBytes scenario)
  lastEvent evs === Just (JobFinished AllOk)
  fs <- evalIO (readIORef ref)
  forM_ scenario.destinations $ \dest -> do
    let root = destinationRoot dest.parent
    forM_ scenario.files $ \file -> do
      fmap fst (Map.lookup (memPath root (fst file)) fs.files) === Just (snd file)
      Map.member (partPath (memPath root (fst file))) fs.files === False
    manifestPaths root fs === Just (sort (map fst scenario.files))
  forM_ scenario.files $ \file -> do
    let replaced = statusesOf (fst file) evs & filter isReplacedStatus & length
    replaced === (if fst file `elem` reusedMismatches scenario then 1 else 0)

flippedReuses :: Scenario -> List (RelPath, ByteString)
flippedReuses scenario
  | scenario.choice /= Just Resume = []
  | otherwise =
      concatMap
        ( \destination ->
            destination
              & (\d -> maybe [] (zip scenario.files) d.held)
              & filter (\pair -> snd pair == Flipped)
        )
        scenario.destinations
        & map fst

reusedMismatches :: Scenario -> List RelPath
reusedMismatches scenario = map fst (flippedReuses scenario)

rewriteBytes :: Scenario -> Int64
rewriteBytes scenario = sum (map (\file -> 2 * fromIntegral (BS.length (snd file))) (flippedReuses scenario))

allReuseWithRecordedOriginal :: Scenario -> Bool
allReuseWithRecordedOriginal scenario =
  scenario.sealed
    && scenario.choice == Just Resume
    && all (\d -> maybe False (all (\h -> h == Same || h == Flipped)) d.held) scenario.destinations

isRecordedCopy :: PlanStep -> Bool
isRecordedCopy step = case step.op of
  Copy (Recorded _) -> True
  _ -> False

lastEvent :: Vector JobEvent -> Maybe JobEvent
lastEvent evs = fmap snd (V.unsnoc evs)

runEngine :: IORef MemFS -> Eff '[Emit, Time, Hasher, FileSystem, IOE] a -> IO (a, Vector JobEvent)
runEngine ref body = runEff (runFileSystemMem ref (runHasherIO (runTime (runEmitCollect body))))

lastProgress :: Vector JobEvent -> Maybe Int64
lastProgress evs =
  evs
    & V.toList
    & mapMaybe progressOf
    & reverse
    & listToMaybe
  where
    progressOf (Progress n) = Just n
    progressOf _ = Nothing

statusesOf :: RelPath -> Vector JobEvent -> List FileStatus
statusesOf target evs = evs & V.toList & mapMaybe matchStatus
  where
    matchStatus (FileStatusChanged rel status) | rel == target = Just status
    matchStatus _ = Nothing

isReplacedStatus :: FileStatus -> Bool
isReplacedStatus = \case
  Done (Replaced _) -> True
  _ -> False

manifestPaths :: OsPath -> MemFS -> Maybe (List RelPath)
manifestPaths root fs = do
  newest <- newestManifestIn root fs
  manifest <- either (const Nothing) Just (parseManifest (TE.decodeUtf8 newest))
  pure (manifest.entries & fileEntries & V.toList & map (\entry -> entry.path) & sort)

newestManifestIn :: OsPath -> MemFS -> Maybe ByteString
newestManifestIn root fs =
  sortOn
    (Down . fst)
    ( fs.files
        & Map.toList
        & filter (\entry -> isManifestIn root (fst entry))
    )
    & listToMaybe
    & fmap (\entry -> fst (snd entry))

isManifestIn :: OsPath -> OsPath -> Bool
isManifestIn root p = (root <> [osp|/ascmhl/|]) `isPrefixOf` p && [osp|.mhl|] `isSuffixOf` p
