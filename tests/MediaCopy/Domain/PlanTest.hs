{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Domain.PlanTest (tests) where

import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Layout (chainPath)
import Ascmhl.Path (RelPath (..))
import Ascmhl.Types (Chain (..), ChainEntry (..), HashEntry (..), Manifest (..), ProcessKind (..), fileEntries)
import Ascmhl.Write (renderChain)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.List (List, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.FileSystem (Tree (..), partPath)
import MediaCopy.Domain.History (HistoryError (..))
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin (noPluginPlan)
import MediaCopy.Domain.Preflight
import MediaCopy.Effects.Emit (Emit, runEmitCollect)
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.Hasher (Hasher, runHasher)
import MediaCopy.Effects.Plugins (runPluginsNone)
import MediaCopy.Engine (executePlan, planJob)
import MediaCopy.Test.InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Plan"
    [ testProperty "the plan is stable, and its bytes are the engine's bytes" planAndEngineAgree
    , testCase "each finding blocks a plan or only warns" blockingRules
    , testCase "a destination continues the media source's history" destinationContinuesTheSourceHistory
    , testProperty "a verify sorts each path once" planVerifyPartitionsPaths
    ]

data Expect = Blocks | OnlyWarns | NotRaised
  deriving stock (Show)

blockingRules :: Assertion
blockingRules =
  forM_ rules $ \(name, getPlan, code, expect) -> do
    plan <- getPlan
    let present = code `elem` codesOf plan
        blocked = planBlocked plan
        holds = case expect of
          Blocks -> present && blocked
          OnlyWarns -> present && not blocked
          NotRaised -> not present && not blocked
    assertBool (name <> ": expected " <> show expect <> " " <> show code <> ", got " <> show (codesOf plan) <> " blocked=" <> show blocked) holds
  where
    offload = offloadJob mediaSource [[osp|/ssd1|]] UseHistory
    decide facts = pure (decideOffload (specOf (Offload offload)) offload facts)
    resumed held destChain sourceChain = pure (partialPlan (Just Resume) held destChain sourceChain)
    twoGenerations = Chain {entries = sealedChain.entries <> V.singleton ChainEntry {sequenceNr = 2, path = RelPath "0002_media-source_2020-01-02_000000.mhl", c4 = Nothing, unknown = V.empty}}
    halfCopied = treeOf [(RelPath "b.txt", 2), (RelPath "A/1.mxf.mc3k-part", 400)] []
    rules =
      [ ("a warning alone", pure (samplePlan (V.singleton Finding {severity = Warning, code = AlreadySealed, detail = ""})), AlreadySealed, OnlyWarns)
      , ("no media source", decide (offloadFacts Nothing (V.singleton (freshTarget [osp|/ssd1|] 1_000_000))), SourceMissing, Blocks)
      , ("a foreign file, read through the store", planOffloadIn (emptyMemFS & withFile [osp|/media-source/a.mxf|] "hi" & withFile [osp|/ssd1/media-source/already.txt|] "there"), DestinationForeign, Blocks)
      , ("too little space", decide (offloadFacts (Just (treeOf [(RelPath "big.mxf", 9000)] [])) (V.singleton (freshTarget [osp|/ssd1|] 10))), InsufficientSpace, Blocks)
      , ("a chain that names no manifest", decide (offloadFacts (Just (treeOf [(RelPath "a.mxf", 2)] [])) (V.singleton (freshTarget [osp|/ssd1|] 1_000_000))) {originals = Left (ChainEmpty (chainPath mediaSource))}, ChainNamesNoManifest, Blocks)
      , ("a chain that names no manifest, read through the store", planOffloadIn (emptyMemFS & withFile [osp|/media-source/a.mxf|] "hi" & withTextFile (chainPath mediaSource) (renderChain emptyChain)), ChainNamesNoManifest, Blocks)
      , ("a verify without history", pure (decideGeneration (verifySpec mediaSource) RequireHistory (generationFacts (Just (treeOf [(RelPath "a.mxf", 2)] [])) emptyChain Map.empty)), NoSeal, Blocks)
      , ("a seal of a sealed folder", pure (decideGeneration (sealSpec mediaSource) AllowFresh (generationFacts (Just (treeOf [(RelPath "a.mxf", 2)] [])) sealedChain (Map.singleton (RelPath "a.mxf") sampleHash))), AlreadySealed, OnlyWarns)
      , ("a seal of a sealed folder, read through the store", sealOfTheReferenceFolder, AlreadySealed, OnlyWarns)
      , ("a foreign file at a partial destination", resumed (treeOf [(RelPath "b.txt", 2), (RelPath "stray.txt", 1)] []) emptyChain emptyChain, DestinationForeign, Blocks)
      , ("a foreign directory at a partial destination", resumed (treeOf [(RelPath "b.txt", 2)] [RelPath "Other"]) emptyChain emptyChain, DestinationForeign, Blocks)
      , ("a destination generation the source does not hold", resumed (treeOf [(RelPath "b.txt", 2)] []) sealedChain emptyChain, DestinationForeign, Blocks)
      , ("a partial destination with no choice", pure (partialPlan Nothing halfCopied emptyChain emptyChain), DestinationPartial, Blocks)
      , ("a partial destination once the operator chose", resumed halfCopied emptyChain emptyChain, DestinationPartial, NotRaised)
      , ("a shared generation with another c4", resumed (treeOf [(RelPath "b.txt", 2)] []) (chainWithC4 "aaaa") (chainWithC4 "bbbb"), DestinationOtherSource, Blocks)
      , ("a destination chain that is a prefix of the source chain", resumed (treeOf [(RelPath "b.txt", 2)] []) sealedChain twoGenerations, DestinationForeign, NotRaised)
      , ("a destination chain that is a prefix of the source chain", resumed (treeOf [(RelPath "b.txt", 2)] []) sealedChain twoGenerations, DestinationOtherSource, NotRaised)
      ]

planOffloadIn :: MemFS -> IO JobPlan
planOffloadIn fs = do
  ref <- newIORef fs
  runEff (runFileSystemMem ref (planJob (specOf (Offload (offloadJob mediaSource [[osp|/ssd1|]] UseHistory)))))

sealOfTheReferenceFolder :: IO JobPlan
sealOfTheReferenceFolder = do
  manifest <- TIO.readFile "tests/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"
  chain <- TIO.readFile "tests/fixtures/ascmhl/ascmhl_chain.xml"
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/A002C001.MXF|] "Nobody inspects the spammish repetition"
          & withTextFile [osp|/vol/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl|] manifest
          & withTextFile [osp|/vol/ascmhl/ascmhl_chain.xml|] chain
      )
  runEff (runFileSystemMem ref (planJob (sealSpec [osp|/vol|])))

destinationContinuesTheSourceHistory :: Assertion
destinationContinuesTheSourceHistory = do
  let recorded = Map.singleton (RelPath "a.mxf") sampleHash
      facts =
        OffloadFacts
          { sourceTree = Just (treeOf [(RelPath "a.mxf", 900)] [])
          , targets = V.singleton (freshTarget [osp|/ssd1|] 1_000_000)
          , originals = Right (Just recorded)
          , sourceHistory = Right (Just (sealedChain, recorded))
          }
      plainJob = offloadJob mediaSource [[osp|/ssd1|]] UseHistory
      sealingJob = offloadJob mediaSource [[osp|/ssd1|]] (SealBeforeCopy StopBeforeCopy)
      plain = decideOffload (specOf (Offload plainJob)) plainJob facts
      sealing = decideOffload (specOf (Offload sealingJob)) sealingJob facts
  plain.sealPass @?= Nothing
  fmap (.bytes) sealing.sealPass @?= Just 900
  fmap (.onFailure) sealing.sealPass @?= Just StopBeforeCopy
  sealing.totalBytes @?= plain.totalBytes
  destinationNumbers plain @?= [2]
  destinationNumbers sealing @?= [3]
  fmap (.generation.number) sealing.sealPass @?= Just 2
  planRaceChecks plain @?= V.singleton (mediaSource, 2)
  planRaceChecks sealing @?= V.singleton (mediaSource, 2)

planVerifyPartitionsPaths :: Property
planVerifyPartitionsPaths = property $ do
  recorded <- forAll (Gen.subsequence candidatePaths)
  disk <- forAll (Gen.subsequence candidatePaths >>= Gen.shuffle)
  let manifest = recorded & map (\p -> (p, hashOf p)) & Map.fromList
      expected p
        | p `elem` recorded && p `elem` disk = VerifyAgainst (hashOf p)
        | p `elem` recorded = ReportMissing
        | otherwise = ReportNew
      result = planVerify manifest (V.fromList disk) & V.toList
  sort (map fst result) === Set.toList (Set.fromList (recorded <> disk))
  forM_ result $ \(p, op) -> op === expected p
  where
    hashOf (RelPath t) = Hash XXH64 t

destinationNumbers :: JobPlan -> List Int
destinationNumbers plan = case plan.execution of
  CopyInto copy -> copy.generations & V.toList & map (.number)
  RecordAt _ -> []

samplePlan :: Vector Finding -> JobPlan
samplePlan findings =
  JobPlan
    { spec = verifySpec mediaSource
    , execution =
        RecordAt
          RecordPass
            { folder = mediaSource
            , generation = PlannedGeneration {folder = mediaSource, number = 1, manifest = [osp|/manifest|], process = ProcessInPlace, directories = V.empty}
            }
    , targets = V.singleton Target {root = mediaSource, freeBytes = Just 1_000_000, state = Fresh}
    , format = Just XXH64
    , originsUsed = "none"
    , steps = V.empty
    , totalBytes = 0
    , bytesToRead = 0
    , creates = V.empty
    , ignorePatterns = V.empty
    , sealPass = Nothing
    , generations = 0
    , findings
    , plugins = noPluginPlan
    }

partialPlan :: Maybe ExistingCopy -> Tree -> Chain -> Chain -> JobPlan
partialPlan choice held destChain sourceChain =
  decideOffload (specOf (Offload job)) job facts
  where
    job = (offloadJob mediaSource [[osp|/ssd1|]] UseHistory) {existingCopy = choice}
    facts =
      OffloadFacts
        { sourceTree = Just (treeOf [(RelPath "A/1.mxf", 9000), (RelPath "b.txt", 2)] [RelPath "A"])
        , targets =
            V.singleton
              TargetFacts
                { root = destinationPath [osp|/ssd1|] mediaSource
                , freeBytes = Just 1_000_000
                , history = Right (Just destChain)
                , existing = Just held
                }
        , originals = Right Nothing
        , sourceHistory = Right (Just (sourceChain, Map.empty))
        }

chainWithC4 :: Text -> Chain
chainWithC4 value =
  Chain {entries = V.singleton ChainEntry {sequenceNr = 1, path = RelPath "0001_media-source_2020-01-01_000000.mhl", c4 = Just (Hash C4 value), unknown = V.empty}}

codesOf :: JobPlan -> List FindingCode
codesOf plan = plan.findings & V.toList & map (.code)

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
    { root = destinationPath parent mediaSource
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
           Flipped -> withFile target (corruptFirstByte bs) fs
           OtherSize -> withFile target (bs <> "x") fs

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
    (_, sealEvents) <- evalIO (runEngine ref (planJob (sealSpec mediaSource) >>= runPluginsNone . executePlan "localhost"))
    annotateShow sealEvents
    lastEvent sealEvents === Just (JobFinished AllOk)
  let parents = map (.parent) scenario.destinations
      job = (offloadJob mediaSource parents UseHistory) {existingCopy = scenario.choice}
  plan <- evalIO (runEff (runFileSystemMem ref (planJob (specOf (Offload job)))))
  again <- evalIO (runEff (runFileSystemMem ref (planJob (specOf (Offload job)))))
  again === plan
  annotateShow plan.findings
  annotateShow plan.steps
  assert (not (planBlocked plan))
  when scenario.sealed (assert (V.any isRecordedCopy plan.steps))
  (_, evs) <- evalIO (runEngine ref (runPluginsNone (executePlan "localhost" plan)))
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
    let replaced = statusesOf (fst file) evs & filter isReplaced & length
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
runEngine ref body = runEff (runFileSystemMem ref (runHasher (runTime (runEmitCollect body))))

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

manifestPaths :: OsPath -> MemFS -> Maybe (List RelPath)
manifestPaths root fs =
  newestManifestIn root fs <&> \manifest -> manifest.entries & fileEntries & V.toList & map (.path) & sort
