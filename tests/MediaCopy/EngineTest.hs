{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.EngineTest (tests) where

import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read (parseChain)
import Ascmhl.Types
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.List (List, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Time (runTime)
import System.OsPath (OsPath, makeRelative, takeDirectory, (</>))
import splice System.OsPath (osp)
import System.OsString (isPrefixOf, isSuffixOf)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Effects.Emit
import MediaCopy.Effects.Hasher
import MediaCopy.Effects.Plugins (runPluginsNone)
import MediaCopy.Engine
import MediaCopy.Test.InMemoryFS
import MediaCopy.Test.InMemoryFS qualified as Mem (MemFS (..))

tests :: TestTree
tests =
  testGroup
    "Engine"
    [ testCase "a fault never loses a good copy nor leaves a partial" aFaultNeverLosesAGoodCopyNorLeavesAPartial
    , testCase "per-file faults do not stop the job" perFileFaultsDoNotStopTheJob
    , testCase "a cancel leaves no partial" aCancelLeavesNoPartial
    , testCase "a manifest that cannot be written fails the job" aManifestThatCannotBeWrittenFailsTheJob
    , testCase "an offload continues the source history" anOffloadContinuesTheSourceHistory
    , testCase "seal-first stops before copy on failure" sealFirstStopsBeforeCopyOnFailure
    , testCase "sealing records original, then verified or failed" sealingRecordsOriginalThenVerifiedOrFailed
    , testCase "verify reports each kind of difference" verifyReportsEachKindOfDifference
    , testCase "a blocked plan writes nothing" aBlockedPlanWritesNothing
    ]

aFaultNeverLosesAGoodCopyNorLeavesAPartial :: Assertion
aFaultNeverLosesAGoodCopyNorLeavesAPartial =
  forM_ faultCases $ \(label, seeded, run, finals) -> do
    ref <- newIORef seeded
    evs <- run ref
    assertEqual (label <> ": job outcome") (JobFinished (WithFailures 1)) (V.last evs)
    fs <- readIORef ref
    assertEqual (label <> ": partials") [] (fs.files & Map.keys & filter (\k -> [osp|.mc3k-part|] `isSuffixOf` k))
    forM_ finals $ \(path, expected, recorded) -> do
      assertEqual (label <> ": final " <> show path) expected (Map.lookup path fs.files <&> fst)
      actions <- actionsOfLatestManifestIn (takeDirectory path) ref
      assertEqual (label <> ": manifest " <> show path) (Just recorded) actions

faultCases :: List (String, MemFS, IORef MemFS -> IO (Vector JobEvent), List (OsPath, Maybe ByteString, List HashAction))
faultCases =
  [
    ( "overwrite open fails"
    , oldCopy & withFailOnOpen [osp|/ssd1/media-source/b.txt.mc3k-part|]
    , \ref -> runResume ref Replace
    , [([osp|/ssd1/media-source/b.txt|], Just "old", [FailedAction])]
    )
  ,
    ( "rename fails on overwrite"
    , oldCopy & withFailOnRename [osp|/ssd1/media-source/b.txt|]
    , \ref -> runResume ref Replace
    , [([osp|/ssd1/media-source/b.txt|], Just "old", [FailedAction])]
    )
  ,
    ( "read-back fails on overwrite"
    , oldCopy & withFailOnOpen [osp|/ssd1/media-source/b.txt|]
    , \ref -> runResume ref Replace
    , [([osp|/ssd1/media-source/b.txt|], Just "hi", [FailedAction])]
    )
  ,
    ( "rename fails on new file"
    , newSource & withFailOnRename [osp|/ssd1/media-source/a.mxf|]
    , \ref -> runOffload ref [[osp|/ssd1|]]
    , [([osp|/ssd1/media-source/a.mxf|], Nothing, [FailedAction])]
    )
  ,
    ( "read-back fails on new file"
    , newSource & withFailOnOpen [osp|/ssd1/media-source/a.mxf|]
    , \ref -> runOffload ref [[osp|/ssd1|]]
    , [([osp|/ssd1/media-source/a.mxf|], Just "data", [FailedAction])]
    )
  ,
    ( "rename fails at the second destination"
    , newSource & withFailOnRename [osp|/ssd2/media-source/a.mxf|]
    , \ref -> runOffload ref [[osp|/ssd1|], [osp|/ssd2|]]
    , [([osp|/ssd1/media-source/a.mxf|], Just "data", [FailedAction]), ([osp|/ssd2/media-source/a.mxf|], Nothing, [FailedAction])]
    )
  ,
    ( "read-back hash mismatch"
    , newSource & withCorruptRead [osp|/ssd1/media-source/a.mxf|]
    , \ref -> runOffload ref [[osp|/ssd1|]]
    , [([osp|/ssd1/media-source/a.mxf|], Just "data", [FailedAction])]
    )
  ]
  where
    oldCopy = emptyMemFS & withFile [osp|/media-source/b.txt|] "hi" & withFile [osp|/ssd1/media-source/b.txt|] "old"
    newSource = emptyMemFS & withFile [osp|/media-source/a.mxf|] "data"

perFileFaultsDoNotStopTheJob :: Assertion
perFileFaultsDoNotStopTheJob = do
  ref <- newIORef (emptyMemFS & withFile [osp|/media-source/b|] "2" & withFile [osp|/media-source/a|] "1" & withFailOnOpen [osp|/media-source/a|])
  evs <- runOffload ref [[osp|/ssd1|]]
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/b|] fs.files) @?= Just "2"

aCancelLeavesNoPartial :: Assertion
aCancelLeavesNoPartial = do
  beforeRename <- runCancelledAt Flushing
  Map.member [osp|/ssd1/media-source/a.mxf|] beforeRename.files @?= False
  Map.member [osp|/ssd1/media-source/a.mxf.mc3k-part|] beforeRename.files @?= False
  afterRename <- runCancelledAt Verifying
  fmap fst (Map.lookup [osp|/ssd1/media-source/a.mxf|] afterRename.files) @?= Just "data"
  Map.member [osp|/ssd1/media-source/a.mxf.mc3k-part|] afterRename.files @?= False

aManifestThatCannotBeWrittenFailsTheJob :: Assertion
aManifestThatCannotBeWrittenFailsTheJob = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "data"
          & withFailOnManifestWrite
      )
  evs <- runOffload ref [[osp|/ssd1|]]
  assertBool "expected JobFailed" (V.any isJobFailed evs)
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/a.mxf|] fs.files) @?= Just "data"
  assertBool "no ascmhl under the destination" (not (any (\k -> [osp|/ssd1/media-source/ascmhl/|] `isPrefixOf` k) (Map.keys fs.files)))

anOffloadContinuesTheSourceHistory :: Assertion
anOffloadContinuesTheSourceHistory = do
  sealed <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runSeal' sealed
  _ <- runOffload' sealed
  actionsOfLatestManifestIn [osp|/dest/vol|] sealed >>= (@?= Just [Verified])
  fs <- readIORef sealed
  let manifestsUnder prefix = fs.files & Map.keys & filter (\k -> prefix `isPrefixOf` k && [osp|.mhl|] `isSuffixOf` k) & sort
      sourceManifests = manifestsUnder [osp|/vol/ascmhl/|]
  length sourceManifests @?= 1
  length (manifestsUnder [osp|/dest/vol/ascmhl/|]) @?= 2
  mapM_
    ( \k ->
        fmap fst (Map.lookup (slashedPath ([osp|/dest/vol/ascmhl|] </> makeRelative [osp|/vol/ascmhl|] k)) fs.files) @?= fmap fst (Map.lookup k fs.files)
    )
    sourceManifests
  case Map.lookup [osp|/dest/vol/ascmhl/ascmhl_chain.xml|] fs.files of
    Nothing -> assertFailure "the destination has no chain"
    Just (bytes, _) -> case parseChain (TE.decodeUtf8 bytes) of
      Left err -> assertFailure (show err)
      Right chain -> V.toList (V.map (.sequenceNr) chain.entries) @?= [1, 2]

  changed <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runSeal' changed
  modifyIORef' changed (withFile [osp|/vol/A002C001.MXF|] (BC.pack "HELLO"))
  evs <- runOffload' changed
  V.last evs @?= JobFinished (WithFailures 1)
  actionsOfLatestManifestIn [osp|/dest/vol|] changed >>= (@?= Just [FailedAction])
  values <- latestManifestHashesIn [osp|/dest/vol|] changed <&> fmap (\hs -> map (.hash.value) hs)
  values @?= Just ["26c7827d889f6da3"]

  md5 <- newIORef (emptyMemFS & withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] md5SealFixture & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runOffload' md5
  algos <- latestManifestHashesIn [osp|/dest/vol|] md5 <&> fmap (\hs -> map (.hash.algo) hs)
  algos @?= Just [MD5]
  actionsOfLatestManifestIn [osp|/dest/vol|] md5 >>= (@?= Just [Verified])

sealFirstStopsBeforeCopyOnFailure :: Assertion
sealFirstStopsBeforeCopyOnFailure = do
  good <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  goodEvs <- runSealFirstOffload good StopBeforeCopy
  V.last goodEvs @?= JobFinished AllOk
  goodFs <- readIORef good
  length (filesUnder [osp|/vol/ascmhl/|] goodFs) @?= 2
  actionsOfLatestManifestIn [osp|/dest/vol|] good >>= (@?= Just [Verified])

  bad <- newIORef (emptyMemFS & withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] wrongSealFixture & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  badEvs <- runSealFirstOffload bad StopBeforeCopy
  assertBool "expected JobFailed" (isJobFailed (V.last badEvs))
  badFs <- readIORef bad
  filesUnder [osp|/dest/|] badFs @?= []
  length (filesUnder [osp|/vol/ascmhl/|] badFs) @?= 3

sealingRecordsOriginalThenVerifiedOrFailed :: Assertion
sealingRecordsOriginalThenVerifiedOrFailed = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello")
          & withFile [osp|/vol/Sidecar/notes.txt|] (BC.pack "")
      )
  _ <- runSeal' ref
  actionsOfLatestManifestIn [osp|/vol|] ref >>= (@?= Just [Original, Original])
  modifyIORef' ref (withFile [osp|/vol/A002C001.MXF|] (BC.pack "HELLO"))
  _ <- runSeal' ref
  actionsOfLatestManifestIn [osp|/vol|] ref >>= (@?= Just [FailedAction, Verified])

verifyReportsEachKindOfDifference :: Assertion
verifyReportsEachKindOfDifference = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/same.mxf|] "same"
          & withFile [osp|/vol/changed.mxf|] "before"
          & withFile [osp|/vol/gone.mxf|] "gone"
      )
  _ <- runSeal' ref
  modifyIORef' ref (\fs -> fs {Mem.files = Map.delete [osp|/vol/gone.mxf|] fs.files})
  modifyIORef' ref (withFile [osp|/vol/changed.mxf|] "after")
  modifyIORef' ref (withFile [osp|/vol/extra.txt|] "new")
  evs <- runVerify' ref
  finalStatus (RelPath "same.mxf") evs @?= Just (Done Ok)
  finalStatus (RelPath "gone.mxf") evs @?= Just (Done Missing)
  finalStatus (RelPath "extra.txt") evs @?= Just (Done New)
  case finalStatus (RelPath "changed.mxf") evs of
    Just (Done (HashMismatch _)) -> pure ()
    other -> assertFailure ("changed.mxf: " <> show other)
  V.last evs @?= JobFinished (WithFailures 2)
  fs <- readIORef ref
  length (filter (\k -> [osp|.mhl|] `isSuffixOf` k) (Map.keys fs.files)) @?= 2

aBlockedPlanWritesNothing :: Assertion
aBlockedPlanWritesNothing = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "hi"
          & withFile [osp|/ssd1/media-source/already.txt|] "there"
      )
  before <- readIORef ref
  plan <- runEff (runFileSystemMem ref (planJob (offloadOneDest [osp|/media-source|] [osp|/ssd1|])))
  planBlocked plan @?= True
  (_, evs) <- runEff (runFileSystemMem ref (runHasher (runTime (runEmitCollect (runPluginsNone (executePlan "localhost" plan))))))
  assertBool "expected JobFailed" (V.any isJobFailed evs)
  afterwards <- readIORef ref
  Map.keys afterwards.files @?= Map.keys before.files

finalStatus :: RelPath -> Vector JobEvent -> Maybe FileStatus
finalStatus path evs = statusesOf path evs & reverse & listToMaybe

filesUnder :: OsPath -> MemFS -> List OsPath
filesUnder prefix fs = fs.files & Map.keys & filter (\k -> prefix `isPrefixOf` k)

runJobEvents :: IORef MemFS -> JobSpec -> IO (Vector JobEvent)
runJobEvents ref spec = do
  (_, evs) <- runEff (runFileSystemMem ref (runHasher (runTime (runEmitCollect (planJob spec >>= runPluginsNone . executePlan "localhost")))))
  pure evs

runOffloadWith :: IORef MemFS -> List OsPath -> Maybe ExistingCopy -> IO (Vector JobEvent)
runOffloadWith ref dests existingCopy =
  runJobEvents ref (specOf (Offload (offloadJob [osp|/media-source|] dests UseHistory) {existingCopy}))

runOffload :: IORef MemFS -> List OsPath -> IO (Vector JobEvent)
runOffload ref dests = runOffloadWith ref dests Nothing

runResume :: IORef MemFS -> ExistingCopy -> IO (Vector JobEvent)
runResume ref choice = runOffloadWith ref [[osp|/ssd1|]] (Just choice)

runOffload' :: IORef MemFS -> IO (Vector JobEvent)
runOffload' ref = runJobEvents ref (offloadOneDest [osp|/vol|] [osp|/dest|])

runSealFirstOffload :: IORef MemFS -> OnSealFailure -> IO (Vector JobEvent)
runSealFirstOffload ref policy =
  runJobEvents ref (specOf (Offload (offloadJob [osp|/vol|] [[osp|/dest|]] (SealBeforeCopy policy))))

runVerify' :: IORef MemFS -> IO (Vector JobEvent)
runVerify' ref = runJobEvents ref (specOf (VerifyFolder VerifyJob {folder = [osp|/vol|]}))

runSeal' :: IORef MemFS -> IO (Vector JobEvent)
runSeal' ref = runJobEvents ref (specOf (SealMediaSource SealJob {folder = [osp|/vol|]}))

offloadOneDest :: OsPath -> OsPath -> JobSpec
offloadOneDest source dest = specOf (Offload (offloadJob source [dest] UseHistory))

data CancelJob = CancelJob
  deriving stock (Show)

instance Exception CancelJob where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

runCancelledAt :: FileStatus -> IO MemFS
runCancelledAt status = do
  ref <- newIORef (emptyMemFS & withFile [osp|/media-source/a.mxf|] "data")
  let sink = \case
        FileStatusChanged _ s | s == status -> throwIO CancelJob
        _ -> pure ()
      spec = offloadOneDest [osp|/media-source|] [osp|/ssd1|]
  result <- try @CancelJob (runEff (runFileSystemMem ref (runHasher (runTime (runEmitIO sink (planJob spec >>= runPluginsNone . executePlan "localhost"))))))
  assertBool "expected the cancel to reach the caller" (either (const True) (const False) result)
  readIORef ref

latestManifestHashesIn :: OsPath -> IORef MemFS -> IO (Maybe (List ManifestHash))
latestManifestHashesIn root ref =
  readIORef ref <&> \fs ->
    newestManifestIn root fs <&> \m ->
      m.entries & fileEntries & V.toList & sortOn (.path) & mapMaybe (\e -> e.hashes V.!? 0)

actionsOfLatestManifestIn :: OsPath -> IORef MemFS -> IO (Maybe (List HashAction))
actionsOfLatestManifestIn root ref =
  latestManifestHashesIn root ref <&> \hashes -> fmap (\hs -> map (.action) hs) hashes

isJobFailed :: JobEvent -> Bool
isJobFailed (JobFailed _) = True
isJobFailed _ = False

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
