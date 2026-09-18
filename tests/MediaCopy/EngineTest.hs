{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.EngineTest (tests) where

import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Path (RelPath (..), pathText)
import Ascmhl.Read (parseChain, parseManifest)
import Ascmhl.Types
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.List (List, sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Time (runTime)
import System.OsPath (OsPath, makeRelative, (</>))
import splice System.OsPath (osp)
import System.OsString (isPrefixOf, isSuffixOf)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Effects.Emit
import MediaCopy.Effects.Hasher
import MediaCopy.Engine
import MediaCopy.Test.InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Engine"
    [ testGroup
        "runJob Offload"
        [ testCase "copies every file to every destination and verifies" copiesEveryFileToEveryDestinationAndVerifies
        , testCase "reports a hash mismatch when a destination read is corrupted" reportsAHashMismatchWhenADestinationReadIsCorrupted
        , testCase "reports IoError per file and keeps going" reportsIoErrorPerFileAndKeepsGoing
        , testCase "records verified when the source was sealed" recordsVerifiedWhenTheSourceWasSealed
        , testCase "records failed when the source changed after sealing" recordsFailedWhenTheSourceChangedAfterSealing
        , testCase "hashes in the algorithm of the originals" hashesInTheAlgorithmOfTheOriginals
        , testCase "seals the media source first and records verified" sealsTheMediaSourceFirstAndRecordsVerified
        , testCase "stops before copying when the seal fails" stopsBeforeCopyingWhenTheSealFails
        , testCase "reuses a destination file whose hash matches" reusesAMatchingDestinationFile
        , testCase "replaces a reused destination file whose hash differs" replacesAMismatchedDestinationFile
        , testCase "keeps the old final when an overwrite fails" keepsTheOldFinalWhenAnOverwriteFails
        , testCase "a rename that fails keeps the old final on an overwrite" renameFailureKeepsTheOldFinalOnOverwrite
        , testCase "a read-back that fails after publish keeps the new copy, flagged" readBackFailureAfterPublishKeepsTheNewCopyFlagged
        , testCase "a rename that fails on a new file leaves no name" renameFailureOnANewFileLeavesNoName
        , testCase "a manifest write that fails fails the job after the copies" manifestWriteFailureFailsTheJobAfterTheCopies
        ]
    , testGroup
        "runJob VerifyFolder"
        [ testCase "verifies, reports missing and new, writes a verify generation" verifiesReportsMissingAndNewWritesAVerifyGeneration
        , testCase "reports a mismatch" reportsAMismatch
        ]
    , testGroup
        "executePlan"
        [ testCase "a blocked plan writes nothing" blockedPlanWritesNothing
        , testCase "the planned steps are the files an offload copies" offloadStepsMatchWhatItCopies
        ]
    , testGroup
        "runJob Seal"
        [ testCase "carries the media source's history into the destination" carriesTheMediaSourceHistoryIntoTheDestination
        , testCase "seals a fresh folder with original actions" sealsAFreshFolderWithOriginalActions
        , testCase "seals again recording verified and failed" sealsAgainRecordingVerifiedAndFailed
        ]
    ]

copiesEveryFileToEveryDestinationAndVerifies :: Assertion
copiesEveryFileToEveryDestinationAndVerifies = do
  ref <- newIORef (withFile [osp|/media-source/A/1.mxf|] (BS.replicate 9000 1) (withFile [osp|/media-source/b.txt|] "hi" emptyMemFS))
  evs <- runOffload ref [[osp|/ssd1|], [osp|/ssd2|]]
  V.last evs @?= JobFinished AllOk
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/A/1.mxf|] fs.files) @?= Just (BS.replicate 9000 1)
  fmap fst (Map.lookup [osp|/ssd2/media-source/b.txt|] fs.files) @?= Just "hi"
  let mhlKeys = Map.keys (Map.filterWithKey (\k _ -> [osp|/ssd1/media-source/ascmhl/|] `isPrefixOf` k) fs.files)
  assertBool
    "expected exactly one .mhl and one ascmhl_chain.xml under /ssd1/media-source/ascmhl/"
    ( length mhlKeys == 2
        && any (\k -> [osp|.mhl|] `isSuffixOf` k) mhlKeys
        && any (\k -> [osp|ascmhl_chain.xml|] `isSuffixOf` k) mhlKeys
    )
  statusesOf (RelPath "A/1.mxf") evs @?= [Copying, Flushing, Publishing, Verifying, Done Ok]
  -- Each destination opens its own manifest window before its manifest lands. So the age the
  -- window reads is that destination's wait, not the two added together.
  manifestPhases evs @?= ["start", "written", "start", "written"]

reportsAHashMismatchWhenADestinationReadIsCorrupted :: Assertion
reportsAHashMismatchWhenADestinationReadIsCorrupted = do
  ref <- newIORef (emptyMemFS & withFile [osp|/media-source/a|] "data" & withCorruptRead [osp|/ssd1/media-source/a|])
  evs <- runOffload ref [[osp|/ssd1|]]
  V.last evs @?= JobFinished (WithFailures 1)
  assertBool "expected exactly one HashMismatch for \"a\"" (length (filter isHashMismatch (statusesOf (RelPath "a") evs)) == 1)

reportsIoErrorPerFileAndKeepsGoing :: Assertion
reportsIoErrorPerFileAndKeepsGoing = do
  ref <- newIORef (emptyMemFS & withFile [osp|/media-source/b|] "2" & withFile [osp|/media-source/a|] "1" & withFailOnOpen [osp|/media-source/a|])
  evs <- runOffload ref [[osp|/ssd1|]]
  assertBool "expected exactly one IoError for \"a\"" (length (filter isIoError (statusesOf (RelPath "a") evs)) == 1)
  statusesOf (RelPath "b") evs @?= [Copying, Flushing, Publishing, Verifying, Done Ok]
  V.last evs @?= JobFinished (WithFailures 1)

runOffloadWith :: IORef MemFS -> List OsPath -> Maybe ExistingCopy -> IO (Vector JobEvent)
runOffloadWith ref dests existingCopy =
  runJobEvents ref $
    JobSpec
      { jobId = JobId 1
      , job = Offload OffloadJob {source = [osp|/media-source|], destinations = NE.fromList dests, sealFirst = UseHistory, existingCopy}
      , createdAt = epoch
      }

runOffload :: IORef MemFS -> List OsPath -> IO (Vector JobEvent)
runOffload ref dests = runOffloadWith ref dests Nothing

-- | The destination already holds b.txt with the right bytes and A/1.mxf as a part file.
reusesAMatchingDestinationFile :: Assertion
reusesAMatchingDestinationFile = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/A/1.mxf|] (BS.replicate 9000 1)
          & withFile [osp|/media-source/b.txt|] "hi"
          & withFile [osp|/ssd1/media-source/b.txt|] "hi"
          & withFailOnOpen [osp|/ssd1/media-source/b.txt.mc3k-part|]
          & withFile [osp|/ssd1/media-source/A/1.mxf.mc3k-part|] (BS.replicate 400 1)
      )
  evs <- runResume ref Resume
  V.last evs @?= JobFinished AllOk
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/A/1.mxf|] fs.files) @?= Just (BS.replicate 9000 1)
  Map.member [osp|/ssd1/media-source/A/1.mxf.mc3k-part|] fs.files @?= False
  -- Every Copy op opens with Copying, whatever the modes of its writes are.
  statusesOf (RelPath "b.txt") evs @?= [Copying, Verifying, Done Ok]
  statusesOf (RelPath "A/1.mxf") evs @?= [Copying, Flushing, Publishing, Verifying, Done Ok]

replacesAMismatchedDestinationFile :: Assertion
replacesAMismatchedDestinationFile = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/b.txt|] "hi"
          & withFile [osp|/ssd1/media-source/b.txt|] "ho"
      )
  evs <- runResume ref Resume
  V.last evs @?= JobFinished AllOk
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/b.txt|] fs.files) @?= Just "hi"
  assertBool "expected exactly one Replaced for b.txt" (length (filter isReplaced (statusesOf (RelPath "b.txt") evs)) == 1)
  -- A rewrite is a whole second copy, so it reports every state a first copy reports. The first
  -- Verifying is the reuse check that found the mismatch; the outcome is asserted above.
  filter (not . isDone) (statusesOf (RelPath "b.txt") evs)
    @?= [Copying, Verifying, Copying, Flushing, Publishing, Verifying]

keepsTheOldFinalWhenAnOverwriteFails :: Assertion
keepsTheOldFinalWhenAnOverwriteFails = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/b.txt|] "hi"
          & withFile [osp|/ssd1/media-source/b.txt|] "old"
          & withFailOnOpen [osp|/ssd1/media-source/b.txt.mc3k-part|]
      )
  evs <- runResume ref Replace
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/b.txt|] fs.files) @?= Just "old"

runResume :: IORef MemFS -> ExistingCopy -> IO (Vector JobEvent)
runResume ref choice = runOffloadWith ref [[osp|/ssd1|]] (Just choice)

renameFailureKeepsTheOldFinalOnOverwrite :: Assertion
renameFailureKeepsTheOldFinalOnOverwrite = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/b.txt|] "hi"
          & withFile [osp|/ssd1/media-source/b.txt|] "old"
          & withFailOnRename [osp|/ssd1/media-source/b.txt|]
      )
  evs <- runResume ref Replace
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/b.txt|] fs.files) @?= Just "old"
  Map.member [osp|/ssd1/media-source/b.txt.mc3k-part|] fs.files @?= False
  assertBool "expected one IoError for b.txt" (length (filter isIoError (statusesOf (RelPath "b.txt") evs)) == 1)
  actions <- actionsOfLatestManifestIn [osp|/ssd1/media-source/ascmhl/|] ref
  actions @?= Just [FailedAction]

readBackFailureAfterPublishKeepsTheNewCopyFlagged :: Assertion
readBackFailureAfterPublishKeepsTheNewCopyFlagged = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/b.txt|] "hi"
          & withFile [osp|/ssd1/media-source/b.txt|] "old"
          & withFailOnOpen [osp|/ssd1/media-source/b.txt|]
      )
  evs <- runResume ref Replace
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/b.txt|] fs.files) @?= Just "hi"
  Map.member [osp|/ssd1/media-source/b.txt.mc3k-part|] fs.files @?= False
  actions <- actionsOfLatestManifestIn [osp|/ssd1/media-source/ascmhl/|] ref
  actions @?= Just [FailedAction]

renameFailureOnANewFileLeavesNoName :: Assertion
renameFailureOnANewFileLeavesNoName = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "data"
          & withFailOnRename [osp|/ssd1/media-source/a.mxf|]
      )
  evs <- runOffload ref [[osp|/ssd1|]]
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  Map.member [osp|/ssd1/media-source/a.mxf|] fs.files @?= False
  Map.member [osp|/ssd1/media-source/a.mxf.mc3k-part|] fs.files @?= False

manifestWriteFailureFailsTheJobAfterTheCopies :: Assertion
manifestWriteFailureFailsTheJobAfterTheCopies = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "data"
          & withFailOnManifestWrite
      )
  evs <- runOffload ref [[osp|/ssd1|]]
  assertBool "expected JobFailed" (V.any isJobFailed evs)
  statusesOf (RelPath "a.mxf") evs @?= [Copying, Flushing, Publishing, Verifying, Done Ok]
  fs <- readIORef ref
  fmap fst (Map.lookup [osp|/ssd1/media-source/a.mxf|] fs.files) @?= Just "data"
  assertBool "no ascmhl under the destination" (not (any (\k -> [osp|/ssd1/media-source/ascmhl/|] `isPrefixOf` k) (Map.keys fs.files)))

isReplaced :: FileStatus -> Bool
isReplaced (Done (Replaced _)) = True
isReplaced _ = False

recordsVerifiedWhenTheSourceWasSealed :: Assertion
recordsVerifiedWhenTheSourceWasSealed = do
  ref <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runSeal' ref
  _ <- runOffload' ref
  actions <- actionsOfLatestManifestIn [osp|/dest/vol/ascmhl/|] ref
  actions @?= Just [Verified]

recordsFailedWhenTheSourceChangedAfterSealing :: Assertion
recordsFailedWhenTheSourceChangedAfterSealing = do
  ref <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runSeal' ref
  modifyIORef' ref (withFile [osp|/vol/A002C001.MXF|] (BC.pack "HELLO"))
  evs <- runOffload' ref
  V.last evs @?= JobFinished (WithFailures 1)
  actions <- actionsOfLatestManifestIn [osp|/dest/vol/ascmhl/|] ref
  actions @?= Just [FailedAction]
  -- The value is the seal's own hash, never the changed file's 5cc5640faafe2c0e.
  values <- latestManifestHashesIn [osp|/dest/vol/ascmhl/|] ref <&> fmap (\hs -> map (\mh -> mh.hash.value) hs)
  values @?= Just ["26c7827d889f6da3"]

hashesInTheAlgorithmOfTheOriginals :: Assertion
hashesInTheAlgorithmOfTheOriginals = do
  ref <- newIORef (emptyMemFS & withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] md5SealFixture)
  modifyIORef' ref (withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runOffload' ref
  algos <- latestManifestHashesIn [osp|/dest/vol/ascmhl/|] ref <&> fmap (\hs -> map (\mh -> mh.hash.algo) hs)
  algos @?= Just [MD5]
  -- Only a comparison that matched gives 'Verified', so both legs must hash in md5.
  actions <- actionsOfLatestManifestIn [osp|/dest/vol/ascmhl/|] ref
  actions @?= Just [Verified]

sealsTheMediaSourceFirstAndRecordsVerified :: Assertion
sealsTheMediaSourceFirstAndRecordsVerified = do
  ref <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  evs <- runSealFirstOffload ref StopBeforeCopy
  V.last evs @?= JobFinished AllOk
  fs <- readIORef ref
  let mediaSourceHistory = Map.keys (Map.filterWithKey (\k _ -> [osp|/vol/ascmhl/|] `isPrefixOf` k) fs.files)
  assertBool "the media source holds one generation and its chain" (length mediaSourceHistory == 2)
  -- The job checked the copy against the generation that it wrote itself.
  actions <- actionsOfLatestManifestIn [osp|/dest/vol/ascmhl/|] ref
  actions @?= Just [Verified]

stopsBeforeCopyingWhenTheSealFails :: Assertion
stopsBeforeCopyingWhenTheSealFails = do
  ref <- newIORef (emptyMemFS & withTextFile [osp|/vol/ascmhl/0001_vol_2026-05-09_170200.mhl|] wrongSealFixture)
  modifyIORef' ref (withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  evs <- runSealFirstOffload ref StopBeforeCopy
  -- The sentence the manual quotes, pinned here in full.
  V.last evs @?= JobFailed "the seal failed for 1 of 1 files; nothing was copied"
  fs <- readIORef ref
  let copied = Map.keys (Map.filterWithKey (\k _ -> [osp|/dest/|] `isPrefixOf` k) fs.files)
      mediaSourceHistory = Map.keys (Map.filterWithKey (\k _ -> [osp|/vol/ascmhl/|] `isPrefixOf` k) fs.files)
  copied @?= []
  -- The media source keeps the seal that found the fault. The job stopped, but it did not discard the record.
  assertBool "the media source holds both generations and its chain" (length mediaSourceHistory == 3)

runSealFirstOffload :: IORef MemFS -> OnSealFailure -> IO (Vector JobEvent)
runSealFirstOffload ref policy =
  runJobEvents ref $
    JobSpec
      { jobId = JobId 1
      , job =
          Offload
            OffloadJob
              { source = [osp|/vol|]
              , destinations = NE.fromList [[osp|/dest|]]
              , sealFirst = SealBeforeCopy policy
              , existingCopy = Nothing
              }
      , createdAt = epoch
      }

-- | A media source that another tool sealed. The recorded hash does not match the file that the test writes.
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

runOffload' :: IORef MemFS -> IO (Vector JobEvent)
runOffload' ref =
  runJobEvents ref $
    JobSpec
      { jobId = JobId 1
      , job = Offload OffloadJob {source = [osp|/vol|], destinations = NE.fromList [[osp|/dest|]], sealFirst = UseHistory, existingCopy = Nothing}
      , createdAt = epoch
      }

-- | A media source that another tool sealed in md5. The mtime is MemFS's epoch, so the manifest describes the file that the test writes.
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

verifiesReportsMissingAndNewWritesAVerifyGeneration :: Assertion
verifiesReportsMissingAndNewWritesAVerifyGeneration = do
  manifest <- TIO.readFile "tests/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"
  chain <- TIO.readFile "tests/fixtures/ascmhl/ascmhl_chain.xml"
  let fs0 =
        withTextFile [osp|/vol/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl|] manifest $
          withTextFile [osp|/vol/ascmhl/ascmhl_chain.xml|] chain $
            withFile [osp|/vol/A002C001.MXF|] (BC.pack "Nobody inspects the spammish repetition") $
              withFile [osp|/vol/extra.txt|] "new" emptyMemFS
  ref <- newIORef fs0
  evs <- runVerify' ref
  statusesOf (RelPath "A002C001.MXF") evs @?= [Verifying, Done Ok]
  statusesOf (RelPath "Sidecar/notes.txt") evs @?= [Done Missing]
  statusesOf (RelPath "extra.txt") evs @?= [Hashing, Done New]
  V.last evs @?= JobFinished (WithFailures 1)
  fs <- readIORef ref
  length (filter (\k -> [osp|.mhl|] `isSuffixOf` k) (Map.keys fs.files)) @?= 2

reportsAMismatch :: Assertion
reportsAMismatch = do
  manifest <- TIO.readFile "tests/fixtures/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl"
  chain <- TIO.readFile "tests/fixtures/ascmhl/ascmhl_chain.xml"
  let fs0 =
        withTextFile [osp|/vol/ascmhl/0001_A002R2EC_2026-05-09_170200.mhl|] manifest $
          withTextFile [osp|/vol/ascmhl/ascmhl_chain.xml|] chain $
            withFile [osp|/vol/A002C001.MXF|] "wrong" $
              withFile [osp|/vol/Sidecar/notes.txt|] "" emptyMemFS
  ref <- newIORef fs0
  evs <- runVerify' ref
  assertBool
    "expected exactly one HashMismatch for \"A002C001.MXF\""
    (length (filter isHashMismatch (statusesOf (RelPath "A002C001.MXF") evs)) == 1)
  statusesOf (RelPath "Sidecar/notes.txt") evs @?= [Verifying, Done Ok]

runVerify' :: IORef MemFS -> IO (Vector JobEvent)
runVerify' ref =
  runJobEvents ref $
    JobSpec
      { jobId = JobId 1
      , job = VerifyFolder VerifyJob {folder = [osp|/vol|]}
      , createdAt = epoch
      }

sealsAFreshFolderWithOriginalActions :: Assertion
sealsAFreshFolderWithOriginalActions = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello")
          & withFile [osp|/vol/Sidecar/notes.txt|] (BC.pack "")
      )
  evs <- runSeal' ref
  actions <- actionsOfLatestManifestIn [osp|/vol/ascmhl/|] ref
  actions @?= Just [Original, Original]
  assertBool "no manifest was written" (any isMhlWritten (V.toList evs))

sealsAgainRecordingVerifiedAndFailed :: Assertion
sealsAgainRecordingVerifiedAndFailed = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello")
          & withFile [osp|/vol/Sidecar/notes.txt|] (BC.pack "")
      )
  _ <- runSeal' ref
  modifyIORef' ref (withFile [osp|/vol/A002C001.MXF|] (BC.pack "HELLO"))
  _ <- runSeal' ref
  actions <- actionsOfLatestManifestIn [osp|/vol/ascmhl/|] ref
  actions @?= Just [FailedAction, Verified]

-- | After a seal and an offload, the destination holds the card's generation byte for byte, its own
-- second generation, and a chain that names both.
carriesTheMediaSourceHistoryIntoTheDestination :: Assertion
carriesTheMediaSourceHistoryIntoTheDestination = do
  ref <- newIORef (emptyMemFS & withFile [osp|/vol/A002C001.MXF|] (BC.pack "hello"))
  _ <- runSeal' ref
  _ <- runOffload' ref
  fs <- readIORef ref
  let manifestsUnder prefix = fs.files & Map.keys & filter (\k -> prefix `isPrefixOf` k && [osp|.mhl|] `isSuffixOf` k) & sort
      sourceManifests = manifestsUnder [osp|/vol/ascmhl/|]
  length sourceManifests @?= 1
  length (manifestsUnder [osp|/dest/vol/ascmhl/|]) @?= 2
  mapM_
    ( \k ->
        fmap fst (Map.lookup ([osp|/dest/vol/ascmhl|] </> makeRelative [osp|/vol/ascmhl|] k) fs.files) @?= fmap fst (Map.lookup k fs.files)
    )
    sourceManifests
  case Map.lookup [osp|/dest/vol/ascmhl/ascmhl_chain.xml|] fs.files of
    Nothing -> assertFailure "the destination has no chain"
    Just (bytes, _) -> case parseChain (TE.decodeUtf8 bytes) of
      Left err -> assertFailure (show err)
      Right chain -> V.toList (V.map (\entry -> entry.sequenceNr) chain.entries) @?= [1, 2]

runSeal' :: IORef MemFS -> IO (Vector JobEvent)
runSeal' ref =
  runJobEvents ref $
    JobSpec
      { jobId = JobId 1
      , job = SealMediaSource SealJob {folder = [osp|/vol|]}
      , createdAt = epoch
      }

isMhlWritten :: JobEvent -> Bool
isMhlWritten (MhlWritten _) = True
isMhlWritten _ = False

-- | The first hash of each file entry of the newest manifest under the given `ascmhl` directory, in path order.
latestManifestHashesIn :: OsPath -> IORef MemFS -> IO (Maybe (List ManifestHash))
latestManifestHashesIn prefix ref = do
  fs <- readIORef ref
  let manifests = fs.files & Map.keys & filter (\p -> isManifestPath prefix p) & sort
  case reverse manifests of
    [] -> pure Nothing
    newest : _ -> case Map.lookup newest fs.files of
      Nothing -> pure Nothing
      Just (bytes, _mtime) -> case parseManifest (TE.decodeUtf8 bytes) of
        Left _ -> pure Nothing
        Right m ->
          m.entries
            & fileEntries
            & V.toList
            & sortOn (\e -> e.path)
            & mapMaybe (\e -> e.hashes V.!? 0)
            & Just
            & pure

-- | The actions of the newest manifest under the given `ascmhl` directory, in path order.
actionsOfLatestManifestIn :: OsPath -> IORef MemFS -> IO (Maybe (List HashAction))
actionsOfLatestManifestIn prefix ref =
  latestManifestHashesIn prefix ref <&> \hashes -> fmap (\hs -> map (\mh -> mh.action) hs) hashes

isManifestPath :: OsPath -> OsPath -> Bool
isManifestPath prefix p = prefix `isPrefixOf` p && [osp|.mhl|] `isSuffixOf` p

blockedPlanWritesNothing :: Assertion
blockedPlanWritesNothing = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/a.mxf|] "hi"
          & withFile [osp|/ssd1/media-source/already.txt|] "there"
      )
  before <- readIORef ref
  plan <- runEff (runFileSystemMem ref (planJob (offloadOneDest [osp|/media-source|] [osp|/ssd1|])))
  planBlocked plan @?= True
  (_, evs) <- runEff (runFileSystemMem ref (runHasherIO (runTime (runEmitCollect (executePlan defaultToolInfo plan)))))
  assertBool "expected JobFailed" (V.any isJobFailed evs)
  afterwards <- readIORef ref
  Map.keys afterwards.files @?= Map.keys before.files

offloadStepsMatchWhatItCopies :: Assertion
offloadStepsMatchWhatItCopies = do
  ref <-
    newIORef
      ( emptyMemFS
          & withFile [osp|/media-source/A/1.mxf|] (BS.replicate 900 1)
          & withFile [osp|/media-source/b.txt|] "hi"
      )
  let spec = offloadOneDest [osp|/media-source|] [osp|/ssd1|]
  plan <- runEff (runFileSystemMem ref (planJob spec))
  _ <- runJobEvents ref spec
  fs <- readIORef ref
  let planned = plan.steps & V.toList & map (\step -> display step.path) & sort
      copied =
        fs.files
          & Map.keys
          & filter (\k -> [osp|/ssd1/media-source/|] `isPrefixOf` k)
          & filter (\k -> not ([osp|/ssd1/media-source/ascmhl/|] `isPrefixOf` k))
          & map (\k -> pathText (makeRelative [osp|/ssd1/media-source|] k))
          & sort
  planned @?= copied

offloadOneDest :: OsPath -> OsPath -> JobSpec
offloadOneDest source dest =
  JobSpec
    { jobId = JobId 1
    , job = Offload OffloadJob {source, destinations = NE.fromList [dest], sealFirst = UseHistory, existingCopy = Nothing}
    , createdAt = epoch
    }

isJobFailed :: JobEvent -> Bool
isJobFailed (JobFailed _) = True
isJobFailed _ = False

runJobEvents :: IORef MemFS -> JobSpec -> IO (Vector JobEvent)
runJobEvents ref spec = do
  (_, evs) <- runEff (runFileSystemMem ref (runHasherIO (runTime (runEmitCollect (runJob defaultToolInfo spec)))))
  pure evs

-- | The manifest events alone, in emission order, as words. The path an 'MhlWritten' carries is not
-- what these tests ask about, so it does not reach the expected value.
manifestPhases :: Vector JobEvent -> List String
manifestPhases events = mapMaybe phaseOf (V.toList events)
  where
    phaseOf = \case
      ManifestWriting -> Just "start"
      MhlWritten _ -> Just "written"
      _ -> Nothing

-- | The statuses reported for one file, in emission order.
statusesOf :: RelPath -> Vector JobEvent -> List FileStatus
statusesOf target events = mapMaybe matchStatus (V.toList events)
  where
    matchStatus (FileStatusChanged rel status) | rel == target = Just status
    matchStatus _ = Nothing

isHashMismatch :: FileStatus -> Bool
isHashMismatch (Done (HashMismatch _)) = True
isHashMismatch _ = False

isIoError :: FileStatus -> Bool
isIoError (Done (IoError _)) = True
isIoError _ = False
