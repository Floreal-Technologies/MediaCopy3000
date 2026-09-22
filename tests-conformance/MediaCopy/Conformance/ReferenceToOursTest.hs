module MediaCopy.Conformance.ReferenceToOursTest (tests) where

import Ascmhl.Hash (Hash (..), HashAlgo (..))
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read (parseManifest)
import Ascmhl.Types (DirectoryEntry (..), Manifest (..), ManifestEntry (..), latestHashes)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl
import MediaCopy.Conformance.Harness
import MediaCopy.Domain.Job (JobEvent (..), JobResult (..))

tests :: Tools -> TestTree
tests tools =
  testGroup
    "reference to ours"
    [ testCase "reads an xxh64 history" (readsAnXxh64History tools)
    , testCase "reads an md5 history" (readsAnMd5History tools)
    , testCase "verifies a folder the reference created" (verifiesAFolderTheReferenceCreated tools)
    ]

readsAnXxh64History :: Tools -> Assertion
readsAnXxh64History tools = readsAHistoryIn tools "xxh64" (Hash XXH64 "26c7827d889f6da3")

readsAnMd5History :: Tools -> Assertion
readsAnMd5History tools = readsAHistoryIn tools "md5" (Hash MD5 "5d41402abc4b2a76b9719d911017c592")

readsAHistoryIn :: Tools -> Text -> Hash -> Assertion
readsAHistoryIn tools format expected =
  withTempTree "mc3k-conformance" $ \work -> do
    let mediaSource = work </> "media-source"
    writeTreeFile (mediaSource </> "A002C001.MXF") "hello"
    writeTreeFile (mediaSource </> "Sidecar" </> "notes.txt") ""
    createDirectoryIfMissing True (mediaSource </> "Empty")
    createHistory tools format mediaSource >>= assertOk ("ascmhl create -h " <> T.unpack format)
    manifests <- readManifestsOf mediaSource
    let hashes = latestHashes manifests
    Map.lookup (RelPath "A002C001.MXF") hashes @?= Just expected
    Map.lookup (RelPath "Sidecar/notes.txt") hashes @?= Just (zeroByteHashFor format)
    assertBool "no directory entry was parsed" (hasDirEntry (RelPath "Empty") manifests)
    history <- readHistoryIO mediaSource
    assertRightHistory ("readHistory over an ascmhl create -h " <> T.unpack format <> " folder") history

verifiesAFolderTheReferenceCreated :: Tools -> Assertion
verifiesAFolderTheReferenceCreated tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let mediaSource = work </> "media-source"
    writeTreeFile (mediaSource </> "A002C001.MXF") "hello"
    writeTreeFile (mediaSource </> "Sidecar" </> "notes.txt") ""
    createHistory tools "xxh64" mediaSource >>= assertOk "ascmhl create -h xxh64"
    spec <- verifySpec mediaSource
    events <- runEngineIO spec
    assertBool
      ("verify reported a failure: " <> show (V.toList events))
      (not (any (\event -> isFailureEvent event) (V.toList events)))

readManifestsOf :: FilePath -> IO (Vector Manifest)
readManifestsOf folder = do
  files <- mhlFilesIn folder
  parsed <- traverse (\file -> parseManifestFile file) files
  pure (V.fromList parsed)

parseManifestFile :: FilePath -> IO Manifest
parseManifestFile file = do
  text <- TIO.readFile file
  case parseManifest text of
    Left err -> assertFailure (file <> ": " <> T.unpack err)
    Right manifest -> pure manifest

assertRightHistory :: (Display e) => String -> Either e (Maybe a) -> Assertion
assertRightHistory what result = case result of
  Left err -> assertFailure (what <> " failed: " <> T.unpack (display err))
  Right Nothing -> assertFailure (what <> " found no history at all")
  Right (Just _) -> pure ()

zeroByteHashFor :: Text -> Hash
zeroByteHashFor format = case format of
  "md5" -> Hash MD5 "d41d8cd98f00b204e9800998ecf8427e"
  _ -> Hash XXH64 "ef46db3751d8e999"

hasDirEntry :: RelPath -> Vector Manifest -> Bool
hasDirEntry wanted manifests =
  V.any (\manifest -> V.any (\entry -> isDirEntryFor wanted entry) manifest.entries) manifests

isDirEntryFor :: RelPath -> ManifestEntry -> Bool
isDirEntryFor wanted entry = case entry of
  ManifestDir directory -> directory.path == wanted
  ManifestFile _ -> False

isFailureEvent :: JobEvent -> Bool
isFailureEvent event = case event of
  (JobFinished (WithFailures _); JobFailed _) -> True
  _ -> False
