module MediaCopy.Conformance.ReferenceToOursTest (tests) where

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo
import Ascmhl.DirectoryEntry qualified as DirectoryEntry
import Ascmhl.Hash (Hash)
import Ascmhl.Manifest (Manifest)
import Ascmhl.Manifest qualified as Manifest
import Ascmhl.ManifestEntry (ManifestEntry)
import Ascmhl.ManifestEntry qualified as ManifestEntry
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effectful (runEff)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl (Tools)
import MediaCopy.Conformance.Ascmhl qualified as Ascmhl
import MediaCopy.Conformance.Harness qualified as Harness
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Effects.Hasher qualified as Hasher

tests :: Tools -> TestTree
tests tools =
  testGroup
    "reference to ours"
    [ testCase "reads and extends reference histories" (forM_ [Algo.XXH64, Algo.MD5] (readsAndExtendsAReferenceHistory tools))
    ]

mediaFiles :: List (Text, ByteString)
mediaFiles = [("A002C001.MXF", "hello"), ("Sidecar/notes.txt", "")]

readsAndExtendsAReferenceHistory :: Tools -> Algo -> Assertion
readsAndExtendsAReferenceHistory tools algo =
  Harness.withTempTree "mc3k-conformance" $ \work -> do
    let mediaSource = work </> "media-source"
    let format = display algo
    forM_ mediaFiles (\file -> Harness.writeTreeFile (mediaSource </> T.unpack (fst file)) (snd file))
    createDirectoryIfMissing True (mediaSource </> "Empty")
    Ascmhl.createHistory tools format mediaSource >>= Ascmhl.assertOk ("ascmhl create -h " <> T.unpack format)
    manifests <- readManifestsOf mediaSource
    forM_ mediaFiles $ \file -> do
      ours <- ourHash algo (snd file)
      Map.lookup (RelPath.RelPath (fst file)) (Manifest.latestHashes manifests) @?= Just ours
    assertBool "no directory entry was parsed" (hasDirEntry (RelPath.RelPath "Empty") manifests)
    Harness.readHistoryIO mediaSource >>= \case
      Right (Just _) -> pure ()
      Right Nothing -> assertFailure "readHistory found no history"
      Left err -> assertFailure ("readHistory failed: " <> T.unpack (display err))
    referenceC4 <- TIO.readFile (Harness.chainFileIn mediaSource) <&> firstC4Of
    assertBool "the reference chain carries no <c4>" (T.isPrefixOf "c4" referenceC4)
    events <- Harness.verifySpec mediaSource >>= Harness.runEngineIO
    assertBool ("verify reported a failure: " <> show (V.toList events)) (not (V.any isFailureEvent events))
    manifestsAfter <- Harness.mhlFilesIn mediaSource
    length manifestsAfter @?= 2
    rewritten <- TIO.readFile (Harness.chainFileIn mediaSource)
    assertBool "the reference's c4 chain entry was lost" (T.isInfixOf referenceC4 rewritten)
    Ascmhl.assertConforms tools mediaSource

ourHash :: Algo -> ByteString -> IO Hash
ourHash algo bytes =
  Hasher.hashBytes algo bytes
    & Hasher.run
    & runEff

readManifestsOf :: FilePath -> IO (Vector Manifest)
readManifestsOf folder = do
  files <- Harness.mhlFilesIn folder
  parsed <- traverse parseManifestFile files
  pure (V.fromList parsed)

parseManifestFile :: FilePath -> IO Manifest
parseManifestFile file = do
  text <- TIO.readFile file
  case Manifest.parse text of
    Left err -> assertFailure (file <> ": " <> T.unpack err)
    Right manifest -> pure manifest

firstC4Of :: Text -> Text
firstC4Of chainText = case T.splitOn "<c4>" chainText of
  (_ : rest : _) -> T.takeWhile (\c -> c /= '<') rest
  _ -> ""

hasDirEntry :: RelPath -> Vector Manifest -> Bool
hasDirEntry wanted manifests =
  V.any (\manifest -> V.any (isDirEntryFor wanted) manifest.entries) manifests

isDirEntryFor :: RelPath -> ManifestEntry -> Bool
isDirEntryFor wanted entry = case entry of
  ManifestEntry.Directory directory -> directory.path == wanted
  ManifestEntry.File _ -> False

isFailureEvent :: JobEvent -> Bool
isFailureEvent event = case event of
  (JobEvent.Finished (JobResult.WithFailures _); JobEvent.Failed _) -> True
  _ -> False
