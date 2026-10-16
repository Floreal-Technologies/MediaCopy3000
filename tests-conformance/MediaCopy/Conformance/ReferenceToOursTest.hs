module MediaCopy.Conformance.ReferenceToOursTest (tests) where

import Ascmhl.Hash (Hash, HashAlgo (..))
import Ascmhl.Path (RelPath (..))
import Ascmhl.Read (parseManifest)
import Ascmhl.Types (DirectoryEntry (..), Manifest (..), ManifestEntry (..), latestHashes)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (runEff)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl
import MediaCopy.Conformance.Harness
import MediaCopy.Domain.Job (JobEvent (..), JobResult (..))
import MediaCopy.Effects.Hasher (hashBytes, runHasher)

tests :: Tools -> TestTree
tests tools =
  testGroup
    "reference to ours"
    [ testCase "we read and extend reference histories" (forM_ [XXH64, MD5] (weReadAndExtendAReferenceHistory tools))
    ]

mediaFiles :: List (Text, ByteString)
mediaFiles = [("A002C001.MXF", "hello"), ("Sidecar/notes.txt", "")]

weReadAndExtendAReferenceHistory :: Tools -> HashAlgo -> Assertion
weReadAndExtendAReferenceHistory tools algo =
  withTempTree "mc3k-conformance" $ \work -> do
    let mediaSource = work </> "media-source"
    let format = display algo
    forM_ mediaFiles (\file -> writeTreeFile (mediaSource </> T.unpack (fst file)) (snd file))
    createDirectoryIfMissing True (mediaSource </> "Empty")
    createHistory tools format mediaSource >>= assertOk ("ascmhl create -h " <> T.unpack format)
    manifests <- readManifestsOf mediaSource
    forM_ mediaFiles $ \file -> do
      ours <- ourHash algo (snd file)
      Map.lookup (RelPath (fst file)) (latestHashes manifests) @?= Just ours
    assertBool "no directory entry was parsed" (hasDirEntry (RelPath "Empty") manifests)
    readHistoryIO mediaSource >>= \case
      Right (Just _) -> pure ()
      Right Nothing -> assertFailure "readHistory found no history"
      Left err -> assertFailure ("readHistory failed: " <> T.unpack (display err))
    referenceC4 <- TIO.readFile (chainFileIn mediaSource) <&> firstC4Of
    assertBool "the reference chain carries no <c4>" (T.isPrefixOf "c4" referenceC4)
    events <- verifySpec mediaSource >>= runEngineIO
    assertBool ("verify reported a failure: " <> show (V.toList events)) (not (V.any isFailureEvent events))
    manifestsAfter <- mhlFilesIn mediaSource
    length manifestsAfter @?= 2
    rewritten <- TIO.readFile (chainFileIn mediaSource)
    assertBool "the reference's c4 chain entry was lost" (T.isInfixOf referenceC4 rewritten)
    assertConforms tools mediaSource

ourHash :: HashAlgo -> ByteString -> IO Hash
ourHash algo bytes =
  hashBytes algo bytes
    & runHasher
    & runEff

readManifestsOf :: FilePath -> IO (Vector Manifest)
readManifestsOf folder = do
  files <- mhlFilesIn folder
  parsed <- traverse parseManifestFile files
  pure (V.fromList parsed)

parseManifestFile :: FilePath -> IO Manifest
parseManifestFile file = do
  text <- TIO.readFile file
  case parseManifest text of
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
  ManifestDir directory -> directory.path == wanted
  ManifestFile _ -> False

isFailureEvent :: JobEvent -> Bool
isFailureEvent event = case event of
  (JobFinished (WithFailures _); JobFailed _) -> True
  _ -> False
