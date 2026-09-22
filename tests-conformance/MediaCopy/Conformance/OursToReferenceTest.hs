module MediaCopy.Conformance.OursToReferenceTest (tests) where

import Control.Monad (void)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl
import MediaCopy.Conformance.Harness
import MediaCopy.Domain.Job (OnSealFailure (..), SealFirst (..))

tests :: Tools -> TestTree
tests tools =
  testGroup
    "ours to reference"
    [ testCase "a fresh offload is schema-valid and verifies" (aFreshOffloadIsSchemaValidAndVerifies tools)
    , testCase "a second generation still verifies" (aSecondGenerationStillVerifies tools)
    , testCase "a corrupted file fails the reference verify" (aCorruptedFileFailsTheReferenceVerify tools)
    , testCase "a seal-first offload leaves a media source the reference verifies" (aSealFirstOffloadSealsTheMediaSource tools)
    ]

buildSampleTree :: FilePath -> IO ()
buildSampleTree root = do
  writeTreeFile (root </> "A" </> "B" </> "f2.txt") "yy"
  writeTreeFile (root </> "A" </> "f1.txt") "x"
  writeTreeFile (root </> "top.txt") "zzz"
  createDirectoryIfMissing True (root </> "Empty")

aFreshOffloadIsSchemaValidAndVerifies :: Tools -> Assertion
aFreshOffloadIsSchemaValidAndVerifies tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    let dest = parent </> "media-source"
    buildSampleTree source
    createDirectoryIfMissing True parent
    spec <- offloadSpec source parent
    _events <- runEngineIO spec
    manifests <- mhlFilesIn dest
    assertBool "no manifest was written" (not (null manifests))
    mapM_ (\file -> assertSchemaValidManifest tools file) manifests
    schemaCheckChain tools (chainFileIn dest) >>= assertOk "xsd-schema-check -df"
    verifyFolder tools dest >>= assertOk "ascmhl-debug verify"
    verifyDirectoryHashes tools dest >>= assertOk "ascmhl-debug verify -dh"
    info <- infoFolder tools dest
    assertOk "ascmhl info" info
    assertBool "info does not mention the tool" (T.isInfixOf "mediacopy3000" info.out)

aSecondGenerationStillVerifies :: Tools -> Assertion
aSecondGenerationStillVerifies tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    let dest = parent </> "media-source"
    buildSampleTree source
    createDirectoryIfMissing True parent
    offloadSpec source parent >>= \spec -> void (runEngineIO spec)
    verifySpec dest >>= \spec -> void (runEngineIO spec)
    manifests <- mhlFilesIn dest
    length manifests @?= 2
    mapM_ (\file -> assertSchemaValidManifest tools file) manifests
    schemaCheckChain tools (chainFileIn dest) >>= assertOk "xsd-schema-check -df"
    verifyFolder tools dest >>= assertOk "ascmhl-debug verify"
    verifyDirectoryHashes tools dest >>= assertOk "ascmhl-debug verify -dh"

aCorruptedFileFailsTheReferenceVerify :: Tools -> Assertion
aCorruptedFileFailsTheReferenceVerify tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    let dest = parent </> "media-source"
    buildSampleTree source
    createDirectoryIfMissing True parent
    offloadSpec source parent >>= \spec -> void (runEngineIO spec)
    writeTreeFile (dest </> "top.txt") "ZZZ"
    verifyFolder tools dest >>= assertFails "ascmhl-debug verify on a corrupted file"

aSealFirstOffloadSealsTheMediaSource :: Tools -> Assertion
aSealFirstOffloadSealsTheMediaSource tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    let dest = parent </> "media-source"
    buildSampleTree source
    createDirectoryIfMissing True parent
    spec <- offloadSpecWith (SealBeforeCopy StopBeforeCopy) source parent
    _events <- runEngineIO spec
    sealed <- mhlFilesIn source
    assertBool "the seal wrote no manifest on the media source" (not (null sealed))
    mapM_ (\file -> assertSchemaValidManifest tools file) sealed
    schemaCheckChain tools (chainFileIn source) >>= assertOk "xsd-schema-check -df on the media source"
    verifyFolder tools source >>= assertOk "ascmhl-debug verify on the media source"
    verifyDirectoryHashes tools source >>= assertOk "ascmhl-debug verify -dh on the media source"
    verifyFolder tools dest >>= assertOk "ascmhl-debug verify on the destination"
