module MediaCopy.Conformance.OursToReferenceTest (tests) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl (Tools)
import MediaCopy.Conformance.Ascmhl qualified as Ascmhl
import MediaCopy.Conformance.Harness qualified as Harness
import MediaCopy.Domain.Offload qualified as Offload

tests :: Tools -> TestTree
tests tools =
  testGroup
    "ours to reference"
    [ testCase "offloads conform" (offloadsConform tools)
    , testCase "the reference catches corruption" (theReferenceCatchesCorruption tools)
    , testCase "seal-first conforms" (sealFirstConforms tools)
    ]

withOffloadTree :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withOffloadTree use =
  Harness.withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    Harness.writeTreeFile (source </> "A" </> "B" </> "f2.txt") "yy"
    Harness.writeTreeFile (source </> "A" </> "f1.txt") "x"
    Harness.writeTreeFile (source </> "top.txt") "zzz"
    createDirectoryIfMissing True (source </> "Empty")
    createDirectoryIfMissing True parent
    use source parent (parent </> "media-source")

offloadsConform :: Tools -> Assertion
offloadsConform tools =
  withOffloadTree $ \source parent dest -> do
    Harness.offloadSpec source parent >>= \spec -> void (Harness.runEngineIO spec)
    Ascmhl.assertConforms tools dest
    Harness.verifySpec dest >>= \spec -> void (Harness.runEngineIO spec)
    manifests <- Harness.mhlFilesIn dest
    length manifests @?= 2
    Ascmhl.assertConforms tools dest

theReferenceCatchesCorruption :: Tools -> Assertion
theReferenceCatchesCorruption tools =
  withOffloadTree $ \source parent dest -> do
    Harness.offloadSpec source parent >>= \spec -> void (Harness.runEngineIO spec)
    Harness.writeTreeFile (dest </> "top.txt") "ZZZ"
    Ascmhl.verifyFolder tools dest >>= Ascmhl.assertFails "ascmhl-debug verify on a corrupted file"

sealFirstConforms :: Tools -> Assertion
sealFirstConforms tools =
  withOffloadTree $ \source parent dest -> do
    Harness.offloadSpecWith (Offload.SealBeforeCopy Offload.StopBeforeCopy) source parent >>= \spec -> void (Harness.runEngineIO spec)
    Ascmhl.assertConforms tools source
    Ascmhl.assertConforms tools dest
