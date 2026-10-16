module MediaCopy.Conformance.OursToReferenceTest (tests) where

import Control.Monad (void)
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
    [ testCase "our offloads conform" (ourOffloadsConform tools)
    , testCase "the reference catches corruption" (theReferenceCatchesCorruption tools)
    , testCase "seal-first conforms" (sealFirstConforms tools)
    ]

withOffloadTree :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withOffloadTree use =
  withTempTree "mc3k-conformance" $ \work -> do
    let source = work </> "media-source"
    let parent = work </> "dest"
    writeTreeFile (source </> "A" </> "B" </> "f2.txt") "yy"
    writeTreeFile (source </> "A" </> "f1.txt") "x"
    writeTreeFile (source </> "top.txt") "zzz"
    createDirectoryIfMissing True (source </> "Empty")
    createDirectoryIfMissing True parent
    use source parent (parent </> "media-source")

ourOffloadsConform :: Tools -> Assertion
ourOffloadsConform tools =
  withOffloadTree $ \source parent dest -> do
    offloadSpec source parent >>= \spec -> void (runEngineIO spec)
    assertConforms tools dest
    verifySpec dest >>= \spec -> void (runEngineIO spec)
    manifests <- mhlFilesIn dest
    length manifests @?= 2
    assertConforms tools dest

theReferenceCatchesCorruption :: Tools -> Assertion
theReferenceCatchesCorruption tools =
  withOffloadTree $ \source parent dest -> do
    offloadSpec source parent >>= \spec -> void (runEngineIO spec)
    writeTreeFile (dest </> "top.txt") "ZZZ"
    verifyFolder tools dest >>= assertFails "ascmhl-debug verify on a corrupted file"

sealFirstConforms :: Tools -> Assertion
sealFirstConforms tools =
  withOffloadTree $ \source parent dest -> do
    offloadSpecWith (SealBeforeCopy StopBeforeCopy) source parent >>= \spec -> void (runEngineIO spec)
    assertConforms tools source
    assertConforms tools dest
