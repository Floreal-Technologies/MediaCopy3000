module MediaCopy.Conformance.InteropTest (tests) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Conformance.Ascmhl
import MediaCopy.Conformance.Harness

tests :: Tools -> TestTree
tests tools =
  testGroup
    "interop"
    [ testCase "our generation follows theirs in the same history" (ourGenerationFollowsTheirsInTheSameHistory tools)
    ]

ourGenerationFollowsTheirsInTheSameHistory :: Tools -> Assertion
ourGenerationFollowsTheirsInTheSameHistory tools =
  withTempTree "mc3k-conformance" $ \work -> do
    let mediaSource = work </> "media-source"
    writeTreeFile (mediaSource </> "A002C001.MXF") "hello"
    writeTreeFile (mediaSource </> "Sidecar" </> "notes.txt") ""
    createHistory tools "xxh64" mediaSource >>= assertOk "ascmhl create -h xxh64"
    before <- TIO.readFile (chainFileIn mediaSource)
    let referenceC4 = firstC4Of before
    assertBool ("the reference chain carries no <c4>:\n" <> T.unpack before) (T.isPrefixOf "c4" referenceC4)
    verifySpec mediaSource >>= \spec -> void (runEngineIO spec)
    manifests <- mhlFilesIn mediaSource
    length manifests @?= 2
    rewritten <- TIO.readFile (chainFileIn mediaSource)
    assertBool "the reference's c4 chain entry was lost" (T.isInfixOf referenceC4 rewritten)
    mapM_ (\file -> assertSchemaValidManifest tools file) manifests
    schemaCheckChain tools (chainFileIn mediaSource) >>= assertOk "xsd-schema-check -df"
    verifyFolder tools mediaSource >>= assertOk "ascmhl-debug verify"
    info <- infoFolder tools mediaSource
    assertOk "ascmhl info" info
    assertBool "info does not show two generations" (T.isInfixOf "Generation 2" info.out)

firstC4Of :: Text -> Text
firstC4Of chainText = case T.splitOn "<c4>" chainText of
  (_ : rest : _) -> T.takeWhile (\c -> c /= '<') rest
  _ -> ""
