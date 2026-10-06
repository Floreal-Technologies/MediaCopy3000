module MediaCopy.Plugin.SchemaTest (tests) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.Schema

tests :: TestTree
tests =
  testGroup
    "Schema"
    [ testCase "schema/protocol-1.schema.json follows the types" followsTheTypes
    ]

goldenFile, actualFile :: FilePath
goldenFile = "schema/protocol-1.schema.json"
actualFile = "schema/actual-protocol-1.schema.json"

followsTheTypes :: Assertion
followsTheTypes = do
  stored <- LBS.readFile goldenFile
  LBS.writeFile actualFile renderSchema
  golden <- either (\problem -> assertFailure (goldenFile <> " is not JSON: " <> problem)) pure (eitherDecode stored)
  assertBool
    ( unlines
        [ "The schema of the Haskell types is different from " <> goldenFile <> "."
        , "The new schema is at " <> actualFile <> "."
        , ""
        , "From the root of the repository:"
        , "1. Look at the change:"
        , "   git diff --no-index " <> inRepository goldenFile <> " " <> inRepository actualFile
        , "2. If the change is correct, keep it:"
        , "   cp " <> inRepository actualFile <> " " <> inRepository goldenFile
        , "   or run `just schema`."
        ]
    )
    (golden == protocolSchema)
  where
    inRepository path = "plugin-protocol/" <> path
