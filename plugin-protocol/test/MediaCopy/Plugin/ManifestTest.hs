module MediaCopy.Plugin.ManifestTest (tests) where

import Data.Aeson (Value (Array))
import Data.Either (isLeft, isRight)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.Manifest

tests :: TestTree
tests =
  testGroup
    "Manifest"
    [ testCase "valid manifests are accepted" validManifestsAreAccepted
    , testCase "each broken rule refuses the manifest" manifestRules
    ]

base :: PluginManifest
base =
  PluginManifest
    { id = PluginId "tech.floreal.probe"
    , name = "Probe"
    , description = "Probes the plan."
    , version = "1.0.0"
    , api = 1
    , namespace = Nothing
    , executable = Map.empty
    , capabilities = V.singleton PlanInspect
    , settings = mempty
    , jobFields = mempty
    }

writer :: PluginManifest
writer = base {capabilities = V.singleton ManifestWrite, namespace = Just "urn:x"}

authors :: Text -> Field
authors key = Field {key, label = "Authors", kind = AuthorsField, required = True, defaultValue = Nothing}

validManifestsAreAccepted :: Assertion
validManifestsAreAccepted =
  [ label
  | (label, manifest) <- accepted
  , isLeft (validateManifest manifest)
  ]
    @?= []

accepted :: List (Text, PluginManifest)
accepted =
  [ ("plan.inspect", base)
  , ("plan.inspect with block", base {capabilities = V.fromList [FilesRead, PlanInspect, Block]})
  , ("manifest.write with a namespace", writer)
  , ("one authors setting with manifest.write", writer {settings = V.singleton (authors "authors")})
  ]

manifestRules :: Assertion
manifestRules =
  [ label
  | (label, manifest) <- refused
  , isRight (validateManifest manifest)
  ]
    @?= []

refused :: List (Text, PluginManifest)
refused =
  [ ("no hook capability", base {capabilities = V.singleton FilesRead})
  , ("block without plan.inspect", base {capabilities = V.fromList [FilesInspect, Block]})
  , ("manifest.write with no namespace", writer {namespace = Nothing})
  , ("the ASC MHL namespace", writer {namespace = Just "urn:ASC:MHL:v2.0"})
  , ("the empty namespace", writer {namespace = Just ""})
  , ("another API major version", base {api = 2})
  , ("an id that is not a reverse domain name", base {id = PluginId "../escape"})
  , ("authors as a job field", writer {jobFields = V.singleton (authors "authors")})
  , ("authors without manifest.write", base {settings = V.singleton (authors "authors")})
  , ("two authors settings", writer {settings = V.fromList [authors "crew", authors "guests"]})
  , ("a default on an authors setting", writer {settings = V.singleton (authors "authors") {defaultValue = Just (Array mempty)}})
  ]
