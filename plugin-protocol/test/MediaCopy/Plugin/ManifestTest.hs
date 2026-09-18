module MediaCopy.Plugin.ManifestTest (tests) where

import Data.Aeson (Value (Array, String), decode, object, toJSON, (.=))
import Data.Either (isLeft, isRight)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.Field qualified as Plugin (Field)
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Manifest qualified as Manifest
import MediaCopy.Plugin.Manifest qualified as Plugin (Manifest)

tests :: TestTree
tests =
  testGroup
    "Manifest"
    [ testCase "valid manifests are accepted" validManifestsAreAccepted
    , testCase "each broken rule refuses the manifest" manifestRules
    , testCase "an author slot needs a role, fills the other fields with empty text, and writes every field" authorSlotsRoundTrip
    ]

base :: Plugin.Manifest
base =
  Manifest.Manifest
    { id = Id.Id "tech.floreal.probe"
    , name = "Probe"
    , description = "Probes the plan."
    , version = "1.0.0"
    , api = 1
    , namespace = Nothing
    , executable = Map.empty
    , capabilities = V.singleton Capability.PlanInspect
    , settings = mempty
    , jobFields = mempty
    }

writer :: Plugin.Manifest
writer = base {Manifest.capabilities = V.singleton Capability.ManifestWrite, Manifest.namespace = Just "urn:x"}

authors :: Text -> Plugin.Field
authors key = Field.Field {key, label = "Authors", kind = Field.AuthorsField, required = True, defaultValue = Nothing}

validManifestsAreAccepted :: Assertion
validManifestsAreAccepted =
  [ label
  | (label, manifest) <- accepted
  , isLeft (Manifest.validate manifest)
  ]
    @?= []

accepted :: List (Text, Plugin.Manifest)
accepted =
  [ ("plan.inspect", base)
  , ("plan.inspect with block", base {Manifest.capabilities = V.fromList [Capability.FilesRead, Capability.PlanInspect, Capability.Block]})
  , ("manifest.write with a namespace", writer)
  , ("one authors setting with manifest.write", writer {Manifest.settings = V.singleton (authors "authors")})
  ]

manifestRules :: Assertion
manifestRules =
  [ label
  | (label, manifest) <- refused
  , isRight (Manifest.validate manifest)
  ]
    @?= []

refused :: List (Text, Plugin.Manifest)
refused =
  [ ("no hook capability", base {Manifest.capabilities = V.singleton Capability.FilesRead})
  , ("block without plan.inspect", base {Manifest.capabilities = V.fromList [Capability.FilesInspect, Capability.Block]})
  , ("manifest.write with no namespace", writer {Manifest.namespace = Nothing})
  , ("the ASC MHL namespace", writer {Manifest.namespace = Just "urn:ASC:MHL:v2.0"})
  , ("the empty namespace", writer {Manifest.namespace = Just ""})
  , ("another API major version", base {Manifest.api = 2})
  , ("an id that is not a reverse domain name", base {Manifest.id = Id.Id "../escape"})
  , ("authors as a job field", writer {Manifest.jobFields = V.singleton (authors "authors")})
  , ("authors without manifest.write", base {Manifest.settings = V.singleton (authors "authors")})
  , ("two authors settings", writer {Manifest.settings = V.fromList [authors "crew", authors "guests"]})
  , ("a default on an authors setting", writer {Manifest.settings = V.singleton (authors "authors") {Field.defaultValue = Just (Array mempty)}})
  ]

authorSlotsRoundTrip :: Assertion
authorSlotsRoundTrip = do
  decode "[{\"role\": \"DIT\", \"name\": \"Jane Doe\"}, {\"role\": \"Camera operator\"}]"
    @?= Just (V.fromList [AuthorSlot.AuthorSlot "DIT" "Jane Doe" "" "", AuthorSlot.AuthorSlot "Camera operator" "" "" ""])
  decode "[{\"name\": \"Jane\"}]" @?= (Nothing :: Maybe (V.Vector Plugin.AuthorSlot))
  decode "[{\"role\": \"DIT\", \"email\": null}]" @?= (Nothing :: Maybe (V.Vector Plugin.AuthorSlot))
  toJSON (AuthorSlot.AuthorSlot "DIT" "Jane Doe" "" "")
    @?= object ["email" .= String "", "name" .= String "Jane Doe", "phone" .= String "", "role" .= String "DIT"]
