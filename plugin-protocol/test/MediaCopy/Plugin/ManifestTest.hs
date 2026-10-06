module MediaCopy.Plugin.ManifestTest (tests) where

import Data.Aeson (Value (Array))
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
    [ testCase "names each capability" namesEachCapability
    , testCase "accepts plan.inspect with block" acceptsPlanInspectWithBlock
    , testCase "refuses a manifest with no hook capability" refusesAManifestWithNoHookCapability
    , testCase "refuses block without plan.inspect" refusesBlockWithoutPlanInspect
    , testCase "refuses manifest.write with no namespace" refusesManifestWriteWithNoNamespace
    , testCase "refuses a reserved namespace" refusesAReservedNamespace
    , testCase "refuses another API major version" refusesAnotherApiMajorVersion
    , testCase "refuses an id that is not a reverse domain name" refusesAnIdThatIsNotAReverseDomainName
    , testCase "refuses authors as a job field" refusesAuthorsAsAJobField
    , testCase "refuses authors without manifest.write" refusesAuthorsWithoutManifestWrite
    , testCase "refuses two authors settings" refusesTwoAuthorsSettings
    , testCase "refuses a default on an authors setting" refusesADefaultOnAnAuthorsSetting
    , testCase "accepts one authors setting with manifest.write" acceptsOneAuthorsSettingWithManifestWrite
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

refused :: Text -> PluginManifest -> Assertion
refused reason m = either Just (const Nothing) (validateManifest m) @?= Just reason

namesEachCapability :: Assertion
namesEachCapability =
  map capabilityName [minBound ..] @?= ["files.read", "plan.inspect", "files.inspect", "block", "manifest.write"]

acceptsPlanInspectWithBlock :: Assertion
acceptsPlanInspectWithBlock = do
  let m = base {capabilities = V.fromList [FilesRead, PlanInspect, Block]}
  either Just (const Nothing) (validateManifest m) @?= Nothing

refusesAManifestWithNoHookCapability :: Assertion
refusesAManifestWithNoHookCapability =
  refused "declares no capability that MediaCopy 3000 calls" base {capabilities = V.singleton FilesRead}

refusesBlockWithoutPlanInspect :: Assertion
refusesBlockWithoutPlanInspect =
  refused
    "declares block, but not plan.inspect"
    base {capabilities = V.fromList [FilesInspect, Block]}

refusesManifestWriteWithNoNamespace :: Assertion
refusesManifestWriteWithNoNamespace =
  refused
    "declares manifest.write, but no namespace"
    base {capabilities = V.singleton ManifestWrite}

refusesAReservedNamespace :: Assertion
refusesAReservedNamespace = do
  let writer = base {capabilities = V.singleton ManifestWrite}
  refused "declares the namespace \"urn:ASC:MHL:v2.0\", which is reserved" writer {namespace = Just "urn:ASC:MHL:v2.0"}
  refused "declares the namespace \"\", which is reserved" writer {namespace = Just ""}

refusesAnotherApiMajorVersion :: Assertion
refusesAnotherApiMajorVersion =
  refused
    "needs plug-in API 2, and this MediaCopy 3000 speaks API 1"
    base {api = 2}

refusesAnIdThatIsNotAReverseDomainName :: Assertion
refusesAnIdThatIsNotAReverseDomainName =
  refused
    "has the id \"../escape\", which is not a reverse domain name"
    base {id = PluginId "../escape"}

authors :: Text -> Field
authors key = Field {key, label = "Authors", kind = AuthorsField, required = True, defaultValue = Nothing}

creditsLike :: PluginManifest
creditsLike = base {capabilities = V.singleton ManifestWrite, namespace = Just "urn:x"}

refusesAuthorsAsAJobField :: Assertion
refusesAuthorsAsAJobField =
  refused "declares the job field \"authors\" of kind authors, which only a setting can be" creditsLike {jobFields = V.singleton (authors "authors")}

refusesAuthorsWithoutManifestWrite :: Assertion
refusesAuthorsWithoutManifestWrite =
  refused
    "declares the setting \"authors\" of kind authors, which needs manifest.write"
    base {settings = V.singleton (authors "authors")}

refusesTwoAuthorsSettings :: Assertion
refusesTwoAuthorsSettings =
  refused "declares more than one setting of kind authors" creditsLike {settings = V.fromList [authors "crew", authors "guests"]}

refusesADefaultOnAnAuthorsSetting :: Assertion
refusesADefaultOnAnAuthorsSetting =
  refused
    "declares a default for the setting \"authors\" of kind authors"
    creditsLike {settings = V.singleton (authors "authors") {defaultValue = Just (Array mempty)}}

acceptsOneAuthorsSettingWithManifestWrite :: Assertion
acceptsOneAuthorsSettingWithManifestWrite =
  either Just (const Nothing) (validateManifest creditsLike {settings = V.singleton (authors "authors")}) @?= Nothing
