module MediaCopy.Plugin.ManifestTest (tests) where

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
    [ testCase "names each role" namesEachRole
    , testCase "names each capability" namesEachCapability
    , testCase "accepts an inspector that blocks" acceptsAnInspectorThatBlocks
    , testCase "refuses a manifest with no role" refusesAManifestWithNoRole
    , testCase "refuses a capability the roles do not allow" refusesACapabilityTheRolesDoNotAllow
    , testCase "refuses a contributor with no namespace" refusesAContributorWithNoNamespace
    , testCase "refuses a reserved namespace" refusesAReservedNamespace
    , testCase "refuses another API major version" refusesAnotherApiMajorVersion
    , testCase "refuses an id that is not a reverse domain name" refusesAnIdThatIsNotAReverseDomainName
    ]

base :: PluginManifest
base =
  PluginManifest
    { id = PluginId "tech.floreal.probe"
    , name = "Probe"
    , version = "1.0.0"
    , api = 1
    , namespace = Nothing
    , executable = Map.empty
    , roles = mempty
    , capabilities = mempty
    , settings = mempty
    , jobFields = mempty
    }

refused :: Text -> PluginManifest -> Assertion
refused reason m = either Just (const Nothing) (validateManifest m) @?= Just reason

namesEachRole :: Assertion
namesEachRole =
  map roleName [minBound ..] @?= ["inspector", "contributor"]

namesEachCapability :: Assertion
namesEachCapability =
  map capabilityName [minBound ..] @?= ["files.read", "block", "manifest.write"]

acceptsAnInspectorThatBlocks :: Assertion
acceptsAnInspectorThatBlocks = do
  let m = base {roles = V.singleton Inspector, capabilities = V.fromList [FilesRead, Block]}
  either Just (const Nothing) (validateManifest m) @?= Nothing

refusesAManifestWithNoRole :: Assertion
refusesAManifestWithNoRole = refused "declares no role" base

refusesACapabilityTheRolesDoNotAllow :: Assertion
refusesACapabilityTheRolesDoNotAllow =
  refused
    "declares block, which only an inspector can have"
    base {roles = V.singleton Contributor, namespace = Just "urn:x", capabilities = V.fromList [Block, ManifestWrite]}

refusesAContributorWithNoNamespace :: Assertion
refusesAContributorWithNoNamespace =
  refused
    "is a contributor, but declares no namespace"
    base {roles = V.singleton Contributor, capabilities = V.singleton ManifestWrite}

refusesAReservedNamespace :: Assertion
refusesAReservedNamespace = do
  let contributor = base {roles = V.singleton Contributor, capabilities = V.singleton ManifestWrite}
  refused "declares the namespace \"urn:ASC:MHL:v2.0\", which is reserved" contributor {namespace = Just "urn:ASC:MHL:v2.0"}
  refused "declares the namespace \"\", which is reserved" contributor {namespace = Just ""}

refusesAnotherApiMajorVersion :: Assertion
refusesAnotherApiMajorVersion =
  refused
    "needs plug-in API 2, and this MediaCopy 3000 speaks API 1"
    base {roles = V.singleton Inspector, api = 2}

refusesAnIdThatIsNotAReverseDomainName :: Assertion
refusesAnIdThatIsNotAReverseDomainName =
  refused
    "has the id \"../escape\", which is not a reverse domain name"
    base {id = PluginId "../escape", roles = V.singleton Inspector}
