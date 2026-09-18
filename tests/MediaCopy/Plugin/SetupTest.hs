module MediaCopy.Plugin.SetupTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import MediaCopy.Plugin.Manifest qualified as Manifest
import System.OsPath (unsafeEncodeUtf)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.Grant qualified as Grant
import MediaCopy.Plugin.Grant qualified as Plugin (Grant)
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Installed qualified as Plugin (Installed)
import MediaCopy.Plugin.Setup qualified as Setup

tests :: TestTree
tests =
  testGroup
    "Plugin.Setup"
    [ testCase "activate needs the plug-in enabled and every declared capability granted or declined" activateNeedsEveryCapabilityAnswered
    ]

activateNeedsEveryCapabilityAnswered :: Assertion
activateNeedsEveryCapabilityAnswered =
  map (\(grantMap, _) -> either (.reason) (const "active") (Setup.activate grantMap probe)) table @?= map snd table
  where
    table :: List (Map Plugin.Id Plugin.Grant, Text)
    table =
      [ (Map.empty, "is not enabled")
      ,
        ( Map.singleton (Id.Id "tech.floreal.probe") Grant.Grant {enabled = True, trace = False, grants = Set.singleton Capability.FilesRead, declined = Set.empty, settings = Map.empty}
        , "asks for plan.inspect, block, which is neither granted nor declined"
        )
      ,
        ( Map.singleton (Id.Id "tech.floreal.probe") Grant.Grant {enabled = True, trace = False, grants = Set.fromList [Capability.FilesRead, Capability.PlanInspect], declined = Set.singleton Capability.Block, settings = Map.empty}
        , "active"
        )
      ]

probe :: Plugin.Installed
probe =
  Installed.Installed
    { folder = unsafeEncodeUtf "/plugins/tech.floreal.probe"
    , manifest =
        Manifest.Manifest
          { id = Id.Id "tech.floreal.probe"
          , name = "Probe"
          , description = "Probes the plan."
          , version = "1.0.0"
          , api = 1
          , namespace = Nothing
          , executable = Map.empty
          , capabilities = V.fromList [Capability.FilesRead, Capability.PlanInspect, Capability.Block]
          , settings = V.empty
          , jobFields = V.empty
          }
    , executable = unsafeEncodeUtf "/plugins/tech.floreal.probe/probe"
    }
