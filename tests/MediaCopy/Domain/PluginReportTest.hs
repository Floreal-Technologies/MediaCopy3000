module MediaCopy.Domain.PluginReportTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginReport qualified as PluginReport

tests :: TestTree
tests =
  testGroup
    "Domain.PluginReport"
    [ testCase "a report of files left or not inspected counts the files in words" reportCountsTheFiles
    ]

reportCountsTheFiles :: Assertion
reportCountsTheFiles =
  for_
    [ (PluginReport.InspectionsLeft 12, "plug-ins: 12 files left to inspect")
    , (PluginReport.NotInspected probe 3, "plug-in Probe: 3 files not inspected")
    , (PluginReport.InspectionsLeft 1, "plug-ins: 1 file left to inspect")
    ]
    (\(report, line) -> display report @?= line)

probe :: PluginRef
probe = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}
