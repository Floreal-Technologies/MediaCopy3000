module MediaCopy.Interface.View.PlanSheetTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Interface.View.PlanSheet (sealChoice)

tests :: TestTree
tests =
  testGroup
    "Interface.View.PlanSheet"
    [ testCase "sealChoice maps the sealing and anyway switches to a seal policy" sealChoiceMapsTheSwitches
    ]

sealChoiceMapsTheSwitches :: Assertion
sealChoiceMapsTheSwitches =
  map (\(sealing, anyway, _) -> sealChoice sealing anyway) table @?= map (\(_, _, choice) -> choice) table
  where
    table :: List (Bool, Bool, Offload.SealFirst)
    table =
      [ (False, True, Offload.UseHistory)
      , (True, True, Offload.SealBeforeCopy Offload.CopyAnyway)
      , (True, False, Offload.SealBeforeCopy Offload.StopBeforeCopy)
      ]
