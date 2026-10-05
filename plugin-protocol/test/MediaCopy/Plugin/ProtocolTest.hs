module MediaCopy.Plugin.ProtocolTest (tests) where

import Data.Aeson (decode)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.Protocol

tests :: TestTree
tests =
  testGroup
    "Protocol"
    [ testCase "decodes an inspect/plan result" decodesAnInspectPlanResult
    , testCase "decodes a contribute result without its optional fields" decodesAContributeResultWithoutItsOptionalFields
    ]

decodesAnInspectPlanResult :: Assertion
decodesAnInspectPlanResult =
  decode "{\"findings\":[{\"key\":\"c2pa-present\",\"severity\":\"warning\",\"title\":\"2 clips carry Content Credentials\",\"detail\":\"A001C001.MP4, A001C002.MP4\"}]}"
    @?= Just
      InspectPlanResult
        { findings =
            V.singleton
              Finding
                { key = "c2pa-present"
                , severity = Warning
                , title = "2 clips carry Content Credentials"
                , detail = "A001C001.MP4, A001C002.MP4"
                }
        }

decodesAContributeResultWithoutItsOptionalFields :: Assertion
decodesAContributeResultWithoutItsOptionalFields =
  decode "{\"authors\":[{\"name\":\"Jane Doe\",\"role\":\"DIT\"}],\"fileMetadata\":[]}"
    @?= Just
      ContributeResult
        { authors = V.singleton Author {name = "Jane Doe", email = Nothing, phone = Nothing, role = Just "DIT"}
        , fileMetadata = V.empty
        , manifestMetadata = Nothing
        }
