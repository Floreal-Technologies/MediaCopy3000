module MediaCopy.Plugin.JsonRpcTest (tests) where

import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.JsonRpc

tests :: TestTree
tests =
  testGroup
    "JsonRpc"
    [ testCase "encodes a request" encodesARequest
    , testCase "decodes a reply with a result" decodesAReplyWithAResult
    , testCase "decodes a reply with an error" decodesAReplyWithAnError
    , testCase "decodes a notification" decodesANotification
    , testCase "refuses a request from a plug-in" refusesARequestFromAPlugin
    , testCase "refuses a line that is not JSON" refusesALineThatIsNotJson
    ]

encodesARequest :: Assertion
encodesARequest =
  encodeRequest 7 "inspect/plan" (object [])
    @?= "{\"id\":7,\"jsonrpc\":\"2.0\",\"method\":\"inspect/plan\",\"params\":{}}"

decodesAReplyWithAResult :: Assertion
decodesAReplyWithAResult =
  decodeIncoming "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"findings\":[]}}"
    @?= Right (Reply 7 (Right (object ["findings" .= ([] :: [Value])])))

decodesAReplyWithAnError :: Assertion
decodesAReplyWithAnError =
  decodeIncoming "{\"jsonrpc\":\"2.0\",\"id\":7,\"error\":{\"code\":-32601,\"message\":\"no such method\"}}"
    @?= Right (Reply 7 (Left RpcError {code = -32601, message = "no such method"}))

decodesANotification :: Assertion
decodesANotification =
  decodeIncoming "{\"jsonrpc\":\"2.0\",\"method\":\"$/progress\",\"params\":{\"message\":\"hashing\"}}"
    @?= Right (Notify "$/progress" (object ["message" .= String "hashing"]))

refusesARequestFromAPlugin :: Assertion
refusesARequestFromAPlugin =
  decodeIncoming "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ask\"}"
    @?= Left "a request from a plug-in: the core accepts none"

refusesALineThatIsNotJson :: Assertion
refusesALineThatIsNotJson =
  decodeIncoming "not json" @?= Left "not a JSON object: not json"
