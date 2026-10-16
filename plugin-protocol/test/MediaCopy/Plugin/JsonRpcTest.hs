module MediaCopy.Plugin.JsonRpcTest (tests) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Text (Text)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Plugin.Generators
import MediaCopy.Plugin.JsonRpc

tests :: TestTree
tests =
  testGroup
    "JsonRpc"
    [ testProperty "requests, replies, errors and notifications round-trip" jsonRpcRoundTrips
    , testCase "the core refuses requests from a plug-in and lines that are not JSON" theCoreRefusesRequestsAndNonJson
    ]

jsonRpcRoundTrips :: Property
jsonRpcRoundTrips = property $ do
  requestId <- forAll (Gen.int (Range.linearFrom 0 minBound maxBound))
  method <- forAll genText
  params <- forAll genValue
  rpcError <- forAll (RpcError <$> Gen.int (Range.linearFrom 0 minBound maxBound) <*> genText)
  decodeRequest (encodeRequest requestId method params) === Right Request {requestId = Just requestId, method, params}
  decodeIncoming (encodeReply requestId params) === Right (Reply requestId (Right params))
  decodeIncoming (encodeFailure requestId rpcError) === Right (Reply requestId (Left rpcError))
  decodeIncoming (notification method params) === Right (Notify method params)

notification :: Text -> Value -> ByteString
notification method params =
  object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params]
    & encode
    & LBS.toStrict

theCoreRefusesRequestsAndNonJson :: Assertion
theCoreRefusesRequestsAndNonJson = do
  assertBool "a request from a plug-in" (isLeft (decodeIncoming (encodeRequest 1 "ask" Null)))
  assertBool "a line that is not JSON" (isLeft (decodeIncoming "not json"))
