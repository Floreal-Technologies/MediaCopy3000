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

import MediaCopy.Plugin.Generators qualified as Generators
import MediaCopy.Plugin.JsonRpc qualified as JsonRpc
import MediaCopy.Plugin.JsonRpc.Error qualified as Error
import MediaCopy.Plugin.JsonRpc.Incoming qualified as Incoming
import MediaCopy.Plugin.JsonRpc.Request qualified as Request

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
  method <- forAll Generators.genText
  params <- forAll Generators.genValue
  rpcError <- forAll (Error.Error <$> Gen.int (Range.linearFrom 0 minBound maxBound) <*> Generators.genText)
  Request.decode (JsonRpc.encodeRequest requestId method params) === Right Request.Request {requestId = Just requestId, method, params}
  Incoming.decode (JsonRpc.encodeReply requestId params) === Right (Incoming.Reply requestId (Right params))
  Incoming.decode (JsonRpc.encodeFailure requestId rpcError) === Right (Incoming.Reply requestId (Left rpcError))
  Incoming.decode (notification method params) === Right (Incoming.Notify method params)

notification :: Text -> Value -> ByteString
notification method params =
  object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params]
    & encode
    & LBS.toStrict

theCoreRefusesRequestsAndNonJson :: Assertion
theCoreRefusesRequestsAndNonJson = do
  assertBool "a request from a plug-in" (isLeft (Incoming.decode (JsonRpc.encodeRequest 1 "ask" Null)))
  assertBool "a line that is not JSON" (isLeft (Incoming.decode "not json"))
