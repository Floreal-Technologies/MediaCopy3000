module MediaCopy.Plugin.ProtocolTest (tests) where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hedgehog (Gen, Property, PropertyT, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Plugin.Generators
import MediaCopy.Plugin.Protocol

tests :: TestTree
tests =
  testGroup
    "Protocol"
    [ testProperty "every protocol message round-trips, optional fields left out or not" protocolMessagesRoundTrip
    ]

protocolMessagesRoundTrip :: Property
protocolMessagesRoundTrip = property $ do
  roundTrips (InitializeParams <$> genInt <*> genInt <*> genText <*> genText <*> genText <*> genMap <*> genMap <*> genVector Gen.enumBounded <*> genValue)
  roundTrips (InitializeResult <$> genInt)
  roundTrips (InspectPlanParams <$> genJob <*> genVector genFile <*> genSize)
  roundTrips (InspectPlanResult <$> genVector genFinding)
  roundTrips (ContributeParams <$> genJob <*> genVector genFile)
  roundTrips (ContributeResult <$> genVector genAuthor <*> genVector (FileMetadata <$> genText <*> genText) <*> Gen.maybe genText)
  roundTrips (InspectFileParams <$> genText <*> genText <*> genVector (HashValue <$> genText <*> genText) <*> genSize)
  roundTrips (InspectFileResult <$> genVector (Annotation <$> genText <*> genText <*> genText) <*> genVector genFinding)
  roundTrips (Progress <$> genText <*> Gen.maybe (Gen.double (Range.linearFrac 0 1)))
  roundTrips (LogLine <$> genText <*> genText)

roundTrips :: (Eq a, Show a, ToJSON a, FromJSON a) => Gen a -> PropertyT IO ()
roundTrips gen = do
  message <- forAll gen
  tripping message encode eitherDecode

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)

genSize :: Gen Int64
genSize = Gen.int64 (Range.exponential 0 maxBound)

genVector :: Gen a -> Gen (Vector a)
genVector gen = V.fromList <$> Gen.list (Range.linear 0 4) gen

genMap :: Gen (Map Text Value)
genMap = Map.fromList <$> Gen.list (Range.linear 0 4) ((,) <$> genText <*> genValue)

genJob :: Gen JobInfo
genJob = JobInfo <$> genText <*> genText <*> genVector genText <*> Gen.maybe genText

genFile :: Gen FileInfo
genFile = FileInfo <$> genText <*> genSize

genFinding :: Gen Finding
genFinding = Finding <$> genText <*> Gen.enumBounded <*> genText <*> genText

genAuthor :: Gen Author
genAuthor = Author <$> genText <*> Gen.maybe genText <*> Gen.maybe genText <*> Gen.maybe genText
