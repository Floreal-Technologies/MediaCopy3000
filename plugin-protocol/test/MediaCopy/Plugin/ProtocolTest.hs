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

import MediaCopy.Plugin.Generators qualified as Generators
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.FileInfo qualified as FileInfo
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.Finding qualified as Finding
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.HashValue qualified as HashValue
import MediaCopy.Plugin.Protocol.Initialize qualified as Initialize
import MediaCopy.Plugin.Protocol.InspectFile qualified as InspectFile
import MediaCopy.Plugin.Protocol.InspectPlan qualified as InspectPlan
import MediaCopy.Plugin.Protocol.JobInfo qualified as JobInfo
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)
import MediaCopy.Plugin.Protocol.LogLine qualified as LogLine
import MediaCopy.Plugin.Protocol.Progress qualified as Progress

tests :: TestTree
tests =
  testGroup
    "Protocol"
    [ testProperty "every protocol message round-trips, optional fields left out or not" protocolMessagesRoundTrip
    ]

protocolMessagesRoundTrip :: Property
protocolMessagesRoundTrip = property $ do
  roundTrips (Initialize.Params <$> genInt <*> genInt <*> Generators.genText <*> Generators.genText <*> Generators.genText <*> genMap <*> genMap <*> genVector Gen.enumBounded <*> Generators.genValue)
  roundTrips (Initialize.Result <$> genInt)
  roundTrips (InspectPlan.Params <$> genJob <*> genVector genFile <*> genSize)
  roundTrips (InspectPlan.Result <$> genVector genFinding)
  roundTrips (Contribute.Params <$> genJob <*> genVector genFile)
  roundTrips (Contribute.Result <$> genVector genAuthor <*> genVector (Contribute.FileMetadata <$> Generators.genText <*> Generators.genText) <*> Gen.maybe Generators.genText)
  roundTrips (InspectFile.Params <$> Generators.genText <*> Generators.genText <*> genVector (HashValue.HashValue <$> Generators.genText <*> Generators.genText) <*> genSize)
  roundTrips (InspectFile.Result <$> genVector (InspectFile.Annotation <$> Generators.genText <*> Generators.genText <*> Generators.genText) <*> genVector genFinding)
  roundTrips (Progress.Progress <$> Generators.genText <*> Gen.maybe (Gen.double (Range.linearFrac 0 1)))
  roundTrips (LogLine.LogLine <$> Generators.genText <*> Generators.genText)

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
genMap = Map.fromList <$> Gen.list (Range.linear 0 4) ((,) <$> Generators.genText <*> Generators.genValue)

genJob :: Gen Protocol.JobInfo
genJob = JobInfo.JobInfo <$> Generators.genText <*> Generators.genText <*> genVector Generators.genText <*> Gen.maybe Generators.genText

genFile :: Gen Protocol.FileInfo
genFile = FileInfo.FileInfo <$> Generators.genText <*> genSize

genFinding :: Gen Protocol.Finding
genFinding = Finding.Finding <$> Generators.genText <*> Gen.enumBounded <*> Generators.genText <*> Generators.genText

genAuthor :: Gen Contribute.Author
genAuthor = Contribute.Author <$> Generators.genText <*> Gen.maybe Generators.genText <*> Gen.maybe Generators.genText <*> Gen.maybe Generators.genText
