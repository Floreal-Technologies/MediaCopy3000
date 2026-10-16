module Ascmhl.HashTest (tests) where

import Control.Monad (unless)
import Crypto.Hash.SHA512 qualified as SHA512
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower, toUpper)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Ascmhl.Hash

tests :: TestTree
tests =
  testGroup
    "Ascmhl.Hash"
    [ testProperty "a hex digest decodes back to its bytes in either case" hexDigestsRoundTrip
    , testProperty "an algorithm name reads back in any case, and no other name reads" algoNamesRoundTrip
    , testProperty "a c4 ID decodes back to its digest and matches the reference tool" c4RoundTrips
    ]

hexDigestsRoundTrip :: Property
hexDigestsRoundTrip = property $ do
  algo <- forAll (Gen.element [XXH64, MD5, SHA1])
  bytes <- forAll (Gen.bytes (Range.linear 1 64))
  word <- forAll (Gen.word64 Range.constantBounded)
  let hex = toHex bytes
  digestBytes (Hash algo hex) === Just bytes
  digestBytes (Hash algo (T.toUpper hex)) === Just bytes
  digestBytes (Hash algo (T.cons 'z' (T.drop 1 hex))) === Nothing
  digestBytes (Hash XXH64 (word64ToHex word)) === Just (bigEndian word)

bigEndian :: Word64 -> ByteString
bigEndian word = [7, 6 .. 0] & map (\i -> fromIntegral (shiftR word (8 * i))) & BS.pack

algoNamesRoundTrip :: Property
algoNamesRoundTrip = property $ do
  algo <- forAll (Gen.enumBounded :: Gen HashAlgo)
  cased <- forAll (anyCase (display algo))
  other <- forAll (Gen.text (Range.linear 0 6) Gen.alphaNum)
  algoFromMhlElement cased === Just algo
  unless (T.toLower other `elem` map display [minBound .. maxBound :: HashAlgo]) (algoFromMhlElement other === Nothing)

anyCase :: Text -> Gen Text
anyCase name = T.pack <$> traverse (\c -> Gen.element [toLower c, toUpper c]) (T.unpack name)

c4RoundTrips :: Property
c4RoundTrips = property $ do
  digest <- forAll (Gen.bytes (Range.singleton 64))
  let c4 = c4FromSha512 digest
  assert ("c4" `T.isPrefixOf` c4)
  T.length c4 === 90
  c4ToBytes c4 === Just digest
  c4FromSha512 (SHA512.hash "hello")
    === "c447Fm3BJZQ62765jMZJH4m28hrDM7Szbj9CUmj4F4gnvyDYXYz4WfnK2nYRhFvRgYEectEXYBYWLDpLo6XGNAfKdt"
