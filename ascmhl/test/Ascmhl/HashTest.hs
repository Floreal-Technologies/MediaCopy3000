module Ascmhl.HashTest (tests) where

import Crypto.Hash.SHA512 qualified as SHA512
import Data.ByteString qualified as BS
import Data.Text.Display (display)
import Test.Tasty
import Test.Tasty.HUnit

import Ascmhl.Hash

tests :: TestTree
tests =
  testGroup
    "Ascmhl.Hash"
    [ testCase "algoFromMhlElement inverts display, ignores case, rejects unknown names" algoFromMhlElementInvertsDisplay
    , testGroup
        "toHex"
        [ testCase "encodes bytes lowercase" encodesBytesLowercase
        ]
    , testGroup
        "word64ToHex"
        [ testCase "zero-pads to 16 chars" zeroPadsTo16Chars
        , testCase "pads small numbers" padsSmallNumbers
        ]
    , testCase "c4FromSha512 matches the reference tool's vectors" c4VectorsMatchTheReference
    , testCase "c4ToBytes round-trips through c4FromSha512" c4RoundTripsThroughBytes
    , testCase "digestBytes decodes hex algos and rejects malformed hex" digestBytesDecodesHexAlgos
    ]

algoFromMhlElementInvertsDisplay :: Assertion
algoFromMhlElementInvertsDisplay = do
  mapM_ (\a -> algoFromMhlElement (display a) @?= Just a) [minBound .. maxBound]
  algoFromMhlElement "XXH64" @?= Just XXH64
  algoFromMhlElement "sha256" @?= Nothing

encodesBytesLowercase :: Assertion
encodesBytesLowercase =
  toHex (BS.pack [0x00, 0xab, 0xff]) @?= "00abff"

zeroPadsTo16Chars :: Assertion
zeroPadsTo16Chars =
  word64ToHex 0xef46db3751d8e999 @?= "ef46db3751d8e999"

padsSmallNumbers :: Assertion
padsSmallNumbers =
  word64ToHex 1 @?= "0000000000000001"

c4VectorsMatchTheReference :: Assertion
c4VectorsMatchTheReference = do
  let sha512Of bytes = SHA512.hash bytes
  c4FromSha512 (sha512Of "")
    @?= "c459dsjfscH38cYeXXYogktxf4Cd9ibshE3BHUo6a58hBXmRQdZrAkZzsWcbWtDg5oQstpDuni4Hirj75GEmTc1sFT"
  c4FromSha512 (sha512Of "hello")
    @?= "c447Fm3BJZQ62765jMZJH4m28hrDM7Szbj9CUmj4F4gnvyDYXYz4WfnK2nYRhFvRgYEectEXYBYWLDpLo6XGNAfKdt"
  c4FromSha512 (sha512Of "The quick brown fox jumps over the lazy dog")
    @?= "c41AA3ASLQfdQwGxaam2okv9uTopf28Piy2pskrf6vQ35fsAKCBzQAv31LTcwMcp3hpDqF4vxgvu3UoPSAN4qZ3Ygd"

c4RoundTripsThroughBytes :: Assertion
c4RoundTripsThroughBytes = do
  let digest = SHA512.hash "hello"
  c4ToBytes (c4FromSha512 digest) @?= Just digest

digestBytesDecodesHexAlgos :: Assertion
digestBytesDecodesHexAlgos = do
  digestBytes (Hash XXH64 "ef46db3751d8e999") @?= Just (BS.pack [0xef, 0x46, 0xdb, 0x37, 0x51, 0xd8, 0xe9, 0x99])
  digestBytes (Hash MD5 "zz") @?= Nothing
