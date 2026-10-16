module Ascmhl.Generators
  ( genSegments
  , genRelPath
  , genHashOf
  , genHash
  , genInstant
  , genWords
  ) where

import Data.Functor ((<&>))
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addDays, fromGregorian, picosecondsToDiffTime)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ascmhl.Hash
import Ascmhl.Path (RelPath (..))

genSegments :: Gen (List Text)
genSegments =
  Gen.list (Range.linear 1 4) (Gen.filter (\segment -> segment /= "..") (Gen.text (Range.linear 1 10) (Gen.element ("abcXYZ019._- " :: String))))

genRelPath :: Gen RelPath
genRelPath = genSegments <&> T.intercalate "/" <&> RelPath

genHashOf :: HashAlgo -> Gen Hash
genHashOf algo =
  Hash algo <$> case algo of
    XXH64 -> word64ToHex <$> Gen.word64 Range.constantBounded
    MD5 -> toHex <$> Gen.bytes (Range.singleton 16)
    SHA1 -> toHex <$> Gen.bytes (Range.singleton 20)
    C4 -> c4FromSha512 <$> Gen.bytes (Range.singleton 64)

genHash :: Gen Hash
genHash = Gen.enumBounded >>= genHashOf

genInstant :: Gen UTCTime
genInstant = do
  day <- Gen.integral (Range.constant 0 73000)
  picoseconds <- Gen.integral (Range.constant 0 (86400 * 1000000000000 - 1))
  pure (UTCTime (addDays day (fromGregorian 1900 1 1)) (picosecondsToDiffTime picoseconds))

genWords :: Gen Text
genWords = Gen.text (Range.linear 0 12) (Gen.element ("abcXYZ 019-_.&<>\"'" :: String))
