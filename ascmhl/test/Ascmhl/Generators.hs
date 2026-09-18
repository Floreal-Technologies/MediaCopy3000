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

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo
import Ascmhl.C4 qualified as C4
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.Hex qualified as Hex
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath

genSegments :: Gen (List Text)
genSegments =
  Gen.list (Range.linear 1 4) (Gen.filter (\segment -> segment /= "..") (Gen.text (Range.linear 1 10) (Gen.element ("abcXYZ019._- " :: String))))

genRelPath :: Gen RelPath
genRelPath = genSegments <&> T.intercalate "/" <&> RelPath.RelPath

genHashOf :: Algo -> Gen Hash
genHashOf algo =
  Hash.Hash algo <$> case algo of
    Algo.XXH64 -> Hex.fromWord64 <$> Gen.word64 Range.constantBounded
    Algo.MD5 -> Hex.fromBytes <$> Gen.bytes (Range.singleton 16)
    Algo.SHA1 -> Hex.fromBytes <$> Gen.bytes (Range.singleton 20)
    Algo.C4 -> C4.fromSha512 <$> Gen.bytes (Range.singleton 64)

genHash :: Gen Hash
genHash = Gen.enumBounded >>= genHashOf

genInstant :: Gen UTCTime
genInstant = do
  day <- Gen.integral (Range.constant 0 73000)
  picoseconds <- Gen.integral (Range.constant 0 (86400 * 1000000000000 - 1))
  pure (UTCTime (addDays day (fromGregorian 1900 1 1)) (picosecondsToDiffTime picoseconds))

genWords :: Gen Text
genWords = Gen.text (Range.linear 0 12) (Gen.element ("abcXYZ 019-_.&<>\"'" :: String))
