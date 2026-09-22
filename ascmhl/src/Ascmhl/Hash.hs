module Ascmhl.Hash
  ( HashAlgo (..)
  , AlgoSpec (..)
  , algoSpec
  , preferredAlgo
  , Hash (..)
  , algoFromMhlElement
  , toHex
  , word64ToHex
  , c4FromSha512
  , c4ToBytes
  , digestBytes
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Numeric (showHex)

-- $setup
-- >>> import Data.Text.Display (display)

data HashAlgo = XXH64 | MD5 | SHA1 | C4
  deriving stock (Eq, Ord, Show, Bounded, Enum)

data Hash = Hash
  { algo :: HashAlgo
  , value :: Text
  }
  deriving stock (Eq, Ord, Show)

data AlgoSpec = AlgoSpec
  { mhlElement :: Text
  , readValue :: Text -> Text
  , toDigest :: Text -> Maybe ByteString
  }

-- |
-- >>> (algoSpec SHA1).readValue "AB12"
-- "ab12"
-- >>> (algoSpec C4).readValue "c4Ab"
-- "c4Ab"
algoSpec :: HashAlgo -> AlgoSpec
algoSpec = \case
  XXH64 -> hexSpec "xxh64"
  MD5 -> hexSpec "md5"
  SHA1 -> hexSpec "sha1"
  C4 -> AlgoSpec {mhlElement = "c4", readValue = id, toDigest = c4ToBytes}
  where
    hexSpec element =
      AlgoSpec {mhlElement = element, readValue = \raw -> T.toLower raw, toDigest = hexToBytes}

allAlgos :: Vector HashAlgo
allAlgos = [minBound .. maxBound] & V.fromList

-- |
-- >>> preferredAlgo
-- XXH64
preferredAlgo :: HashAlgo
preferredAlgo = XXH64

-- |
-- >>> map display [minBound .. maxBound :: HashAlgo]
-- ["xxh64","md5","sha1","c4"]
instance Display HashAlgo where
  displayBuilder which = displayBuilder (algoSpec which).mhlElement

-- |
-- >>> algoFromMhlElement "SHA1"
-- Just SHA1
-- >>> algoFromMhlElement "c4"
-- Just C4
-- >>> algoFromMhlElement "blake3"
-- Nothing
algoFromMhlElement :: Text -> Maybe HashAlgo
algoFromMhlElement (T.toLower -> t) =
  V.find (\which -> (algoSpec which).mhlElement == t) allAlgos

-- |
-- >>> toHex (BS.pack [0, 15, 255])
-- "000fff"
toHex :: ByteString -> Text
toHex bs = bs & BS.unpack & map byteHex & T.concat
  where
    byteHex :: Word8 -> Text
    byteHex w = T.justifyRight 2 '0' (T.pack (showHex w ""))

-- |
-- >>> word64ToHex 255
-- "00000000000000ff"
word64ToHex :: Word64 -> Text
word64ToHex w = T.justifyRight 16 '0' (T.pack (showHex w ""))

c4Alphabet :: Text
c4Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- |
-- >>> T.length (c4FromSha512 (BS.replicate 64 0))
-- 90
-- >>> T.take 2 (c4FromSha512 (BS.replicate 64 0))
-- "c4"
c4FromSha512 :: ByteString -> Text
c4FromSha512 digest =
  digest
    & BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) (0 :: Integer)
    & base58
    & (\body -> T.justifyRight 88 '1' body)
    & (\body -> c4Prefix <> body)
  where
    base58 n =
      if n == 0
        then ""
        else
          let (quotient, remainder) = n `divMod` 58
          in base58 quotient <> T.singleton (T.index c4Alphabet (fromIntegral remainder))

c4Prefix :: Text
c4Prefix = "c4"

-- |
-- >>> fmap BS.length (c4ToBytes (c4FromSha512 (BS.replicate 64 0)))
-- Just 64
-- >>> c4ToBytes "c4abc"
-- Nothing
c4ToBytes :: Text -> Maybe ByteString
c4ToBytes t = do
  body <- T.stripPrefix c4Prefix t
  if T.length body == 88 then Just () else Nothing
  n <- T.foldl' (\acc ch -> addDigit acc ch) (Just 0) body
  if n < c4Modulus then Just (integerToBytes 64 n) else Nothing
  where
    addDigit acc ch = do
      n <- acc
      i <- T.findIndex (\c -> c == ch) c4Alphabet
      Just (n * 58 + fromIntegral i)
    c4Modulus = 256 ^ (64 :: Int)

integerToBytes :: Int -> Integer -> ByteString
integerToBytes width n =
  [0 .. width - 1]
    & reverse
    & map (\i -> fromIntegral (shiftR n (8 * i) .&. 0xFF))
    & BS.pack

hexToBytes :: Text -> Maybe ByteString
hexToBytes t =
  if odd (T.length t)
    then Nothing
    else t & T.chunksOf 2 & traverse (\pair -> byteOf pair) & fmap BS.pack
  where
    byteOf pair = case T.unpack pair of
      [hi, lo] -> do
        high <- nibble hi
        low <- nibble lo
        Just (fromIntegral (high * 16 + low))
      _ -> Nothing
    nibble c
      | isDigit c = Just (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
      | otherwise = Nothing

-- |
-- >>> fmap toHex (digestBytes Hash {algo = MD5, value = "0f1e"})
-- Just "0f1e"
-- >>> digestBytes Hash {algo = MD5, value = "0f1"}
-- Nothing
digestBytes :: Hash -> Maybe ByteString
digestBytes h = (algoSpec h.algo).toDigest h.value
