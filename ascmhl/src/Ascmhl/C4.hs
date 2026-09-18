module Ascmhl.C4
  ( fromSha512
  , toBytes
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T

alphabet :: Text
alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- |
-- >>> T.length (fromSha512 (BS.replicate 64 0))
-- 90
-- >>> T.take 2 (fromSha512 (BS.replicate 64 0))
-- "c4"
fromSha512 :: ByteString -> Text
fromSha512 digest =
  digest
    & BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) (0 :: Integer)
    & base58
    & T.justifyRight 88 '1'
    & (prefix <>)
  where
    base58 n =
      if n == 0
        then ""
        else
          let (quotient, remainder) = n `divMod` 58
          in base58 quotient <> T.singleton (T.index alphabet (fromIntegral remainder))

prefix :: Text
prefix = "c4"

-- |
-- >>> fmap BS.length (toBytes (fromSha512 (BS.replicate 64 0)))
-- Just 64
-- >>> toBytes "c4abc"
-- Nothing
toBytes :: Text -> Maybe ByteString
toBytes t = do
  body <- T.stripPrefix prefix t
  if T.length body == 88 then Just () else Nothing
  n <- T.foldl' addDigit (Just 0) body
  if n < modulus then Just (integerToBytes 64 n) else Nothing
  where
    addDigit acc ch = do
      n <- acc
      i <- T.findIndex (\c -> c == ch) alphabet
      Just (n * 58 + fromIntegral i)
    modulus = 256 ^ (64 :: Int)

integerToBytes :: Int -> Integer -> ByteString
integerToBytes width n =
  [0 .. width - 1]
    & reverse
    & map (\i -> fromIntegral (shiftR n (8 * i) .&. 0xFF))
    & BS.pack
