module Ascmhl.Hex
  ( fromBytes
  , fromWord64
  , toBytes
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Numeric (showHex)

-- |
-- >>> fromBytes (BS.pack [0, 15, 255])
-- "000fff"
fromBytes :: ByteString -> Text
fromBytes bs = bs & BS.unpack & map byteHex & T.concat
  where
    byteHex :: Word8 -> Text
    byteHex w = T.justifyRight 2 '0' (T.pack (showHex w ""))

-- |
-- >>> fromWord64 255
-- "00000000000000ff"
fromWord64 :: Word64 -> Text
fromWord64 w = T.justifyRight 16 '0' (T.pack (showHex w ""))

toBytes :: Text -> Maybe ByteString
toBytes t =
  if odd (T.length t)
    then Nothing
    else t & T.chunksOf 2 & traverse byteOf & fmap BS.pack
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
