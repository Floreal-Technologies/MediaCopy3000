module MediaCopy.Plugin.Protocol.Progress (Progress (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data Progress = Progress
  { message :: Text
  , fraction :: Maybe Double
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Progress

instance HasCodec Progress where
  codec =
    JsonSchema.namedObject "progress" $
      Progress
        <$> requiredField' "message" .= (.message)
        <*> optionalFieldOrNullWith' "fraction" fractionCodec .= (.fraction)
    where
      fractionCodec = dimapCodec toRealFloat fromFloatDigits (scientificWithBoundsCodec (Bounds (Just 0) (Just 1)))
