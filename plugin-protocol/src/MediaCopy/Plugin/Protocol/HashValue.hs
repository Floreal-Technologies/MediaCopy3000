module MediaCopy.Plugin.Protocol.HashValue
  ( HashValue (..)
  , algoCodec
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data HashValue = HashValue
  { algo :: Text
  , value :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec HashValue

instance HasCodec HashValue where
  codec =
    JsonSchema.namedObject "hashValue" $
      HashValue
        <$> requiredFieldWith' "algo" algoCodec .= (.algo)
        <*> requiredField' "value" .= (.value)

-- | The name of a hash format. The schema lists the names.
algoCodec :: JSONCodec Text
algoCodec = named "hashAlgo" textCodec
