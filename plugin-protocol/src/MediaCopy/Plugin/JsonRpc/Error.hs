module MediaCopy.Plugin.JsonRpc.Error
  ( Error (..)
  , methodNotFound
  , invalidParams
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)

data Error = Error
  { code :: Int
  , message :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON Error where
  parseJSON = withObject "error" $ \o -> Error <$> o .: "code" <*> o .: "message"

instance ToJSON Error where
  toJSON rpcError = object ["code" .= rpcError.code, "message" .= rpcError.message]

methodNotFound :: Text -> Error
methodNotFound method = Error {code = -32601, message = "unknown method " <> method}

invalidParams :: Text -> Error
invalidParams problem = Error {code = -32602, message = problem}
