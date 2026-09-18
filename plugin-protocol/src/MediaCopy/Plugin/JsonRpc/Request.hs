module MediaCopy.Plugin.JsonRpc.Request
  ( Request (..)
  , decode
  ) where

import Data.Aeson (FromJSON (..), Value (..), decodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)

data Request = Request
  { requestId :: Maybe Int
  , method :: Text
  , params :: Value
  }
  deriving stock (Eq, Show)

parsed :: (FromJSON a) => Value -> Either Text a
parsed = first T.pack . parseEither parseJSON

decode :: ByteString -> Either Text Request
decode line = case decodeStrict line of
  Just (Object o) -> case KeyMap.lookup "method" o of
    Just (String method) -> do
      requestId <- traverse parsed (KeyMap.lookup "id" o)
      Right Request {requestId, method, params = fromMaybe Null (KeyMap.lookup "params" o)}
    _ -> Left "a message with no method"
  _ -> Left ("not a JSON object: " <> T.take 200 (decodeUtf8Lenient line))
