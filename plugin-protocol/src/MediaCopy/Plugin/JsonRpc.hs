module MediaCopy.Plugin.JsonRpc
  ( -- * Outgoing messages
    encodeRequest

    -- * Incoming messages
  , Incoming (..)
  , RpcError (..)
  , decodeIncoming

    -- * Plug-in side
  , Request (..)
  , decodeRequest
  , encodeReply
  , encodeFailure
  , methodNotFound
  , invalidParams
  ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)

data RpcError = RpcError
  { code :: Int
  , message :: Text
  }
  deriving stock (Eq, Show)

data Incoming
  = Reply Int (Either RpcError Value)
  | Notify Text Value
  deriving stock (Eq, Show)

envelope :: [Pair] -> ByteString
envelope fields = LBS.toStrict (encode (object ("jsonrpc" .= ("2.0" :: Text) : fields)))

parsed :: (FromJSON a) => Value -> Either Text a
parsed = first T.pack . parseEither parseJSON

encodeRequest :: Int -> Text -> Value -> ByteString
encodeRequest requestId method params =
  envelope ["id" .= requestId, "method" .= method, "params" .= params]

decodeIncoming :: ByteString -> Either Text Incoming
decodeIncoming line = case decodeStrict line of
  Just (Object o) -> case (KeyMap.lookup "id" o, KeyMap.lookup "method" o) of
    (Just _, Just _) -> Left "a request from a plug-in: the core accepts none"
    (Just idValue, Nothing) -> do
      requestId <- parsed idValue
      case (KeyMap.lookup "result" o, KeyMap.lookup "error" o) of
        (_, Just errorValue) -> Reply requestId . Left <$> parsed errorValue
        (Just result, Nothing) -> Right (Reply requestId (Right result))
        (Nothing, Nothing) -> Left "a reply with no result and no error"
    (Nothing, Just methodValue) -> do
      method <- parsed methodValue
      Right (Notify method (fromMaybe Null (KeyMap.lookup "params" o)))
    (Nothing, Nothing) -> Left "a message with no id and no method"
  _ -> Left ("not a JSON object: " <> T.take 200 (decodeUtf8Lenient line))

instance FromJSON RpcError where
  parseJSON = withObject "error" $ \o -> RpcError <$> o .: "code" <*> o .: "message"

instance ToJSON RpcError where
  toJSON rpcError = object ["code" .= rpcError.code, "message" .= rpcError.message]

data Request = Request
  { requestId :: Maybe Int
  , method :: Text
  , params :: Value
  }
  deriving stock (Eq, Show)

decodeRequest :: ByteString -> Either Text Request
decodeRequest line = case decodeStrict line of
  Just (Object o) -> case KeyMap.lookup "method" o of
    Just (String method) -> do
      requestId <- traverse parsed (KeyMap.lookup "id" o)
      Right Request {requestId, method, params = fromMaybe Null (KeyMap.lookup "params" o)}
    _ -> Left "a message with no method"
  _ -> Left ("not a JSON object: " <> T.take 200 (decodeUtf8Lenient line))

encodeReply :: Int -> Value -> ByteString
encodeReply requestId result =
  envelope ["id" .= requestId, "result" .= result]

encodeFailure :: Int -> RpcError -> ByteString
encodeFailure requestId rpcError =
  envelope ["id" .= requestId, "error" .= rpcError]

methodNotFound :: Text -> RpcError
methodNotFound method = RpcError {code = -32601, message = "unknown method " <> method}

invalidParams :: Text -> RpcError
invalidParams problem = RpcError {code = -32602, message = problem}
