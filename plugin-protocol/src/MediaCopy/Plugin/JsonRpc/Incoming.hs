module MediaCopy.Plugin.JsonRpc.Incoming
  ( Incoming (..)
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

import MediaCopy.Plugin.JsonRpc.Error qualified as JsonRpc (Error)

data Incoming
  = Reply Int (Either JsonRpc.Error Value)
  | Notify Text Value
  deriving stock (Eq, Show)

parsed :: (FromJSON a) => Value -> Either Text a
parsed = first T.pack . parseEither parseJSON

decode :: ByteString -> Either Text Incoming
decode line = case decodeStrict line of
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
