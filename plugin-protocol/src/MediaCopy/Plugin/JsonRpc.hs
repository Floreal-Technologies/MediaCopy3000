module MediaCopy.Plugin.JsonRpc
  ( encodeRequest
  , encodeReply
  , encodeFailure
  ) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (List)
import Data.Text (Text)

import MediaCopy.Plugin.JsonRpc.Error qualified as JsonRpc (Error)

envelope :: List Pair -> ByteString
envelope fields = LBS.toStrict (encode (object ("jsonrpc" .= ("2.0" :: Text) : fields)))

encodeRequest :: Int -> Text -> Value -> ByteString
encodeRequest requestId method params =
  envelope ["id" .= requestId, "method" .= method, "params" .= params]

encodeReply :: Int -> Value -> ByteString
encodeReply requestId result =
  envelope ["id" .= requestId, "result" .= result]

encodeFailure :: Int -> JsonRpc.Error -> ByteString
encodeFailure requestId rpcError =
  envelope ["id" .= requestId, "error" .= rpcError]
