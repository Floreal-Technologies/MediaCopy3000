module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value (Null), fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.JsonRpc qualified as JsonRpc
import MediaCopy.Plugin.JsonRpc.Error qualified as Error
import MediaCopy.Plugin.JsonRpc.Request qualified as JsonRpc (Request)
import MediaCopy.Plugin.JsonRpc.Request qualified as Request
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.Initialise qualified as Initialise
import System.IO (hFlush, hSetBinaryMode, isEOF, stdin, stdout)

import MediaCopy.Credits

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  serve V.empty

serve :: Vector Contribute.Author -> IO ()
serve slots =
  isEOF >>= \case
    True -> pure ()
    False ->
      BS8.hGetLine stdin >>= \line -> case Request.decode (BS8.strip line) of
        Left _ -> serve slots
        Right request -> case request.method of
          "initialise" -> withParams request $ \(params :: Initialise.Params) -> do
            reply request (Initialise.Result {api = Api.major})
            serve (slotsOf params.settings)
          "contribute" -> withParams request $ \(_ :: Contribute.Params) -> do
            reply request (contribution slots)
            serve slots
          "shutdown" -> reply request Null
          method -> do
            mapM_ (\requestId -> send (JsonRpc.encodeFailure requestId (Error.methodNotFound method))) request.requestId
            serve slots
  where
    withParams :: (FromJSON p) => JsonRpc.Request -> (p -> IO ()) -> IO ()
    withParams request use = case fromJSON request.params of
      Aeson.Success params -> use params
      Aeson.Error problem -> do
        mapM_ (\requestId -> send (JsonRpc.encodeFailure requestId (Error.invalidParams (T.pack problem)))) request.requestId
        serve slots

reply :: (ToJSON r) => JsonRpc.Request -> r -> IO ()
reply request result = mapM_ (\requestId -> send (JsonRpc.encodeReply requestId (toJSON result))) request.requestId

send :: BS.ByteString -> IO ()
send line = BS.hPut stdout (line <> "\n") >> hFlush stdout
