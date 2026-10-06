module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value (Null), fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.JsonRpc
import MediaCopy.Plugin.Manifest (apiMajor)
import MediaCopy.Plugin.Protocol
import System.IO (hFlush, hSetBinaryMode, isEOF, stdin, stdout)

import MediaCopy.Credits

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  serve V.empty

serve :: Vector Author -> IO ()
serve slots =
  isEOF >>= \case
    True -> pure ()
    False ->
      BS8.hGetLine stdin >>= \line -> case decodeRequest (BS8.strip line) of
        Left _ -> serve slots
        Right request -> case request.method of
          "initialize" -> withParams request $ \(params :: InitializeParams) -> do
            reply request (InitializeResult {api = apiMajor})
            serve (slotsOf params.settings)
          "contribute" -> withParams request $ \(_ :: ContributeParams) -> do
            reply request (contribution slots)
            serve slots
          "shutdown" -> reply request Null
          method -> do
            mapM_ (\requestId -> send (encodeFailure requestId (methodNotFound method))) request.requestId
            serve slots
  where
    withParams :: (FromJSON p) => Request -> (p -> IO ()) -> IO ()
    withParams request use = case fromJSON request.params of
      Aeson.Success params -> use params
      Aeson.Error problem -> do
        mapM_ (\requestId -> send (encodeFailure requestId (invalidParams (T.pack problem)))) request.requestId
        serve slots

reply :: (ToJSON r) => Request -> r -> IO ()
reply request result = mapM_ (\requestId -> send (encodeReply requestId (toJSON result))) request.requestId

send :: BS.ByteString -> IO ()
send line = BS.hPut stdout (line <> "\n") >> hFlush stdout
