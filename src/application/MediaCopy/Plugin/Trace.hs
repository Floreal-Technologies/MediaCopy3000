{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Plugin.Trace
  ( TraceTarget (..)
  , Direction (..)
  , Tracer (..)
  , silentTracer
  , openTracer
  , traceFolder
  , traceRecord
  ) where

import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception (IOException, try)
import Data.Aeson (Value, decodeStrict, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory.OsPath (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.File.OsPath qualified as FileIO
import System.IO (Handle, IOMode (AppendMode), hClose, hFlush)
import System.OsPath (OsPath, encodeUtf, (</>))
import splice System.OsPath (osp)

-- $setup
-- >>> import Data.Time (UTCTime (..), fromGregorian)

data TraceTarget = TraceTarget
  { pluginId :: Text
  , stage :: Text
  }

data Direction = Out | In | ErrorOutput

data Tracer = Tracer
  { write :: Direction -> ByteString -> IO ()
  , close :: IO ()
  }

silentTracer :: Tracer
silentTracer = Tracer {write = \_ _ -> pure (), close = pure ()}

traceFolder :: IO OsPath
traceFolder = getXdgDirectory XdgState [osp|mediacopy3000|] <&> (</> [osp|plugin-traces|])

openTracer :: TraceTarget -> Text -> (Text -> IO ()) -> IO Tracer
openTracer target pid complain = do
  started <- getCurrentTime
  let name = target.pluginId <> "-" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" started) <> "-" <> target.stage <> "-" <> pid <> ".jsonl"
  opened <- try @IOException $ do
    folder <- traceFolder
    createDirectoryIfMissing True folder
    path <- (folder </>) <$> encodeUtf (T.unpack name)
    FileIO.openBinaryFile path AppendMode
  case opened of
    Left e -> complain ("the trace cannot be written: " <> T.show e) >> pure silentTracer
    Right handle -> do
      state <- newMVar (Just handle)
      let write direction line = modifyMVar_ state $ \case
            Nothing -> pure Nothing
            Just h -> do
              now <- getCurrentTime
              written <- try @IOException (BS.hPut h (LBS.toStrict (encode (traceRecord now direction line)) <> "\n") >> hFlush h)
              case written of
                Right () -> pure (Just h)
                Left e -> do
                  complain ("the trace cannot be written: " <> T.show e)
                  closeQuietly h
                  pure Nothing
          close = modifyMVar_ state (\open -> mapM_ closeQuietly open >> pure Nothing)
      pure Tracer {write, close}

closeQuietly :: Handle -> IO ()
closeQuietly h = () <$ try @IOException (hClose h)

-- |
-- >>> let at = UTCTime (fromGregorian 2026 10 5) 0.25
-- >>> traceRecord at In "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"
-- Object (fromList [("dir",String "in"),("message",Object (fromList [("id",Number 1.0),("jsonrpc",String "2.0"),("result",Object (fromList []))])),("time",String "2026-10-05T00:00:00.250Z")])
-- >>> traceRecord at ErrorOutput "not json"
-- Object (fromList [("dir",String "stderr"),("text",String "not json"),("time",String "2026-10-05T00:00:00.250Z")])
traceRecord :: UTCTime -> Direction -> ByteString -> Value
traceRecord at direction line =
  object
    [ "time" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" at
    , "dir" .= directionName
    , case decodeStrict line of
        Just message -> "message" .= (message :: Value)
        Nothing -> "text" .= decodeUtf8Lenient line
    ]
  where
    directionName = case direction of
      Out -> "out" :: Text
      In -> "in"
      ErrorOutput -> "stderr"
