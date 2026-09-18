{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Plugin.Tracer
  ( Tracer (..)
  , Target (..)
  , Direction (..)
  , silent
  , open
  ) where

import Data.Aeson (Value, decodeStrict, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (modifyMVar_, newMVar)
import Effectful.Exception qualified as E
import Effectful.Time (Time, currentTime)
import Floreal.Commons.Xdg (Xdg, XdgDirectory (XdgState))
import System.OsPath (encodeUtf, (</>))
import splice System.OsPath (osp)

import MediaCopy.Effects.AppDirectory qualified as AppDirectory
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem

data Target = Target
  { pluginId :: Text
  , stage :: Text
  }

data Direction = Out | In | ErrorOutput

data Tracer es = Tracer
  { write :: Direction -> ByteString -> Eff es ()
  , close :: Eff es ()
  }

silent :: Tracer es
silent = Tracer {write = \_ _ -> pure (), close = pure ()}

open :: (FileSystem :> es, Xdg :> es, Time :> es, Concurrent :> es) => Target -> Text -> (Text -> Eff es ()) -> Eff es (Tracer es)
open target pid complain = do
  started <- currentTime
  let name = target.pluginId <> "-" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" started) <> "-" <> target.stage <> "-" <> pid <> ".jsonl"
  opened <- E.try @IOException $ do
    folder <- AppDirectory.ensure XdgState [osp|mediacopy3000|] [osp|plugin-traces|]
    path <- (folder </>) <$> encodeUtf (T.unpack name)
    FileSystem.appendBytes path ""
    pure path
  case opened of
    Left e -> complain ("the trace cannot be written: " <> T.show e) >> pure silent
    Right path -> do
      state <- newMVar True
      let write direction line = modifyMVar_ state $ \case
            False -> pure False
            True -> do
              now <- currentTime
              written <- E.try @IOException (FileSystem.appendBytes path (LBS.toStrict (encode (traceRecord now direction line)) <> "\n"))
              case written of
                Right () -> pure True
                Left e -> do
                  complain ("the trace cannot be written: " <> T.show e)
                  pure False
          close = modifyMVar_ state (\_ -> pure False)
      pure Tracer {write, close}

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
