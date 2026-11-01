module MediaCopy.Plugin.Protocol.LogLine (LogLine (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data LogLine = LogLine
  { level :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec LogLine

instance HasCodec LogLine where
  codec =
    JsonSchema.namedObject "logLine" $
      LogLine
        <$> requiredField "level" "For example debug, info, warning or error. The event log shows the level before the message." .= (.level)
        <*> requiredField' "message" .= (.message)
