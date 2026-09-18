{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.LogLine (LogLine (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data LogLine = LogLine
  { level :: Text
  , message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire LogLine

instance Plugin.JsonSchema LogLine where
  defName = Just "logLine"
  schema = JsonSchema.record @LogLine & JsonSchema.describeProperty "level" "For example debug, info, warning or error. The event log shows the level before the message."
