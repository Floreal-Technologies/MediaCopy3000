module MediaCopy.Plugin.Protocol.Finding
  ( Finding (..)
  , Severity (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data Severity = Blocker | Warning
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Severity

instance HasCodec Severity where
  codec =
    named "severity" $ boundedEnumCodec $ \case
      Blocker -> "blocker"
      Warning -> "warning"

data Finding = Finding
  { key :: Text
  , severity :: Severity
  , title :: Text
  , detail :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Finding

instance HasCodec Finding where
  codec =
    JsonSchema.namedObject "finding" $
      Finding
        <$> requiredField' "key" .= (.key)
        <*> requiredField' "severity" .= (.severity)
        <*> requiredField' "title" .= (.title)
        <*> requiredField' "detail" .= (.detail)
