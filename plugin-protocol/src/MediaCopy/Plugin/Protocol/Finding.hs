{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.Finding
  ( Finding (..)
  , Severity (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Severity = Blocker | Warning
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance ToJSON Severity where
  toJSON = \case
    Blocker -> String "blocker"
    Warning -> String "warning"

instance FromJSON Severity where
  parseJSON = withText "severity" $ \case
    "blocker" -> pure Blocker
    "warning" -> pure Warning
    other -> fail ("unknown severity " <> show other)

data Finding = Finding
  { key :: Text
  , severity :: Severity
  , title :: Text
  , detail :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Finding

instance Plugin.JsonSchema Severity where
  defName = Just "severity"
  schema = JsonSchema.enumOf @Severity

instance Plugin.JsonSchema Finding where
  defName = Just "finding"
  schema = JsonSchema.record @Finding
