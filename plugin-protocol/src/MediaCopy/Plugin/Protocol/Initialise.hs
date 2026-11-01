module MediaCopy.Plugin.Protocol.Initialise
  ( Params (..)
  , Result (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.JobInfo qualified as JobInfo

data Params = Params
  { api :: Int
  , minor :: Int
  , app :: Text
  , locale :: Text
  , job :: Text
  , settings :: Map Text Value
  , jobFields :: Map Text Value
  , grants :: Vector Plugin.Capability
  , entitlement :: Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Params

instance HasCodec Params where
  codec =
    JsonSchema.namedObject "initialiseParams" $
      Params
        <$> requiredField' "api" .= (.api)
        <*> requiredField' "minor" .= (.minor)
        <*> requiredField' "app" .= (.app)
        <*> requiredField' "locale" .= (.locale)
        <*> requiredFieldWith' "job" JobInfo.kindCodec .= (.job)
        <*> requiredField "settings" "One value per setting, by key. A setting of kind authors holds an array of authorSlot." .= (.settings)
        <*> requiredField' "jobFields" .= (.jobFields)
        <*> requiredField' "grants" .= (.grants)
        <*> requiredField' "entitlement" .= (.entitlement)

-- |
-- >>> import Data.Aeson (eitherDecode)
-- >>> eitherDecode "{\"api\":1,\"roles\":[\"inspector\"]}" :: Either String Result
-- Right (Result {api = 1})
data Result = Result
  { api :: Int
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Result

instance HasCodec Result where
  codec =
    JsonSchema.namedObject "initialiseResult" $
      Result
        <$> requiredField' "api" .= (.api)
