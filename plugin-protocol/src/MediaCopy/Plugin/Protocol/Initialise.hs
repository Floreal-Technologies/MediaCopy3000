{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.Initialise
  ( Params (..)
  , Result (..)
  ) where

import Data.Aeson hiding (Result)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

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
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Params

instance Plugin.JsonSchema Params where
  defName = Just "initialiseParams"
  schema =
    JsonSchema.record @Params
      & JsonSchema.property "job" (JsonSchema.ref "jobKind")
      & JsonSchema.describeProperty "settings" "One value per setting, by key. A setting of kind authors holds an array of authorSlot."
      & JsonSchema.property "entitlement" (object ["type" .= String "null"] & JsonSchema.describe "Always null in API 1.0.")

-- |
-- >>> import Data.Aeson (eitherDecode)
-- >>> eitherDecode "{\"api\":1,\"roles\":[\"inspector\"]}" :: Either String Result
-- Right (Result {api = 1})
data Result = Result
  { api :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Result

instance Plugin.JsonSchema Result where
  defName = Just "initialiseResult"
  schema = JsonSchema.record @Result & JsonSchema.property "api" (object ["const" .= Api.major])
