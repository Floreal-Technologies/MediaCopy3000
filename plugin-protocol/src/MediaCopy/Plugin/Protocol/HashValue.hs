{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.HashValue (HashValue (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data HashValue = HashValue
  { algo :: Text
  , value :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire HashValue

instance Plugin.JsonSchema HashValue where
  defName = Just "hashValue"
  schema = JsonSchema.record @HashValue & JsonSchema.property "algo" (JsonSchema.ref "hashAlgo")
