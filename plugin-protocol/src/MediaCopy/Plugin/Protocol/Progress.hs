{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.Progress (Progress (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Progress = Progress
  { message :: Text
  , fraction :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Progress

instance Plugin.JsonSchema Progress where
  defName = Just "progress"
  schema = JsonSchema.record @Progress & JsonSchema.property "fraction" (object ["type" .= String "number", "minimum" .= (0 :: Int), "maximum" .= (1 :: Int)])
