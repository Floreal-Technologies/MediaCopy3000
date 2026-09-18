{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.InspectFile
  ( Params (..)
  , Result (..)
  , Annotation (..)
  ) where

import Data.Aeson hiding (Result)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.HashValue qualified as Protocol (HashValue)

data Params = Params
  { path :: Text
  , location :: Text
  , hashes :: Vector Protocol.HashValue
  , size :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Params

instance Plugin.JsonSchema Params where
  defName = Just "inspectFileParams"
  schema = JsonSchema.record @Params

data Annotation = Annotation
  { key :: Text
  , label :: Text
  , value :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Annotation

data Result = Result
  { annotations :: Vector Annotation
  , warnings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Result

instance Plugin.JsonSchema Result where
  defName = Just "inspectFileResult"
  schema = JsonSchema.record @Result

instance Plugin.JsonSchema Annotation where
  defName = Just "annotation"
  schema = JsonSchema.record @Annotation
