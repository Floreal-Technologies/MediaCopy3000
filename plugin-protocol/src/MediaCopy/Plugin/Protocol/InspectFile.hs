module MediaCopy.Plugin.Protocol.InspectFile
  ( Params (..)
  , Result (..)
  , Annotation (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.HashValue qualified as Protocol (HashValue)

data Params = Params
  { path :: Text
  , location :: Text
  , hashes :: Vector Protocol.HashValue
  , size :: Int64
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Params

instance HasCodec Params where
  codec =
    JsonSchema.namedObject "inspectFileParams" $
      Params
        <$> requiredField' "path" .= (.path)
        <*> requiredField' "location" .= (.location)
        <*> requiredField' "hashes" .= (.hashes)
        <*> requiredField' "size" .= (.size)

data Annotation = Annotation
  { key :: Text
  , label :: Text
  , value :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Annotation

instance HasCodec Annotation where
  codec =
    JsonSchema.namedObject "annotation" $
      Annotation
        <$> requiredField' "key" .= (.key)
        <*> requiredField' "label" .= (.label)
        <*> requiredField' "value" .= (.value)

data Result = Result
  { annotations :: Vector Annotation
  , warnings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Result

instance HasCodec Result where
  codec =
    JsonSchema.namedObject "inspectFileResult" $
      Result
        <$> requiredField' "annotations" .= (.annotations)
        <*> requiredField' "warnings" .= (.warnings)
