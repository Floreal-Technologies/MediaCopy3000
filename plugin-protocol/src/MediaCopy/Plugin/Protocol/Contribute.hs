module MediaCopy.Plugin.Protocol.Contribute
  ( Params (..)
  , Result (..)
  , Author (..)
  , FileMetadata (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , files :: Vector Protocol.FileInfo
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Params

instance HasCodec Params where
  codec =
    JsonSchema.namedObject "contributeParams" $
      Params
        <$> requiredField' "job" .= (.job)
        <*> requiredField' "files" .= (.files)

data Author = Author
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , role :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Author

instance HasCodec Author where
  codec =
    JsonSchema.describedObject "author" "Each text holds only characters that XML 1.0 allows." $
      Author
        <$> requiredField' "name" .= (.name)
        <*> JsonSchema.optionalText "email" .= (.email)
        <*> JsonSchema.optionalText "phone" .= (.phone)
        <*> JsonSchema.optionalText "role" .= (.role)

data FileMetadata = FileMetadata
  { path :: Text
  , xml :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec FileMetadata

instance HasCodec FileMetadata where
  codec =
    JsonSchema.namedObject "fileMetadata" $
      FileMetadata
        <$> requiredField "path" "The path of one of the files of the request." .= (.path)
        <*> requiredField "xml" "XML elements in the namespace of the plug-in, at most 32 levels deep, with only characters that XML 1.0 allows. Comments and processing instructions are removed." .= (.xml)

data Result = Result
  { authors :: Vector Author
  , fileMetadata :: Vector FileMetadata
  , manifestMetadata :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Result

instance HasCodec Result where
  codec =
    JsonSchema.namedObject "contributeResult" $
      Result
        <$> requiredField' "authors" .= (.authors)
        <*> requiredField' "fileMetadata" .= (.fileMetadata)
        <*> JsonSchema.optionalText "manifestMetadata" .= (.manifestMetadata)
