{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.Contribute
  ( Params (..)
  , Result (..)
  , Author (..)
  , FileMetadata (..)
  ) where

import Data.Aeson hiding (Result)
import Data.Function ((&))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , files :: Vector Protocol.FileInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Params

instance Plugin.JsonSchema Params where
  defName = Just "contributeParams"
  schema = JsonSchema.record @Params

data Author = Author
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , role :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Author

data FileMetadata = FileMetadata
  { path :: Text
  , xml :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire FileMetadata

data Result = Result
  { authors :: Vector Author
  , fileMetadata :: Vector FileMetadata
  , manifestMetadata :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Result

instance Plugin.JsonSchema Author where
  defName = Just "author"
  schema = JsonSchema.record @Author & JsonSchema.describe "Each text holds only characters that XML 1.0 allows."

instance Plugin.JsonSchema FileMetadata where
  defName = Just "fileMetadata"
  schema =
    JsonSchema.record @FileMetadata
      & JsonSchema.describeProperty "path" "The path of one of the files of the request."
      & JsonSchema.describeProperty "xml" "XML elements in the namespace of the plug-in, at most 32 levels deep, with only characters that XML 1.0 allows. Comments and processing instructions are removed."

instance Plugin.JsonSchema Result where
  defName = Just "contributeResult"
  schema = JsonSchema.record @Result
