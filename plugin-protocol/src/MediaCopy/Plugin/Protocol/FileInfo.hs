module MediaCopy.Plugin.Protocol.FileInfo (FileInfo (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data FileInfo = FileInfo
  { path :: Text
  , size :: Int64
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec FileInfo

instance HasCodec FileInfo where
  codec =
    JsonSchema.namedObject "fileInfo" $
      FileInfo
        <$> requiredField' "path" .= (.path)
        <*> requiredField' "size" .= (.size)
