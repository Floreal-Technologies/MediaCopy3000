{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.FileInfo (FileInfo (..)) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data FileInfo = FileInfo
  { path :: Text
  , size :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire FileInfo

instance Plugin.JsonSchema FileInfo where
  defName = Just "fileInfo"
  schema = JsonSchema.record @FileInfo
