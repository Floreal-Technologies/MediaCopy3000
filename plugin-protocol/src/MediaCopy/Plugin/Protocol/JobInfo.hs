{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.JobInfo (JobInfo (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data JobInfo = JobInfo
  { kind :: Text
  , source :: Text
  , destinations :: Vector Text
  , hashFormat :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire JobInfo

instance Plugin.JsonSchema JobInfo where
  defName = Just "jobInfo"
  schema =
    JsonSchema.record @JobInfo
      & JsonSchema.property "kind" (JsonSchema.ref "jobKind")
      & JsonSchema.property "hashFormat" (JsonSchema.ref "hashAlgo")
