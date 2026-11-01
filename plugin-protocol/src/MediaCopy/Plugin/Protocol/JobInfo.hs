module MediaCopy.Plugin.Protocol.JobInfo
  ( JobInfo (..)
  , kindCodec
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.HashValue qualified as HashValue

data JobInfo = JobInfo
  { kind :: Text
  , source :: Text
  , destinations :: Vector Text
  , hashFormat :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec JobInfo

instance HasCodec JobInfo where
  codec =
    JsonSchema.namedObject "jobInfo" $
      JobInfo
        <$> requiredFieldWith' "kind" kindCodec .= (.kind)
        <*> requiredField' "source" .= (.source)
        <*> requiredField' "destinations" .= (.destinations)
        <*> optionalFieldWith' "hashFormat" HashValue.algoCodec .= (.hashFormat)

-- | The kind of a job. The schema lists the kinds.
kindCodec :: JSONCodec Text
kindCodec = named "jobKind" textCodec
