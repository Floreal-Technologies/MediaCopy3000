module MediaCopy.Plugin.Protocol.JobFinished
  ( Params (..)
  , Outcome (..)
  , ManifestInfo (..)
  , Result (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , outcome :: Outcome
  , manifests :: Vector ManifestInfo
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Params

instance HasCodec Params where
  codec =
    JsonSchema.namedObject "jobFinishedParams" $
      Params
        <$> requiredField' "job" .= (.job)
        <*> requiredField' "outcome" .= (.outcome)
        <*> requiredField' "manifests" .= (.manifests)

data Outcome
  = AllOk
  | WithFailures Int
  | Failed Text
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Outcome

instance HasCodec Outcome where
  codec = JsonSchema.namedObject "jobFinishedOutcome" (discriminatedUnionCodec "status" encode decode)
    where
      count = requiredField' "count"
      message = requiredField' "message"
      encode = \case
        AllOk -> ("allOk", pureCodec ())
        WithFailures n -> ("withFailures", mapToEncoder n count)
        Failed text -> ("failed", mapToEncoder text message)
      decode =
        HashMap.fromList
          [ ("allOk", ("AllOk", pureCodec AllOk))
          , ("withFailures", ("WithFailures", mapToDecoder WithFailures count))
          , ("failed", ("Failed", mapToDecoder Failed message))
          ]

data ManifestInfo = ManifestInfo
  { destination :: Text
  , path :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec ManifestInfo

instance HasCodec ManifestInfo where
  codec =
    JsonSchema.namedObject "manifestInfo" $
      ManifestInfo
        <$> requiredField' "destination" .= (.destination)
        <*> requiredField' "path" .= (.path)

newtype Result = Result
  { warnings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Result

instance HasCodec Result where
  codec =
    JsonSchema.namedObject "jobFinishedResult" $
      Result
        <$> requiredField' "warnings" .= (.warnings)
