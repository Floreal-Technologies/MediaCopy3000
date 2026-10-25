module MediaCopy.Plugin.Protocol.InspectPlan
  ( Params (..)
  , Result (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , files :: Vector Protocol.FileInfo
  , readBudgetBytes :: Int64
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Params

instance HasCodec Params where
  codec =
    JsonSchema.namedObject "inspectPlanParams" $
      Params
        <$> requiredField' "job" .= (.job)
        <*> requiredField' "files" .= (.files)
        <*> requiredField' "readBudgetBytes" .= (.readBudgetBytes)

newtype Result = Result
  { findings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Result

instance HasCodec Result where
  codec =
    JsonSchema.namedObject "inspectPlanResult" $
      Result
        <$> requiredField' "findings" .= (.findings)
