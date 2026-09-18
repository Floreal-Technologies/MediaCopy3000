{-# LANGUAGE DerivingVia #-}

module MediaCopy.Plugin.Protocol.InspectPlan
  ( Params (..)
  , Result (..)
  ) where

import Data.Aeson hiding (Result)
import Data.Int (Int64)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , files :: Vector Protocol.FileInfo
  , readBudgetBytes :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Params

instance Plugin.JsonSchema Params where
  defName = Just "inspectPlanParams"
  schema = JsonSchema.record @Params

newtype Result = Result
  { findings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Result

instance Plugin.JsonSchema Result where
  defName = Just "inspectPlanResult"
  schema = JsonSchema.record @Result
