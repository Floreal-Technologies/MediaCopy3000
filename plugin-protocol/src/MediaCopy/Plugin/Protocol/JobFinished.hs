module MediaCopy.Plugin.Protocol.JobFinished
  ( Params (..)
  , Outcome (..)
  , Failures (..)
  , Failure (..)
  , ManifestInfo (..)
  , Result (..)
  ) where

import Data.Aeson hiding (Result)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import MediaCopy.Plugin.Internal.Wire (Tagged, Wire)
import MediaCopy.Plugin.Internal.Wire qualified as Wire
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

data Params = Params
  { job :: Protocol.JobInfo
  , outcome :: Outcome
  , manifests :: Vector ManifestInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Params

instance Plugin.JsonSchema Params where
  defName = Just "jobFinishedParams"
  schema = JsonSchema.record @Params

data Outcome
  = AllOk
  | WithFailures Failures
  | Failed Failure
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Tagged Outcome

newtype Failures = Failures
  { count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Failures

newtype Failure = Failure
  { message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Failure

instance Plugin.JsonSchema Outcome where
  defName = Just "jobFinishedOutcome"
  schema = JsonSchema.taggedUnion @Outcome

data ManifestInfo = ManifestInfo
  { destination :: Text
  , path :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire ManifestInfo

instance Plugin.JsonSchema ManifestInfo where
  defName = Just "manifestInfo"
  schema = JsonSchema.record @ManifestInfo

newtype Result = Result
  { warnings :: Vector Protocol.Finding
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Result

instance Plugin.JsonSchema Result where
  defName = Just "jobFinishedResult"
  schema = JsonSchema.record @Result
