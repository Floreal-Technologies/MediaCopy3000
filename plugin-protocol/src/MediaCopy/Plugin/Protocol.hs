{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module MediaCopy.Plugin.Protocol
  ( -- * Shared types
    JobInfo (..)
  , FileInfo (..)
  , HashValue (..)
  , Severity (..)
  , Finding (..)

    -- * initialize
  , methodInitialize
  , InitializeParams (..)
  , InitializeResult (..)

    -- * inspect/plan
  , methodInspectPlan
  , InspectPlanParams (..)
  , InspectPlanResult (..)

    -- * contribute
  , methodContribute
  , ContributeParams (..)
  , ContributeResult (..)
  , Author (..)
  , FileMetadata (..)

    -- * inspect/file
  , methodInspectFile
  , InspectFileParams (..)
  , InspectFileResult (..)
  , Annotation (..)

    -- * shutdown
  , methodShutdown

    -- * Notifications
  , notifyProgress
  , Progress (..)
  , notifyLog
  , LogLine (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic, Rep)

import MediaCopy.Plugin.Manifest (Capability, Role)

methodInitialize, methodInspectPlan, methodContribute, methodInspectFile, methodShutdown :: Text
methodInitialize = "initialize"
methodInspectPlan = "inspect/plan"
methodContribute = "contribute"
methodInspectFile = "inspect/file"
methodShutdown = "shutdown"

notifyProgress, notifyLog :: Text
notifyProgress = "$/progress"
notifyLog = "$/log"

newtype Wire a = Wire a

wireOptions :: Options
wireOptions = defaultOptions {omitNothingFields = True}

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Wire a) where
  toJSON (Wire a) = genericToJSON wireOptions a
  toEncoding (Wire a) = genericToEncoding wireOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Wire a) where
  parseJSON value = Wire <$> genericParseJSON wireOptions value

data InitializeParams = InitializeParams
  { api :: Int
  , minor :: Int
  , app :: Text
  , locale :: Text
  , job :: Text
  , settings :: Map Text Value
  , jobFields :: Map Text Value
  , grants :: Vector Capability
  , entitlement :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InitializeParams

data InitializeResult = InitializeResult
  { api :: Int
  , roles :: Vector Role
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InitializeResult

data JobInfo = JobInfo
  { kind :: Text
  , source :: Text
  , destinations :: Vector Text
  , hashFormat :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire JobInfo

data FileInfo = FileInfo
  { path :: Text
  , size :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire FileInfo

data Severity = Blocker | Warning
  deriving stock (Eq, Ord, Show)

instance ToJSON Severity where
  toJSON = \case
    Blocker -> String "blocker"
    Warning -> String "warning"

instance FromJSON Severity where
  parseJSON = withText "severity" $ \case
    "blocker" -> pure Blocker
    "warning" -> pure Warning
    other -> fail ("unknown severity " <> show other)

data Finding = Finding
  { key :: Text
  , severity :: Severity
  , title :: Text
  , detail :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Finding

data InspectPlanParams = InspectPlanParams
  { job :: JobInfo
  , files :: Vector FileInfo
  , readBudgetBytes :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InspectPlanParams

newtype InspectPlanResult = InspectPlanResult
  { findings :: Vector Finding
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InspectPlanResult

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

data ContributeParams = ContributeParams
  { job :: JobInfo
  , files :: Vector FileInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire ContributeParams

data ContributeResult = ContributeResult
  { authors :: Vector Author
  , fileMetadata :: Vector FileMetadata
  , manifestMetadata :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire ContributeResult

data HashValue = HashValue
  { algo :: Text
  , value :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire HashValue

data InspectFileParams = InspectFileParams
  { path :: Text
  , location :: Text
  , hashes :: Vector HashValue
  , size :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InspectFileParams

data Annotation = Annotation
  { key :: Text
  , label :: Text
  , value :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Annotation

data InspectFileResult = InspectFileResult
  { annotations :: Vector Annotation
  , warnings :: Vector Finding
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire InspectFileResult

data Progress = Progress
  { message :: Text
  , fraction :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire Progress

data LogLine = LogLine
  { level :: Text
  , message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Wire LogLine
