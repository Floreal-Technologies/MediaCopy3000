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

    -- * Schemas without a type
  , jobKindSchema
  , hashAlgoSchema
  ) where

import Data.Aeson
import Data.Function ((&))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic, Rep)

import MediaCopy.Plugin.JsonSchema
import MediaCopy.Plugin.Manifest (Capability, apiMajor)

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

-- |
-- >>> import Data.Aeson (eitherDecode)
-- >>> eitherDecode "{\"api\":1,\"roles\":[\"inspector\"]}" :: Either String InitializeResult
-- Right (InitializeResult {api = 1})
data InitializeResult = InitializeResult
  { api :: Int
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
  deriving stock (Bounded, Enum, Eq, Ord, Show)

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

jobKindSchema :: Value
jobKindSchema = object ["enum" .= (["offload", "verify", "seal"] :: [Text])]

hashAlgoSchema :: Value
hashAlgoSchema =
  object ["enum" .= (["xxh64", "md5", "sha1", "c4"] :: [Text])]
    & describe "The name of a hash format, as the element name in an ASC MHL manifest."

instance JsonSchema Severity where
  defName = Just "severity"
  schema = enumOf @Severity

instance JsonSchema InitializeParams where
  defName = Just "initializeParams"
  schema =
    record @InitializeParams
      & property "job" (ref "jobKind")
      & describeProperty "settings" "One value per setting, by key. A setting of kind authors holds an array of authorSlot."
      & property "entitlement" (object ["type" .= String "null"] & describe "Always null in API 1.0.")

instance JsonSchema InitializeResult where
  defName = Just "initializeResult"
  schema = record @InitializeResult & property "api" (object ["const" .= apiMajor])

instance JsonSchema JobInfo where
  defName = Just "jobInfo"
  schema =
    record @JobInfo
      & property "kind" (ref "jobKind")
      & property "hashFormat" (ref "hashAlgo")

instance JsonSchema FileInfo where
  defName = Just "fileInfo"
  schema = record @FileInfo

instance JsonSchema Finding where
  defName = Just "finding"
  schema = record @Finding

instance JsonSchema InspectPlanParams where
  defName = Just "inspectPlanParams"
  schema = record @InspectPlanParams

instance JsonSchema InspectPlanResult where
  defName = Just "inspectPlanResult"
  schema = record @InspectPlanResult

instance JsonSchema Author where
  defName = Just "author"
  schema = record @Author & describe "Each text holds only characters that XML 1.0 allows."

instance JsonSchema FileMetadata where
  defName = Just "fileMetadata"
  schema =
    record @FileMetadata
      & describeProperty "path" "The path of one of the files of the request."
      & describeProperty "xml" "XML elements in the namespace of the plug-in, at most 32 levels deep, with only characters that XML 1.0 allows. Comments and processing instructions are removed."

instance JsonSchema ContributeParams where
  defName = Just "contributeParams"
  schema = record @ContributeParams

instance JsonSchema ContributeResult where
  defName = Just "contributeResult"
  schema = record @ContributeResult

instance JsonSchema HashValue where
  defName = Just "hashValue"
  schema = record @HashValue & property "algo" (ref "hashAlgo")

instance JsonSchema InspectFileParams where
  defName = Just "inspectFileParams"
  schema = record @InspectFileParams

instance JsonSchema Annotation where
  defName = Just "annotation"
  schema = record @Annotation

instance JsonSchema InspectFileResult where
  defName = Just "inspectFileResult"
  schema = record @InspectFileResult

instance JsonSchema Progress where
  defName = Just "progress"
  schema = record @Progress & property "fraction" (object ["type" .= String "number", "minimum" .= (0 :: Int), "maximum" .= (1 :: Int)])

instance JsonSchema LogLine where
  defName = Just "logLine"
  schema = record @LogLine & describeProperty "level" "For example debug, info, warning or error. The event log shows the level before the message."
