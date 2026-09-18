-- | What a job records about the tool that made it.
module MediaCopy.Engine.Config
  ( ToolInfo (..)
  , defaultToolInfo
  , JobInstant (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Version (showVersion)

import Paths_mediacopy3000 (version)

data ToolInfo = ToolInfo
  { toolName :: Text
  , toolVersion :: Text
  , hostname :: Text
  }
  deriving stock (Eq, Show)

defaultToolInfo :: ToolInfo
defaultToolInfo =
  ToolInfo
    { toolName = "mediacopy3000"
    , toolVersion = T.pack (showVersion version)
    , hostname = "localhost"
    }

newtype JobInstant = JobInstant UTCTime
  deriving stock (Eq, Show)
