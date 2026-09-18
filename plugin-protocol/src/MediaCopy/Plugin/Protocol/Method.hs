module MediaCopy.Plugin.Protocol.Method
  ( initialize
  , inspectPlan
  , contribute
  , inspectFile
  , shutdown
  , progress
  , log
  ) where

import Data.Text (Text)
import Prelude hiding (log)

initialize, inspectPlan, contribute, inspectFile, shutdown :: Text
initialize = "initialize"
inspectPlan = "inspect/plan"
contribute = "contribute"
inspectFile = "inspect/file"
shutdown = "shutdown"

progress, log :: Text
progress = "$/progress"
log = "$/log"
