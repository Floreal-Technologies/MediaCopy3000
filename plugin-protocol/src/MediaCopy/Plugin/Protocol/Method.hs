module MediaCopy.Plugin.Protocol.Method
  ( initialise
  , inspectPlan
  , contribute
  , inspectFile
  , shutdown
  , progress
  , log
  ) where

import Data.Text (Text)
import Prelude hiding (log)

initialise, inspectPlan, contribute, inspectFile, shutdown :: Text
initialise = "initialise"
inspectPlan = "inspect/plan"
contribute = "contribute"
inspectFile = "inspect/file"
shutdown = "shutdown"

progress, log :: Text
progress = "$/progress"
log = "$/log"
