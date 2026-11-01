module MediaCopy.Plugin.Protocol.Method
  ( initialise
  , inspectPlan
  , contribute
  , inspectFile
  , shutdown
  , jobFinished
  , progress
  , log
  ) where

import Data.Text (Text)
import Prelude hiding (log)

initialise, inspectPlan, contribute, inspectFile, shutdown, jobFinished :: Text
initialise = "initialise"
inspectPlan = "inspect/plan"
contribute = "contribute"
inspectFile = "inspect/file"
shutdown = "shutdown"
jobFinished = "job/finished"

progress, log :: Text
progress = "$/progress"
log = "$/log"
