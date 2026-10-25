module MediaCopy.Plugin.Capability
  ( Capability (..)
  , hooks
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List (List)
import Data.Text.Display

data Capability
  = FilesRead
  | PlanInspect
  | FilesInspect
  | Block
  | ManifestWrite
  | JobFinished
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Capability

instance Display Capability where
  displayBuilder = \case
    FilesRead -> "files.read"
    PlanInspect -> "plan.inspect"
    FilesInspect -> "files.inspect"
    Block -> "block"
    ManifestWrite -> "manifest.write"
    JobFinished -> "job.finished"

instance HasCodec Capability where
  codec = named "capability" (boundedEnumCodec display)

hooks :: List Capability
hooks =
  [ PlanInspect
  , FilesInspect
  , ManifestWrite
  , JobFinished
  ]
