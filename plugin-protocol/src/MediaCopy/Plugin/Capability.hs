module MediaCopy.Plugin.Capability
  ( Capability (..)
  , hooks
  ) where

import Data.Aeson
import Data.List (List)
import Data.Text.Display

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Capability
  = FilesRead
  | PlanInspect
  | FilesInspect
  | Block
  | ManifestWrite
  | JobFinished
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display Capability where
  displayBuilder = \case
    FilesRead -> "files.read"
    PlanInspect -> "plan.inspect"
    FilesInspect -> "files.inspect"
    Block -> "block"
    ManifestWrite -> "manifest.write"
    JobFinished -> "job.finished"

instance ToJSON Capability where
  toJSON capability = String (display capability)

hooks :: List Capability
hooks =
  [ PlanInspect
  , FilesInspect
  , ManifestWrite
  , JobFinished
  ]

instance FromJSON Capability where
  parseJSON = withText "capability" $ \found -> case lookup found [(display value, value) | value <- [minBound ..]] of
    Just value -> pure value
    Nothing -> fail ("unknown capability " <> show found)

instance Plugin.JsonSchema Capability where
  defName = Just "capability"
  schema = JsonSchema.enumOf @Capability
