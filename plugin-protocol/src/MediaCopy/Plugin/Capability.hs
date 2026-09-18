module MediaCopy.Plugin.Capability
  ( Capability (..)
  , toText
  , hooks
  ) where

import Data.Aeson
import Data.List (List)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Capability = FilesRead | PlanInspect | FilesInspect | Block | ManifestWrite
  deriving stock (Bounded, Enum, Eq, Ord, Show)

toText :: Capability -> Text
toText = \case
  FilesRead -> "files.read"
  PlanInspect -> "plan.inspect"
  FilesInspect -> "files.inspect"
  Block -> "block"
  ManifestWrite -> "manifest.write"

instance ToJSON Capability where
  toJSON capability = String (toText capability)

hooks :: List Capability
hooks = [PlanInspect, FilesInspect, ManifestWrite]

instance FromJSON Capability where
  parseJSON = withText "capability" $ \found -> case lookup found [(toText value, value) | value <- [minBound ..]] of
    Just value -> pure value
    Nothing -> fail ("unknown capability " <> show found)

instance Plugin.JsonSchema Capability where
  defName = Just "capability"
  schema = JsonSchema.enumOf @Capability
