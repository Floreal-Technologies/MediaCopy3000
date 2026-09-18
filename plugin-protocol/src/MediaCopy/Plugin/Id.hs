module MediaCopy.Plugin.Id (Id (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

newtype Id = Id Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Plugin.JsonSchema Id where
  defName = Just "pluginId"
  schema =
    object ["type" .= String "string", "pattern" .= String "^(?!.*\\.\\.)[a-z0-9_-][a-z0-9._-]*\\.[a-z0-9._-]*[a-z0-9_-]$"]
      & JsonSchema.describe "A reverse domain name in lowercase, for example tech.floreal.c2pa-reader. It holds at least one '.', it does not start or end with '.', and it has no '..'. It is also the name of the folder of the plug-in."
