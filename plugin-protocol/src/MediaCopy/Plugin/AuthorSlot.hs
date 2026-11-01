module MediaCopy.Plugin.AuthorSlot (AuthorSlot (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

data AuthorSlot = AuthorSlot
  { role :: Text
  , name :: Maybe Text
  , email :: Maybe Text
  , phone :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec AuthorSlot

instance HasCodec AuthorSlot where
  codec =
    JsonSchema.describedObject "authorSlot" "One author slot of a setting of kind authors. In plugins.json, name, email and phone are the defaults. In initialise.settings, they are the merged values for the job, and slots with no name are removed." $
      AuthorSlot
        <$> requiredField' "role" .= (.role)
        <*> JsonSchema.optionalText "name" .= (.name)
        <*> JsonSchema.optionalText "email" .= (.email)
        <*> JsonSchema.optionalText "phone" .= (.phone)
