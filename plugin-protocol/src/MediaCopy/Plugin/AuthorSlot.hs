module MediaCopy.Plugin.AuthorSlot (AuthorSlot (..)) where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data AuthorSlot = AuthorSlot
  { role :: Text
  , name :: Text
  , email :: Text
  , phone :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON AuthorSlot where
  parseJSON = withObject "author slot" $ \o -> do
    role <- o .: "role"
    name <- o .:! "name" .!= ""
    email <- o .:! "email" .!= ""
    phone <- o .:! "phone" .!= ""
    pure AuthorSlot {role, name, email, phone}

instance ToJSON AuthorSlot where
  toJSON slot = object ["role" .= slot.role, "name" .= slot.name, "email" .= slot.email, "phone" .= slot.phone]

instance Plugin.JsonSchema AuthorSlot where
  defName = Just "authorSlot"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= ["role" :: Text]
      , "properties"
          .= object
            [ "role" .= object ["type" .= String "string", "minLength" .= (1 :: Int)]
            , "name" .= JsonSchema.string
            , "email" .= JsonSchema.string
            , "phone" .= JsonSchema.string
            ]
      ]
      & JsonSchema.describe "One author slot of a setting of kind authors. In plugins.json, name, email and phone are the defaults. In initialise.settings, they are the merged values for the job, and slots with no name are removed."
