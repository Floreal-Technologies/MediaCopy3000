module MediaCopy.Plugin.Field
  ( Field (..)
  , Kind (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Function ((&))
import Data.List (List)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Kind = TextField | SecretField | EmailField | BoolField | ChoiceField (Vector Text) | PathField | AuthorsField
  deriving stock (Eq, Show)

data Field = Field
  { key :: Text
  , label :: Text
  , kind :: Kind
  , required :: Bool
  , defaultValue :: Maybe Value
  }
  deriving stock (Eq, Show)

kindName :: Kind -> Text
kindName = \case
  TextField -> "text"
  SecretField -> "secret"
  EmailField -> "email"
  BoolField -> "bool"
  ChoiceField _ -> "choice"
  PathField -> "path"
  AuthorsField -> "authors"

fieldKinds :: List (Text, Object -> Parser Kind)
fieldKinds =
  [ (kindName TextField, \_ -> pure TextField)
  , (kindName SecretField, \_ -> pure SecretField)
  , (kindName BoolField, \_ -> pure BoolField)
  , (kindName (ChoiceField V.empty), \o -> ChoiceField <$> o .: "options")
  , (kindName PathField, \_ -> pure PathField)
  , (kindName AuthorsField, \_ -> pure AuthorsField)
  ]

instance FromJSON Field where
  parseJSON = withObject "field" $ \o -> do
    key <- o .: "key"
    label <- o .: "label"
    name <- o .: "kind"
    kind <- case lookup name fieldKinds of
      Just parse -> parse o
      Nothing -> fail ("unknown field kind " <> show name)
    required <- fromMaybe False <$> o .:? "required"
    defaultValue <- o .:? "default"
    pure Field {key, label, kind, required, defaultValue}

instance Plugin.JsonSchema Field where
  defName = Just "field"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= (["key", "label", "kind"] :: [Text])
      , "properties"
          .= object
            [ "key" .= JsonSchema.string
            , "label" .= JsonSchema.string
            , "kind" .= object ["enum" .= map fst fieldKinds]
            , "options" .= JsonSchema.embed @(Vector Text)
            , "required" .= object ["type" .= String "boolean", "default" .= False]
            , "default" .= object []
            ]
      , "allOf"
          .= [ object ["if" .= kindIs "choice", "then" .= object ["required" .= ["options" :: Text]]]
             , object ["if" .= kindIs "authors", "then" .= object ["not" .= object ["required" .= ["default" :: Text]]]]
             ]
      ]
      & JsonSchema.describe "A setting or a job field. The kind authors is allowed only in settings, only once, only with manifest.write, and without default. Its stored value and its value in initialise are arrays of authorSlot."
    where
      kindIs :: Text -> Value
      kindIs name = object ["properties" .= object ["kind" .= object ["const" .= name]]]
