module MediaCopy.Plugin.Field
  ( Field (..)
  , Kind (..)
  , kindName
  , fieldKinds
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.List (List)
import Data.Text (Text)
import Data.Vector (Vector)

import MediaCopy.Plugin.JsonSchema qualified as JsonSchema

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
  deriving (FromJSON, ToJSON) via Autodocodec Field

kindName :: Kind -> Text
kindName = \case
  TextField -> "text"
  SecretField -> "secret"
  EmailField -> "email"
  BoolField -> "bool"
  ChoiceField _ -> "choice"
  PathField -> "path"
  AuthorsField -> "authors"

-- | The kinds that a plug-in can declare. A choice also needs options.
fieldKinds :: List Kind
fieldKinds = [TextField, SecretField, EmailField, BoolField, ChoiceField mempty, PathField, AuthorsField]

instance HasCodec Field where
  codec =
    JsonSchema.describedObject "field" "A setting or a job field. The kind authors is allowed only in settings, only once, only with manifest.write, and without default. Its stored value and its value in initialise are arrays of authorSlot." $
      Field
        <$> requiredField' "key" .= (.key)
        <*> requiredField' "label" .= (.label)
        <*> kindCodec .= (.kind)
        <*> optionalFieldWithOmittedDefault' "required" False .= (.required)
        <*> optionalFieldOrNull' "default" .= (.defaultValue)

-- | The kind, and the options of a choice, which sit next to it in the object.
kindCodec :: ObjectCodec Kind Kind
kindCodec =
  bimapCodec toKind fromKind $
    (,)
      <$> requiredField' "kind" .= fst
      <*> optionalField' "options" .= snd
  where
    toKind = \case
      ("choice", Just options) -> Right (ChoiceField options)
      ("choice", Nothing) -> Left "a field of kind choice needs options"
      (name, _) -> maybe (Left ("unknown field kind " <> show name)) Right (lookup name [(kindName kind, kind) | kind <- fieldKinds])
    fromKind kind =
      ( kindName kind
      , case kind of
          ChoiceField options -> Just options
          _ -> Nothing
      )
