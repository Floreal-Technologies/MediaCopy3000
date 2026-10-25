module MediaCopy.Plugin.JsonSchema
  ( -- * Codecs
    namedObject
  , describedObject
  , optionalText
  , nonEmptyText

    -- * Schema patches
  , Patch
  , ref
  , defsPrefix
  , describe
  , extend
  , require
  , property
  , extendProperty
  ) where

import Autodocodec (Codec (..), ObjectCodec, ValueCodec, dimapCodec, named, optionalFieldOrNull', (<?>))
import Control.Monad (mfilter)
import Data.Aeson (Key, Object, Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

-- | An object with a name in @$defs@. 'Autodocodec.object' writes its name as a comment, and the schema would show it as a description.
namedObject :: Text -> ObjectCodec input output -> ValueCodec input output
namedObject name = named name . ObjectOfCodec Nothing

-- | 'namedObject' with a description.
describedObject :: Text -> Text -> ObjectCodec input output -> ValueCodec input output
describedObject name description = named name . (<?> description) . ObjectOfCodec Nothing

-- | An optional text. An absent key, @null@ and the empty text decode as 'Nothing'.
optionalText :: Text -> ObjectCodec (Maybe Text) (Maybe Text)
optionalText key = dimapCodec (>>= nonEmptyText) id (optionalFieldOrNull' key)

nonEmptyText :: Text -> Maybe Text
nonEmptyText = mfilter (not . T.null) . Just

-- | A change to one schema in @$defs@, for what autodocodec-schema cannot say.
type Patch = Value -> Value

ref :: Text -> Value
ref name = object ["$ref" .= (defsPrefix <> name)]

defsPrefix :: Text
defsPrefix = "#/$defs/"

describe :: Text -> Patch
describe text = extend [("description", String text)]

-- | Adds keywords to a schema.
extend :: [(Key, Value)] -> Patch
extend pairs = \case
  Object o -> Object (KeyMap.union (KeyMap.fromList pairs) o)
  other -> error ("cannot add " <> show (map fst pairs) <> " to a schema that is not an object: " <> show other)

-- | Adds a key to the required keys of an object schema.
require :: Text -> Patch
require name = \case
  Object o -> Object (KeyMap.insert "required" (Array (V.snoc (required o) (String name))) o)
  other -> error ("cannot require " <> show name <> " in a schema that is not an object: " <> show other)
  where
    required o = case KeyMap.lookup "required" o of
      Just (Array names) -> names
      _ -> V.empty

-- | Replaces the schema of one property of an object schema.
property :: Text -> Value -> Patch
property name value = adjustProperties (KeyMap.insert (Key.fromText name) value)

-- | Adds keywords to the schema of one property of an object schema.
extendProperty :: Text -> [(Key, Value)] -> Patch
extendProperty name pairs = adjustProperties $ \properties -> case KeyMap.lookup (Key.fromText name) properties of
  Just value -> KeyMap.insert (Key.fromText name) (extend pairs value) properties
  Nothing -> error ("the schema has no property " <> show name)

adjustProperties :: (Object -> Object) -> Patch
adjustProperties f = \case
  Object o -> Object (KeyMap.insert "properties" (Object (f (fromMaybe mempty (KeyMap.lookup "properties" o >>= asObject)))) o)
  other -> error ("cannot change the properties of a schema that is not an object: " <> show other)
  where
    asObject = \case
      Object properties -> Just properties
      _ -> Nothing
