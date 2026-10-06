{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module MediaCopy.Plugin.JsonSchema
  ( -- * Class
    JsonSchema (..)
  , embed
  , ref
  , defsPrefix

    -- * Building blocks
  , record
  , enumOf
  , string
  , describe
  , property
  , describeProperty

    -- * Output
  , render
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prettyprinter (Doc, braces, brackets, comma, defaultLayoutOptions, hardline, layoutPretty, nest, pretty, punctuate, vsep)
import Prettyprinter.Render.Text (renderLazy)

-- | A type that has a JSON Schema. A type with a name goes into @$defs@, and the other schemas refer to it.
class JsonSchema a where
  schema :: Value
  defName :: Maybe Text
  defName = Nothing

-- | The schema of @a@ as another schema uses it: a reference when it has a name, the schema itself otherwise.
embed :: forall a. (JsonSchema a) => Value
embed = maybe (schema @a) ref (defName @a)

ref :: Text -> Value
ref name = object ["$ref" .= (defsPrefix <> name)]

defsPrefix :: Text
defsPrefix = "#/$defs/"

instance JsonSchema Text where
  schema = string

instance JsonSchema Bool where
  schema = typed "boolean"

instance JsonSchema Int where
  schema = typed "integer"

instance JsonSchema Int64 where
  schema = typed "integer"

instance JsonSchema Double where
  schema = typed "number"

instance JsonSchema Value where
  schema = object []

instance (JsonSchema a) => JsonSchema (Vector a) where
  schema = object ["type" .= String "array", "items" .= embed @a]

instance (JsonSchema a) => JsonSchema (Map Text a) where
  schema = object ["type" .= String "object", "additionalProperties" .= embed @a]

string :: Value
string = typed "string"

typed :: Text -> Value
typed name = object ["type" .= name]

-- | The schema of a record whose JSON instance is generic with @omitNothingFields@: a 'Maybe' field is optional, every other field is required.
record :: forall a. (GProperties (Rep a)) => Value
record =
  object $
    ["type" .= String "object"]
      <> ["required" .= required | not (null required)]
      <> ["properties" .= object [Key.fromText name .= value | (name, _, value) <- fields]]
  where
    fields = gproperties @(Rep a)
    required = [name | (name, True, _) <- fields]

class GProperties (f :: Type -> Type) where
  gproperties :: [(Text, Bool, Value)]

instance (GProperties f) => GProperties (M1 D d f) where
  gproperties = gproperties @f

instance (GProperties f) => GProperties (M1 C c f) where
  gproperties = gproperties @f

instance (GProperties f, GProperties g) => GProperties (f :*: g) where
  gproperties = gproperties @f <> gproperties @g

instance (KnownSymbol name, FieldSchema t) => GProperties (M1 S ('MetaSel ('Just name) su ss ds) (K1 i t)) where
  gproperties = [(T.pack (symbolVal (Proxy @name)), required, value)]
    where
      (required, value) = fieldSchema @t

class FieldSchema t where
  fieldSchema :: (Bool, Value)

instance {-# OVERLAPPING #-} (JsonSchema a) => FieldSchema (Maybe a) where
  fieldSchema = (False, embed @a)

instance (JsonSchema a) => FieldSchema a where
  fieldSchema = (True, embed @a)

enumOf :: forall a. (Bounded a, Enum a, ToJSON a) => Value
enumOf = object ["enum" .= map toJSON [minBound .. maxBound :: a]]

describe :: Text -> Value -> Value
describe text = insert "description" (String text)

-- | Replaces the schema of one property of an object schema.
property :: Text -> Value -> Value -> Value
property name value = \case
  Object o -> Object (KeyMap.insert "properties" (insert (Key.fromText name) value (fromMaybe (object []) (KeyMap.lookup "properties" o))) o)
  other -> error ("cannot add the property " <> show name <> " to a schema that is not an object: " <> show other)

describeProperty :: Text -> Text -> Value -> Value
describeProperty name text = adjust "properties" (adjust (Key.fromText name) (describe text))

insert :: Key -> Value -> Value -> Value
insert key value = \case
  Object o -> Object (KeyMap.insert key value o)
  other -> error ("cannot add " <> show key <> " to a schema that is not an object: " <> show other)

adjust :: Key -> (Value -> Value) -> Value -> Value
adjust key f = \case
  Object o | Just value <- KeyMap.lookup key o -> Object (KeyMap.insert key (f value) o)
  other -> error ("the schema has no " <> show key <> ": " <> show other)

render :: Value -> ByteString
render document = encodeUtf8 (renderLazy (layoutPretty defaultLayoutOptions (go Schema document <> hardline)))
  where
    go :: Context -> Value -> Doc ()
    go context = \case
      Object o
        | KeyMap.null o -> "{}"
        | otherwise -> block braces [json (Key.toText key) <> ": " <> go (inside context key) value | (key, value) <- sortOn (rank context . fst) (KeyMap.toList o)]
      Array items
        | null items -> "[]"
        | otherwise -> block brackets (map (go context) (toList items))
      scalar -> json scalar
    block enclose items = enclose (nest 2 (hardline <> vsep (punctuate comma items)) <> hardline)
    json :: (ToJSON a) => a -> Doc ()
    json = pretty . decodeUtf8 . encode
    rank context key = case context of
      Schema -> (fromMaybe (length keywords) (elemIndex key keywords), key)
      Names -> (0, key)
    inside context key = case context of
      Names -> Schema
      Schema -> if key `elem` ["properties", "$defs", "x-methods", "x-notifications"] then Names else Schema

data Context = Schema | Names

keywords :: [Key]
keywords =
  [ "$schema"
  , "$id"
  , "title"
  , "description"
  , "$ref"
  , "x-methods"
  , "x-notifications"
  , "$defs"
  , "params"
  , "result"
  , "type"
  , "const"
  , "enum"
  , "pattern"
  , "minimum"
  , "maximum"
  , "required"
  , "properties"
  , "items"
  , "additionalProperties"
  , "contains"
  , "minContains"
  , "maxContains"
  , "not"
  , "allOf"
  , "if"
  , "then"
  , "default"
  ]
