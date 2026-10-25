{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module MediaCopy.Plugin.Internal.Wire
  ( Wire (..)
  , Tagged (..)
  , constructorTag
  , statusField
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

newtype Wire a = Wire a

wireOptions :: Options
wireOptions = defaultOptions {omitNothingFields = True}

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Wire a) where
  toJSON (Wire a) = genericToJSON wireOptions a
  toEncoding (Wire a) = genericToEncoding wireOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Wire a) where
  parseJSON value = Wire <$> genericParseJSON wireOptions value

-- | A sum type whose constructors have no argument or one record. The JSON is an object: the constructor name in @status@, next to the fields of the record.
newtype Tagged a = Tagged a

constructorTag :: String -> String
constructorTag = \case
  c : cs -> toLower c : cs
  [] -> []

statusField :: Text
statusField = "status"

tagOf :: forall name. (KnownSymbol name) => Text
tagOf = T.pack (constructorTag (symbolVal (Proxy @name)))

instance (Generic a, GTaggedTo (Rep a)) => ToJSON (Tagged a) where
  toJSON (Tagged a) = Object (gtaggedTo (from a))

instance (Generic a, GTaggedFrom (Rep a)) => FromJSON (Tagged a) where
  parseJSON = withObject "tagged object" $ \o -> do
    tag <- o .: Key.fromText statusField
    case lookup tag (gtaggedFrom o) of
      Just parser -> Tagged . to <$> parser
      Nothing -> fail ("unknown " <> T.unpack statusField <> " " <> show tag)

class GTaggedTo (f :: Type -> Type) where
  gtaggedTo :: f p -> Object

instance (GTaggedTo f) => GTaggedTo (M1 D d f) where
  gtaggedTo (M1 x) = gtaggedTo x

instance (GTaggedTo f, GTaggedTo g) => GTaggedTo (f :+: g) where
  gtaggedTo = \case
    L1 x -> gtaggedTo x
    R1 x -> gtaggedTo x

instance (KnownSymbol name) => GTaggedTo (M1 C ('MetaCons name fx s) U1) where
  gtaggedTo _ = KeyMap.singleton (Key.fromText statusField) (String (tagOf @name))

instance (KnownSymbol name, ToJSON t) => GTaggedTo (M1 C ('MetaCons name fx s) (M1 S sel (K1 i t))) where
  gtaggedTo (M1 (M1 (K1 payload))) = case toJSON payload of
    Object o -> KeyMap.insert (Key.fromText statusField) (String (tagOf @name)) o
    other -> error ("the argument of " <> symbolVal (Proxy @name) <> " is not a JSON object: " <> show other)

class GTaggedFrom (f :: Type -> Type) where
  gtaggedFrom :: Object -> [(Text, Parser (f p))]

instance (GTaggedFrom f) => GTaggedFrom (M1 D d f) where
  gtaggedFrom o = [(tag, M1 <$> parser) | (tag, parser) <- gtaggedFrom o]

instance (GTaggedFrom f, GTaggedFrom g) => GTaggedFrom (f :+: g) where
  gtaggedFrom o = [(tag, L1 <$> parser) | (tag, parser) <- gtaggedFrom o] <> [(tag, R1 <$> parser) | (tag, parser) <- gtaggedFrom o]

instance (KnownSymbol name) => GTaggedFrom (M1 C ('MetaCons name fx s) U1) where
  gtaggedFrom _ = [(tagOf @name, pure (M1 U1))]

instance (KnownSymbol name, FromJSON t) => GTaggedFrom (M1 C ('MetaCons name fx s) (M1 S sel (K1 i t))) where
  gtaggedFrom o = [(tagOf @name, M1 . M1 . K1 <$> parseJSON (Object o))]
