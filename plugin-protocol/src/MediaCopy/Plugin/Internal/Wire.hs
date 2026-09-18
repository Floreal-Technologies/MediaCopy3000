{-# LANGUAGE UndecidableInstances #-}

module MediaCopy.Plugin.Internal.Wire (Wire (..)) where

import Data.Aeson
import GHC.Generics (Generic, Rep)

newtype Wire a = Wire a

wireOptions :: Options
wireOptions = defaultOptions {omitNothingFields = True}

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Wire a) where
  toJSON (Wire a) = genericToJSON wireOptions a
  toEncoding (Wire a) = genericToEncoding wireOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Wire a) where
  parseJSON value = Wire <$> genericParseJSON wireOptions value
