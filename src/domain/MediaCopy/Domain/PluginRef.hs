module MediaCopy.Domain.PluginRef
  ( PluginRef (..)
  ) where

data PluginRef = PluginRef
  { id :: Text
  , name :: Text
  }
  deriving stock (Eq, Ord, Show)
