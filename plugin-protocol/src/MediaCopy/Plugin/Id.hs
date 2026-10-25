module MediaCopy.Plugin.Id
  ( Id (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text (Text)

newtype Id = Id Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via Autodocodec Id

instance HasCodec Id where
  codec =
    named "pluginId" $
      dimapCodec Id (\(Id raw) -> raw) textCodec
        <?> "A reverse domain name in lowercase, for example tech.floreal.c2pa-reader. It holds at least one '.', it does not start or end with '.', and it has no '..'. It is also the name of the folder of the plug-in."
