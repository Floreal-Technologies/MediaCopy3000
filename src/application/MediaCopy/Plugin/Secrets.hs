module MediaCopy.Plugin.Secrets
  ( storeSecret
  , readSecret
  , deleteSecret
  , secretExists
  ) where

import Control.Exception (displayException)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T

import MediaCopy.Guard (guarded)
import MediaCopy.Plugin.Secrets.Native qualified as Native

storeSecret :: Text -> Text -> Text -> IO (Either Text ())
storeSecret plugin key value = keyring (Native.store plugin key value)

readSecret :: Text -> Text -> IO (Either Text (Maybe Text))
readSecret plugin key = keyring (Native.lookup plugin key)

deleteSecret :: Text -> Text -> IO (Either Text ())
deleteSecret plugin key = keyring (Native.remove plugin key)

secretExists :: Text -> Text -> IO (Either Text Bool)
secretExists plugin key = keyring (Native.exists plugin key)

keyring :: IO a -> IO (Either Text a)
keyring action = first (\e -> "the keyring refused: " <> T.pack (displayException e)) <$> guarded action
