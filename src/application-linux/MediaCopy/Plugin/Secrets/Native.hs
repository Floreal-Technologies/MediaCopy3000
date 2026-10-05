module MediaCopy.Plugin.Secrets.Native
  ( store
  , lookup
  , remove
  , exists
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GI.Gio qualified as Gio
import GI.Secret qualified as Secret
import Prelude hiding (lookup)

attributes :: Text -> Text -> Map Text Text
attributes plugin key = Map.fromList [("application", "mediacopy3000"), ("plugin", plugin), ("key", key)]

noCancel :: Maybe Gio.Cancellable
noCancel = Nothing

store :: Text -> Text -> Text -> IO ()
store plugin key value =
  Secret.passwordStoreSync Nothing (attributes plugin key) Nothing ("MediaCopy 3000 – " <> plugin <> " – " <> key) value noCancel

lookup :: Text -> Text -> IO (Maybe Text)
lookup plugin key = Secret.passwordLookupSync Nothing (attributes plugin key) noCancel

remove :: Text -> Text -> IO ()
remove plugin key = Secret.passwordClearSync Nothing (attributes plugin key) noCancel

exists :: Text -> Text -> IO Bool
exists plugin key = not . null <$> Secret.passwordSearchSync Nothing (attributes plugin key) [] noCancel
