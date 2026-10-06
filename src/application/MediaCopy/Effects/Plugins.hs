module MediaCopy.Effects.Plugins
  ( Plugins
  , fileVerified
  , settle
  , runPluginsNone
  , runPluginsSession
  ) where

import Control.Exception (displayException)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, send)

import MediaCopy.Domain.Plugin (VerifiedFile)
import MediaCopy.Guard (guarded)
import MediaCopy.Plugin.Session (Session)
import MediaCopy.Plugin.Session qualified as Session

data Plugins :: Effect where
  FileVerified :: VerifiedFile -> Plugins m ()
  Settle :: Plugins m ()

type instance DispatchOf Plugins = Dynamic

fileVerified :: (Plugins :> es) => VerifiedFile -> Eff es ()
fileVerified file = send (FileVerified file)

settle :: (Plugins :> es) => Eff es ()
settle = send Settle

runPluginsNone :: Eff (Plugins : es) a -> Eff es a
runPluginsNone = interpret_ $ \case
  FileVerified _ -> pure ()
  Settle -> pure ()

runPluginsSession :: (IOE :> es) => Session -> Eff (Plugins : es) a -> Eff es a
runPluginsSession session = interpret_ $ \case
  FileVerified file -> liftIO (Session.fileVerified session file)
  Settle ->
    liftIO $
      guarded (Session.finishInspections session)
        >>= either (Session.logFault session . T.pack . displayException) pure
