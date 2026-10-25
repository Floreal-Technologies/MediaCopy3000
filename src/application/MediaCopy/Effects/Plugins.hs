module MediaCopy.Effects.Plugins
  ( Plugins (..)
  , fileVerified
  , settle
  , runNone
  , runSession
  ) where

import Control.Exception (displayException)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, send)
import Effectful.Exception (trySync)

import MediaCopy.Domain.JobEnd (JobEnd)
import MediaCopy.Domain.VerifiedFile (VerifiedFile)
import MediaCopy.Plugin.Connection qualified as Connection
import MediaCopy.Plugin.Session qualified as Plugin (Session)
import MediaCopy.Plugin.Session qualified as Session

data Plugins :: Effect where
  FileVerified :: VerifiedFile -> Plugins m ()
  Settle :: JobEnd -> Plugins m ()

type instance DispatchOf Plugins = Dynamic

fileVerified :: (Plugins :> es) => VerifiedFile -> Eff es ()
fileVerified file = send (FileVerified file)

settle :: (Plugins :> es) => JobEnd -> Eff es ()
settle jobEnd = send (Settle jobEnd)

runNone :: Eff (Plugins : es) a -> Eff es a
runNone = interpret_ $ \case
  FileVerified _ -> pure ()
  Settle _ -> pure ()

runSession :: (Connection.Host es) => Plugin.Session es -> Eff (Plugins : es) a -> Eff es a
runSession session = interpret_ $ \case
  FileVerified file -> Session.inspect session file
  Settle jobEnd ->
    trySync (Session.finish session jobEnd)
      >>= either (\e -> e & displayException & T.pack & Session.logFault session) pure
