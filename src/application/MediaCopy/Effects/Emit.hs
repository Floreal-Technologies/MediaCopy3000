module MediaCopy.Effects.Emit
  ( Emit
  , emit
  , runEmitIO
  , runEmitCollect
  ) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (modify, runState)

import MediaCopy.Domain.Job (JobEvent)

data Emit :: Effect where
  Emit :: JobEvent -> Emit m ()

type instance DispatchOf Emit = Dynamic

emit :: (Emit :> es) => JobEvent -> Eff es ()
emit ev = send (Emit ev)

runEmitIO :: (IOE :> es) => (JobEvent -> IO ()) -> Eff (Emit : es) a -> Eff es a
runEmitIO sink = interpret_ $ \case
  Emit ev -> liftIO (sink ev)

-- | Collects emitted events in order, for tests.
runEmitCollect :: Eff (Emit : es) a -> Eff es (a, Vector JobEvent)
runEmitCollect action = do
  (a, evs) <- reinterpret_ (runState []) (\case Emit ev -> modify (\xs -> ev : xs)) action
  pure (a, V.fromList (reverse evs))
