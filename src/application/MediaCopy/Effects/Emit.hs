module MediaCopy.Effects.Emit
  ( Emit
  , event
  , runWith
  , runCollect
  ) where

import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (modify, runState)

import MediaCopy.Domain.JobEvent (JobEvent)

data Emit :: Effect where
  Emit :: JobEvent -> Emit m ()

type instance DispatchOf Emit = Dynamic

event :: (Emit :> es) => JobEvent -> Eff es ()
event ev = send (Emit ev)

runWith :: (JobEvent -> Eff es ()) -> Eff (Emit : es) a -> Eff es a
runWith sink = interpret_ $ \case
  Emit ev -> sink ev

runCollect :: Eff (Emit : es) a -> Eff es (a, Vector JobEvent)
runCollect action = do
  (a, evs) <- reinterpret_ (runState []) (\case Emit ev -> modify (ev :)) action
  pure (a, V.fromList (reverse evs))
