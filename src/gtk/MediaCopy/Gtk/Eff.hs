{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Run GTK callbacks in 'Eff'.
--
-- haskell-gi calls a callback on a new Haskell thread each time, so the
-- unlift functions here use @ConcUnlift Ephemeral Unlimited@. Each callback
-- runs in a clone of the environment. State that GUI code changes must live
-- in an 'Data.IORef.IORef' or in a shared effect, never in a local one.
module MediaCopy.Gtk.Eff
  ( Gtk
  , runGtk
  , Lifted
  , Lower
  , Recover
  , onE
  , lowerE
  , idleE
  , timeoutE
  , toIO1
  , withStreak
  ) where

import Control.Exception (SomeException, catch, displayException, throwIO)
import Data.GI.Base (on)
import Data.GI.Base.BasicTypes (GObject)
import Data.GI.Base.Signals (SignalHandlerId, SignalInfo (..), SignalProxy)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Word (Word32)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception (catchSync, isSyncException)
import GI.GLib qualified as GLib
import System.IO (hPutStrLn, stderr)

data Gtk :: Effect

type instance DispatchOf Gtk = Static WithSideEffects

newtype instance StaticRep Gtk = GtkRep (IORef (SomeException -> IO ()))

strategy :: UnliftStrategy
strategy = ConcUnlift Ephemeral Unlimited

runGtk :: (IOE :> es) => (SomeException -> Eff (Gtk : es) ()) -> Eff (Gtk : es) a -> Eff es a
runGtk onFault action = withUnliftStrategy strategy $ do
  ref <- liftIO (newIORef (\err -> hPutStrLn stderr (displayException err)))
  evalStaticRep (GtkRep ref) $ do
    report <- toIO1 onFault
    liftIO (writeIORef ref report)
    action

data Unlift es = Unlift
  { run :: forall r. Eff es r -> IO r
  , fault :: SomeException -> IO ()
  }

unlift :: (Gtk :> es, IOE :> es) => Eff es (Unlift es)
unlift = do
  GtkRep ref <- getStaticRep
  withEffToIO strategy $ \run ->
    pure Unlift {run, fault = \err -> readIORef ref >>= \report -> report err}

type family Lifted (es :: [Effect]) (g :: Type) :: Type where
  Lifted es (IO r) = Eff es r
  Lifted es (a -> g) = a -> Lifted es g

class Lower g where
  lower :: Unlift es -> Lifted es g -> g

instance (Recover r) => Lower (IO r) where
  lower Unlift {run, fault} action = guardFault fault recover (run action)

instance (Lower g) => Lower (a -> g) where
  lower u f a = lower u (f a)

class Recover r where
  recover :: Maybe r

instance Recover () where
  recover = Just ()

instance {-# OVERLAPPABLE #-} Recover r where
  recover = Nothing

guardFault :: (SomeException -> IO ()) -> Maybe r -> IO r -> IO r
guardFault report fallback action =
  action `catch` \err ->
    if isSyncException err
      then do
        report err `catch` \(inner :: SomeException) -> hPutStrLn stderr (displayException inner)
        maybe (throwIO err) pure fallback
      else throwIO err

onE
  :: forall object info es
   . (Gtk :> es, IOE :> es, GObject object, SignalInfo info, Lower (HaskellCallbackType info))
  => object
  -> SignalProxy object info
  -> ((?self :: object) => Lifted es (HaskellCallbackType info))
  -> Eff es SignalHandlerId
onE object signal handler = do
  u <- unlift
  on object signal (lower @(HaskellCallbackType info) u handler)

lowerE :: forall g es. (Gtk :> es, IOE :> es, Lower g) => Lifted es g -> Eff es g
lowerE callback = unlift >>= \u -> pure (lower @g u callback)

idleE :: (Gtk :> es, IOE :> es) => Eff es () -> Eff es ()
idleE action = do
  Unlift {run, fault} <- unlift
  _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (guardFault fault (Just ()) (run action) >> pure False)
  pure ()

timeoutE :: (Gtk :> es, IOE :> es) => Word32 -> Eff es Bool -> Eff es Word32
timeoutE milliseconds action = do
  Unlift {run, fault} <- unlift
  GLib.timeoutAdd GLib.PRIORITY_DEFAULT milliseconds (guardFault fault (Just False) (run action))

toIO1 :: (Gtk :> es, IOE :> es) => (a -> Eff es b) -> Eff es (a -> IO b)
toIO1 f = unlift >>= \Unlift {run} -> pure (\a -> run (f a))

withStreak :: (IOE :> es) => IORef Bool -> (SomeException -> Eff es ()) -> (SomeException -> Eff es ()) -> Eff es () -> Eff es ()
withStreak failing first again action =
  (action >> liftIO (writeIORef failing False)) `catchSync` \err -> do
    already <- liftIO (atomicModifyIORef' failing (\old -> (True, old)))
    if already then again err else first err
