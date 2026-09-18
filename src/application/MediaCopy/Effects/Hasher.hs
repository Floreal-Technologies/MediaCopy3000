-- | The hasher effect, and the mutable state each algorithm keeps while it runs.
module MediaCopy.Effects.Hasher
  ( Hasher
  , HasherState
  , feed
  , finish
  , withHasher
  , hashBytes
  , runHasherIO
  ) where

import Ascmhl.Hash
import Control.Monad (when)
import Control.Monad.Primitive (touch)
import Crypto.Hash.MD5 qualified as MD5
import Crypto.Hash.SHA1 qualified as SHA1
import Crypto.Hash.SHA512 qualified as SHA512
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BU
import Data.Digest.XXHash.FFI.C (c_xxh64_digest, c_xxh64_reset, c_xxh64_update_safe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.Primitive.ByteArray (MutableByteArray (MutableByteArray), newAlignedPinnedByteArray)
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, send)
import Foreign.C.Types (CULLong (..))

import MediaCopy.Domain.JobFormat (JobFormat, formatAlgo)

-- | The state closes over its own algorithm, so 'stateOf' is the one place that chooses by
-- 'HashAlgo' and a new algorithm cannot be half-added. 'finish' spends the state: a later feed, or
-- a second finish, is a named failure and never a quietly wrong hash.
data HasherState = HasherState
  { feedH :: ByteString -> IO ()
  , finishH :: IO Hash
  , spent :: IORef Bool
  }

data Hasher :: Effect where
  NewHasher :: JobFormat -> Hasher m HasherState
  Feed :: HasherState -> ByteString -> Hasher m ()
  Finish :: HasherState -> Hasher m Hash

type instance DispatchOf Hasher = Dynamic

feed :: (Hasher :> es) => HasherState -> ByteString -> Eff es ()
feed st bs = send (Feed st bs)

finish :: (Hasher :> es) => HasherState -> Eff es Hash
finish st = send (Finish st)

-- | One hasher of the format, for the length of the action. The state needs no release: it owns
-- nothing but its own memory.
withHasher :: (Hasher :> es) => JobFormat -> (HasherState -> Eff es a) -> Eff es a
withHasher fmt use = send (NewHasher fmt) >>= use

-- | One fresh hasher of the format, fed once.
hashBytes :: (Hasher :> es) => JobFormat -> ByteString -> Eff es Hash
hashBytes fmt bytes = withHasher fmt (\hasher -> feed hasher bytes >> finish hasher)

runHasherIO :: (IOE :> es) => Eff (Hasher : es) a -> Eff es a
runHasherIO action =
  action
    & interpret_
      ( \case
          NewHasher fmt -> liftIO (newHasherState (formatAlgo fmt))
          Feed st bs -> liftIO (unspent "feed" st >> st.feedH bs)
          Finish st -> liftIO (unspent "finish" st >> writeIORef st.spent True >> st.finishH)
      )

-- | This is the only gate on a spent state, so no caller reads one hash twice.
unspent :: String -> HasherState -> IO ()
unspent what st = do
  done <- readIORef st.spent
  when done (ioError (userError ("MediaCopy.Effects.Hasher: " <> what <> " after finish")))

xxh64StateBytes :: Int
xxh64StateBytes = 128

-- | Allocates fresh mutable state for the algorithm, unspent.
newHasherState :: HashAlgo -> IO HasherState
newHasherState algo = newIORef False >>= \flag -> stateOf flag algo

stateOf :: IORef Bool -> HashAlgo -> IO HasherState
stateOf spent = \case
  XXH64 -> do
    mba@(MutableByteArray st) <- newAlignedPinnedByteArray xxh64StateBytes 8
    c_xxh64_reset st 0
    pure
      HasherState
        { spent
        , feedH = \bs -> do
            BU.unsafeUseAsCStringLen bs (\(buf, len) -> c_xxh64_update_safe st buf (fromIntegral len))
            -- Keeps the pinned state array alive during the safe call.
            touch mba
        , finishH = do
            CULLong w <- c_xxh64_digest st
            touch mba
            pure (Hash XXH64 (word64ToHex w))
        }
  MD5 -> newIORef MD5.init <&> \ref -> ctxHasher spent ref MD5.update (\ctx -> Hash MD5 (toHex (MD5.finalize ctx)))
  SHA1 -> newIORef SHA1.init <&> \ref -> ctxHasher spent ref SHA1.update (\ctx -> Hash SHA1 (toHex (SHA1.finalize ctx)))
  C4 -> newIORef SHA512.init <&> \ref -> ctxHasher spent ref SHA512.update (\ctx -> Hash C4 (c4FromSha512 (SHA512.finalize ctx)))

-- | The shape the cryptohash-* packages share: an immutable context carried in an 'IORef'.
ctxHasher :: IORef Bool -> IORef ctx -> (ctx -> ByteString -> ctx) -> (ctx -> Hash) -> HasherState
ctxHasher spent ref update done =
  HasherState
    { spent
    , feedH = \bs -> modifyIORef' ref (\ctx -> update ctx bs)
    , finishH = readIORef ref <&> \ctx -> done ctx
    }
