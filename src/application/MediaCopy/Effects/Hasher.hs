module MediaCopy.Effects.Hasher
  ( Hasher
  , hashing
  , hashBytes
  , runHasher
  ) where

import Ascmhl.Hash
import Control.Monad.Primitive (touch)
import Crypto.Hash.MD5 qualified as MD5
import Crypto.Hash.SHA1 qualified as SHA1
import Crypto.Hash.SHA512 qualified as SHA512
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BU
import Data.Digest.XXHash.FFI.C (c_xxh64_digest, c_xxh64_reset, c_xxh64_update_safe)
import Data.Functor ((<&>))
import Data.IORef
import Data.Primitive.ByteArray (MutableByteArray (MutableByteArray), newAlignedPinnedByteArray)
import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Foreign.C.Types (CULLong (..))

data HasherState = HasherState
  { feedH :: ByteString -> IO ()
  , finishH :: IO Hash
  }

data Hasher :: Effect

type instance DispatchOf Hasher = Static WithSideEffects

data instance StaticRep Hasher = HasherRep

runHasher :: (IOE :> es) => Eff (Hasher : es) a -> Eff es a
runHasher = evalStaticRep HasherRep

hashing :: (Hasher :> es) => HashAlgo -> ((ByteString -> Eff es ()) -> Eff es a) -> Eff es (Hash, a)
hashing fmt use =
  getStaticRep >>= \HasherRep -> do
    st <- unsafeEff_ (stateOf fmt)
    a <- use (unsafeEff_ . st.feedH)
    h <- unsafeEff_ st.finishH
    pure (h, a)

hashBytes :: (Hasher :> es) => HashAlgo -> ByteString -> Eff es Hash
hashBytes fmt bytes = fst <$> hashing fmt (\put -> put bytes)

xxh64StateBytes :: Int
xxh64StateBytes = 128

stateOf :: HashAlgo -> IO HasherState
stateOf algo = case algo of
  XXH64 -> do
    mba@(MutableByteArray st) <- newAlignedPinnedByteArray xxh64StateBytes 8
    c_xxh64_reset st 0
    pure
      HasherState
        { feedH = \bs -> do
            BU.unsafeUseAsCStringLen bs (\(buf, len) -> c_xxh64_update_safe st buf (fromIntegral len))
            touch mba
        , finishH = do
            CULLong w <- c_xxh64_digest st
            touch mba
            pure (Hash XXH64 (word64ToHex w))
        }
  MD5 -> newIORef MD5.init <&> \ref -> ctxHasher ref MD5.update (Hash MD5 . toHex . MD5.finalize)
  SHA1 -> newIORef SHA1.init <&> \ref -> ctxHasher ref SHA1.update (Hash SHA1 . toHex . SHA1.finalize)
  C4 -> newIORef SHA512.init <&> \ref -> ctxHasher ref SHA512.update (Hash C4 . c4FromSha512 . SHA512.finalize)

ctxHasher :: IORef ctx -> (ctx -> ByteString -> ctx) -> (ctx -> Hash) -> HasherState
ctxHasher ref update done =
  HasherState
    { feedH = \bs -> modifyIORef' ref (\ctx -> update ctx bs)
    , finishH = readIORef ref <&> \ctx -> done ctx
    }
