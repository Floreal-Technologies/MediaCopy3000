module MediaCopy.Effects.Hasher
  ( Hasher
  , hashing
  , hashBytes
  , run
  ) where

import Ascmhl.Algo (Algo)
import Ascmhl.Algo qualified as Algo
import Ascmhl.C4 qualified as C4
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.Hex qualified as Hex
import Control.Monad.Primitive (touch)
import Crypto.Hash.MD5 qualified as MD5
import Crypto.Hash.SHA1 qualified as SHA1
import Crypto.Hash.SHA512 qualified as SHA512
import Data.ByteString.Unsafe qualified as BU
import Data.Digest.XXHash.FFI.C (c_xxh64_digest, c_xxh64_reset, c_xxh64_update_safe)
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

run :: (IOE :> es) => Eff (Hasher : es) a -> Eff es a
run = evalStaticRep HasherRep

hashing :: (Hasher :> es) => Algo -> ((ByteString -> Eff es ()) -> Eff es a) -> Eff es (Hash, a)
hashing fmt use =
  getStaticRep >>= \HasherRep -> do
    st <- unsafeEff_ (stateOf fmt)
    a <- use (unsafeEff_ . st.feedH)
    h <- unsafeEff_ st.finishH
    pure (h, a)

hashBytes :: (Hasher :> es) => Algo -> ByteString -> Eff es Hash
hashBytes fmt bytes = fst <$> hashing fmt (\put -> put bytes)

xxh64StateBytes :: Int
xxh64StateBytes = 128

stateOf :: Algo -> IO HasherState
stateOf algo = case algo of
  Algo.XXH64 -> do
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
            pure (Hash.Hash Algo.XXH64 (Hex.fromWord64 w))
        }
  Algo.MD5 -> newIORef MD5.init <&> \ref -> ctxHasher ref MD5.update (\ctx -> ctx & MD5.finalize & Hex.fromBytes & Hash.Hash Algo.MD5)
  Algo.SHA1 -> newIORef SHA1.init <&> \ref -> ctxHasher ref SHA1.update (\ctx -> ctx & SHA1.finalize & Hex.fromBytes & Hash.Hash Algo.SHA1)
  Algo.C4 -> newIORef SHA512.init <&> \ref -> ctxHasher ref SHA512.update (\ctx -> ctx & SHA512.finalize & C4.fromSha512 & Hash.Hash Algo.C4)

ctxHasher :: IORef ctx -> (ctx -> ByteString -> ctx) -> (ctx -> Hash) -> HasherState
ctxHasher ref update done =
  HasherState
    { feedH = \bs -> modifyIORef' ref (\ctx -> update ctx bs)
    , finishH = readIORef ref <&> \ctx -> done ctx
    }
