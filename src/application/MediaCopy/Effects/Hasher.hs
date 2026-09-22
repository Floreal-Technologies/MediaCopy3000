module MediaCopy.Effects.Hasher
  ( Hasher
  , HasherState
  , feed
  , finish
  , withHasher
  , hashBytes
  , runHasher
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
import Data.Functor ((<&>))
import Data.IORef
import Data.Primitive.ByteArray (MutableByteArray (MutableByteArray), newAlignedPinnedByteArray)
import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Foreign.C.Types (CULLong (..))

import MediaCopy.Domain.JobFormat (JobFormat, formatAlgo)

data HasherState = HasherState
  { feedH :: ByteString -> IO ()
  , finishH :: IO Hash
  , spent :: IORef Bool
  }

data Hasher :: Effect

type instance DispatchOf Hasher = Static WithSideEffects

data instance StaticRep Hasher = HasherRep

runHasher :: (IOE :> es) => Eff (Hasher : es) a -> Eff es a
runHasher = evalStaticRep HasherRep

feed :: (Hasher :> es) => HasherState -> ByteString -> Eff es ()
feed st bs = getStaticRep >>= \HasherRep -> unsafeEff_ (unspent "feed" st >> st.feedH bs)

finish :: (Hasher :> es) => HasherState -> Eff es Hash
finish st = getStaticRep >>= \HasherRep -> unsafeEff_ (unspent "finish" st >> writeIORef st.spent True >> st.finishH)

withHasher :: (Hasher :> es) => JobFormat -> (HasherState -> Eff es a) -> Eff es a
withHasher fmt use = getStaticRep >>= \HasherRep -> unsafeEff_ (stateOf (formatAlgo fmt)) >>= use

hashBytes :: (Hasher :> es) => JobFormat -> ByteString -> Eff es Hash
hashBytes fmt bytes = withHasher fmt (\hasher -> feed hasher bytes >> finish hasher)

unspent :: String -> HasherState -> IO ()
unspent what st = do
  done <- readIORef st.spent
  when done (ioError (userError ("MediaCopy.Effects.Hasher: " <> what <> " after finish")))

xxh64StateBytes :: Int
xxh64StateBytes = 128

stateOf :: HashAlgo -> IO HasherState
stateOf algo =
  newIORef False >>= \spent -> case algo of
    XXH64 -> do
      mba@(MutableByteArray st) <- newAlignedPinnedByteArray xxh64StateBytes 8
      c_xxh64_reset st 0
      pure
        HasherState
          { spent
          , feedH = \bs -> do
              BU.unsafeUseAsCStringLen bs (\(buf, len) -> c_xxh64_update_safe st buf (fromIntegral len))
              touch mba
          , finishH = do
              CULLong w <- c_xxh64_digest st
              touch mba
              pure (Hash XXH64 (word64ToHex w))
          }
    MD5 -> newIORef MD5.init <&> \ref -> ctxHasher spent ref MD5.update (\ctx -> Hash MD5 (toHex (MD5.finalize ctx)))
    SHA1 -> newIORef SHA1.init <&> \ref -> ctxHasher spent ref SHA1.update (\ctx -> Hash SHA1 (toHex (SHA1.finalize ctx)))
    C4 -> newIORef SHA512.init <&> \ref -> ctxHasher spent ref SHA512.update (\ctx -> Hash C4 (c4FromSha512 (SHA512.finalize ctx)))

ctxHasher :: IORef Bool -> IORef ctx -> (ctx -> ByteString -> ctx) -> (ctx -> Hash) -> HasherState
ctxHasher spent ref update done =
  HasherState
    { spent
    , feedH = \bs -> modifyIORef' ref (\ctx -> update ctx bs)
    , finishH = readIORef ref <&> \ctx -> done ctx
    }
