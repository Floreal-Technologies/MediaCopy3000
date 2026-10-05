module MediaCopy.Plugin.Secrets.Native
  ( store
  , lookup
  , remove
  , exists
  ) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Word (Word32)
import Foreign.C.String (CWString, withCWString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff)
import Prelude hiding (lookup)
import System.Win32.Types (BOOL, getLastError)

#include <windows.h>
#include <wincred.h>

data Credential

foreign import ccall unsafe "CredWriteW"
  credWrite :: Ptr Credential -> Word32 -> IO BOOL

foreign import ccall unsafe "CredReadW"
  credRead :: CWString -> Word32 -> Word32 -> Ptr (Ptr Credential) -> IO BOOL

foreign import ccall unsafe "CredDeleteW"
  credDelete :: CWString -> Word32 -> Word32 -> IO BOOL

foreign import ccall unsafe "CredFree"
  credFree :: Ptr Credential -> IO ()

genericType, persistLocalMachine, errorNotFound :: Word32
genericType = #{const CRED_TYPE_GENERIC}
persistLocalMachine = #{const CRED_PERSIST_LOCAL_MACHINE}
errorNotFound = #{const ERROR_NOT_FOUND}

target :: Text -> Text -> String
target plugin key = T.unpack ("MediaCopy3000/" <> plugin <> "/" <> key)

failed :: String -> IO a
failed what = getLastError >>= \code -> ioError (userError (what <> " failed with error " <> show code))

store :: Text -> Text -> Text -> IO ()
store plugin key value =
  withCWString (target plugin key) $ \name ->
    unsafeUseAsCStringLen (encodeUtf8 value) $ \(bytes, len) ->
      allocaBytes #{size CREDENTIALW} $ \cred -> do
        fillBytes cred 0 #{size CREDENTIALW}
        #{poke CREDENTIALW, Type} cred genericType
        #{poke CREDENTIALW, TargetName} cred name
        #{poke CREDENTIALW, CredentialBlobSize} cred (fromIntegral len :: Word32)
        #{poke CREDENTIALW, CredentialBlob} cred bytes
        #{poke CREDENTIALW, Persist} cred persistLocalMachine
        ok <- credWrite cred 0
        unless ok (failed "CredWriteW")

lookup :: Text -> Text -> IO (Maybe Text)
lookup plugin key =
  withCWString (target plugin key) $ \name ->
    alloca $ \found -> do
      ok <- credRead name genericType 0 found
      if ok
        then do
          cred <- peek found
          flip finally (credFree cred) $ do
            size <- #{peek CREDENTIALW, CredentialBlobSize} cred :: IO Word32
            blob <- #{peek CREDENTIALW, CredentialBlob} cred :: IO (Ptr ())
            bytes <- BS.packCStringLen (castPtr blob, fromIntegral size)
            pure (Just (decodeUtf8Lenient bytes))
        else do
          code <- getLastError
          if code == errorNotFound then pure Nothing else failed "CredReadW"

remove :: Text -> Text -> IO ()
remove plugin key =
  withCWString (target plugin key) $ \name -> do
    ok <- credDelete name genericType 0
    unless ok $ do
      code <- getLastError
      unless (code == errorNotFound) (failed "CredDeleteW")

exists :: Text -> Text -> IO Bool
exists plugin key =
  withCWString (target plugin key) $ \name ->
    alloca $ \found -> do
      ok <- credRead name genericType 0 found
      if ok
        then peek found >>= credFree >> pure True
        else do
          code <- getLastError
          if code == errorNotFound then pure False else failed "CredReadW"
