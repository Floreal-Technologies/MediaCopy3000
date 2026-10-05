module MediaCopy.Plugin.Secrets.Native
  ( store
  , lookup
  , remove
  , exists
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Foreign.C.Types (CLong (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Prelude hiding (lookup)

type CFTypeRef = Ptr ()

foreign import ccall unsafe "CFStringCreateWithBytes"
  cfStringCreateWithBytes :: Ptr () -> Ptr CUChar -> CLong -> CUInt -> CUChar -> IO CFTypeRef

foreign import ccall unsafe "CFDataCreate"
  cfDataCreate :: Ptr () -> Ptr CUChar -> CLong -> IO CFTypeRef

foreign import ccall unsafe "CFDataGetLength"
  cfDataGetLength :: CFTypeRef -> IO CLong

foreign import ccall unsafe "CFDataGetBytePtr"
  cfDataGetBytePtr :: CFTypeRef -> IO (Ptr CUChar)

foreign import ccall unsafe "CFDictionaryCreateMutable"
  cfDictionaryCreateMutable :: Ptr () -> CLong -> Ptr () -> Ptr () -> IO CFTypeRef

foreign import ccall unsafe "CFDictionarySetValue"
  cfDictionarySetValue :: CFTypeRef -> CFTypeRef -> CFTypeRef -> IO ()

foreign import ccall unsafe "CFRelease"
  cfRelease :: CFTypeRef -> IO ()

foreign import ccall unsafe "&kCFTypeDictionaryKeyCallBacks"
  keyCallBacks :: Ptr ()

foreign import ccall unsafe "&kCFTypeDictionaryValueCallBacks"
  valueCallBacks :: Ptr ()

foreign import ccall unsafe "&kCFBooleanTrue"
  kCFBooleanTrue :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecClass"
  kSecClass :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecClassGenericPassword"
  kSecClassGenericPassword :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecAttrService"
  kSecAttrService :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecAttrAccount"
  kSecAttrAccount :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecValueData"
  kSecValueData :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecReturnData"
  kSecReturnData :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecReturnAttributes"
  kSecReturnAttributes :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecMatchLimit"
  kSecMatchLimit :: Ptr CFTypeRef

foreign import ccall unsafe "&kSecMatchLimitOne"
  kSecMatchLimitOne :: Ptr CFTypeRef

foreign import ccall safe "SecItemAdd"
  secItemAdd :: CFTypeRef -> Ptr CFTypeRef -> IO Int32

foreign import ccall safe "SecItemUpdate"
  secItemUpdate :: CFTypeRef -> CFTypeRef -> IO Int32

foreign import ccall safe "SecItemCopyMatching"
  secItemCopyMatching :: CFTypeRef -> Ptr CFTypeRef -> IO Int32

foreign import ccall safe "SecItemDelete"
  secItemDelete :: CFTypeRef -> IO Int32

errSecSuccess, errSecItemNotFound :: Int32
errSecSuccess = 0
errSecItemNotFound = -25300

utf8Encoding :: CUInt
utf8Encoding = 0x08000100

withCF :: IO CFTypeRef -> (CFTypeRef -> IO a) -> IO a
withCF create = bracket create (\ref -> unless (ref == nullPtr) (cfRelease ref))

withString :: Text -> (CFTypeRef -> IO a) -> IO a
withString text use =
  unsafeUseAsCStringLen (encodeUtf8 text) $ \(bytes, len) ->
    withCF (cfStringCreateWithBytes nullPtr (castPtr bytes) (fromIntegral len) utf8Encoding 0) use

withData :: Text -> (CFTypeRef -> IO a) -> IO a
withData text use =
  unsafeUseAsCStringLen (encodeUtf8 text) $ \(bytes, len) ->
    withCF (cfDataCreate nullPtr (castPtr bytes) (fromIntegral len)) use

withQuery :: Text -> Text -> (CFTypeRef -> IO a) -> IO a
withQuery plugin key use =
  withString "MediaCopy 3000" $ \service ->
    withString (plugin <> "/" <> key) $ \account ->
      withCF (cfDictionaryCreateMutable nullPtr 0 keyCallBacks valueCallBacks) $ \query -> do
        set query kSecClass =<< peek kSecClassGenericPassword
        set query kSecAttrService service
        set query kSecAttrAccount account
        use query
  where
    set dict keyRef value = peek keyRef >>= \k -> cfDictionarySetValue dict k value

check :: Text -> Int32 -> IO ()
check what status = when (status /= errSecSuccess) (ioError (userError (T.unpack what <> " failed with OSStatus " <> show status)))

store :: Text -> Text -> Text -> IO ()
store plugin key value =
  withData value $ \secret ->
    withQuery plugin key $ \query ->
      withCF (cfDictionaryCreateMutable nullPtr 0 keyCallBacks valueCallBacks) $ \update -> do
        peek kSecValueData >>= \k -> cfDictionarySetValue update k secret
        updated <- secItemUpdate query update
        if updated == errSecItemNotFound
          then do
            peek kSecValueData >>= \k -> cfDictionarySetValue query k secret
            check "SecItemAdd" =<< secItemAdd query nullPtr
          else check "SecItemUpdate" updated

lookup :: Text -> Text -> IO (Maybe Text)
lookup plugin key =
  withQuery plugin key $ \query -> do
    true <- peek kCFBooleanTrue
    peek kSecReturnData >>= \k -> cfDictionarySetValue query k true
    one <- peek kSecMatchLimitOne
    peek kSecMatchLimit >>= \k -> cfDictionarySetValue query k one
    alloca $ \result -> do
      poke result nullPtr
      status <- secItemCopyMatching query result
      if status == errSecItemNotFound
        then pure Nothing
        else do
          check "SecItemCopyMatching" status
          withCF (peek result) $ \found -> do
            len <- cfDataGetLength found
            bytes <- cfDataGetBytePtr found
            Just . decodeUtf8Lenient <$> BS.packCStringLen (castPtr bytes, fromIntegral len)

remove :: Text -> Text -> IO ()
remove plugin key =
  withQuery plugin key $ \query -> do
    status <- secItemDelete query
    unless (status == errSecItemNotFound) (check "SecItemDelete" status)

exists :: Text -> Text -> IO Bool
exists plugin key =
  withQuery plugin key $ \query -> do
    true <- peek kCFBooleanTrue
    peek kSecReturnAttributes >>= \k -> cfDictionarySetValue query k true
    one <- peek kSecMatchLimitOne
    peek kSecMatchLimit >>= \k -> cfDictionarySetValue query k one
    alloca $ \result -> do
      poke result nullPtr
      status <- secItemCopyMatching query result
      if status == errSecItemNotFound
        then pure False
        else do
          check "SecItemCopyMatching" status
          withCF (peek result) (\_ -> pure True)
