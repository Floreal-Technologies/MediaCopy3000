{-# LANGUAGE TypeFamilies #-}

module MediaCopy.Effects.Console
  ( Stream (..)
  , Console (..)
  , writeText
  , writeLine
  , useUtf8
  , runIO
  ) where

import Data.Text.IO qualified as Text
import Effectful (Dispatch (Dynamic), DispatchOf)
import Effectful.Dispatch.Dynamic (interpret_, send)
import System.IO (Handle, hSetEncoding, stderr, stdout, utf8)

data Stream = Stdout | Stderr
  deriving stock (Eq, Show)

data Console :: Effect where
  WriteText :: Stream -> Text -> Console m ()
  UseUtf8 :: Console m ()

type instance DispatchOf Console = Dynamic

writeText :: (Console :> es) => Stream -> Text -> Eff es ()
writeText stream text = send (WriteText stream text)

writeLine :: (Console :> es) => Stream -> Text -> Eff es ()
writeLine stream text = writeText stream (text <> "\n")

useUtf8 :: (Console :> es) => Eff es ()
useUtf8 = send UseUtf8

streamHandle :: Stream -> Handle
streamHandle = \case
  Stdout -> stdout
  Stderr -> stderr

runIO :: (IOE :> es) => Eff (Console : es) a -> Eff es a
runIO = interpret_ $ \case
  WriteText stream text -> liftIO (Text.hPutStr (streamHandle stream) text)
  UseUtf8 -> liftIO (hSetEncoding stdout utf8 >> hSetEncoding stderr utf8)
