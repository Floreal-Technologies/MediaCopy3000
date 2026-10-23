{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Test.FakeWorld
  ( World (..)
  , FakeAnswer (..)
  , FakePlugin (..)
  , runEnvironmentFake
  , runXdgFake
  , runProcessFake
  , fakeRunner
  ) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text.Encoding (encodeUtf8)
import Effectful (runEff)
import Effectful.Concurrent (ThreadId, forkIO, killThread)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Log (runNoLog)
import Effectful.Time (runTime)
import Effectful.Timeout (runTimeout)
import Floreal.Commons.Environment (Environment (..))
import Floreal.Commons.Process (Child (..), Output (..), Process (..), Spawn (..))
import Floreal.Commons.Xdg (Xdg (..), XdgDirectory (..))
import MediaCopy.Plugin.JsonRpc qualified as JsonRpc
import MediaCopy.Plugin.JsonRpc.Error qualified as Error
import MediaCopy.Plugin.JsonRpc.Request qualified as Request
import MediaCopy.Plugin.Protocol.LogLine qualified as LogLine
import MediaCopy.Plugin.Protocol.Method qualified as Method
import System.Exit (ExitCode (..))
import System.OsPath ((</>))
import splice System.OsPath (osp)

import MediaCopy.Effects.Console (Console)
import MediaCopy.Effects.Console qualified as Console
import MediaCopy.Effects.Hasher qualified as Hasher
import MediaCopy.Runtime (Runner (..))
import MediaCopy.Test.InMemoryFS (MemFS)
import MediaCopy.Test.InMemoryFS qualified as InMemoryFS

data World = World
  { disk :: IORef MemFS
  , plugins :: Map OsPath FakePlugin
  }

data FakeAnswer
  = Answer Value
  | Refuse Text
  | Crash Int
  | Logging Text FakeAnswer

newtype FakePlugin = FakePlugin (Text -> Value -> FakeAnswer)

runEnvironmentFake :: Eff (Environment : es) a -> Eff es a
runEnvironmentFake = interpret_ $ \case
  LookupEnv _ -> pure Nothing
  GetEnvironment -> pure []
  GetArgs -> pure []
  GetExecutablePath -> pure [osp|/opt/mediacopy3000/bin/mediacopy3000|]
  GetHomeDirectory -> pure [osp|/home/operator|]
  GetHostName -> pure "localhost"

runXdgFake :: Eff (Xdg : es) a -> Eff es a
runXdgFake = interpret_ $ \case
  GetXdgDirectory kind program -> pure (xdgRoot kind </> program)
  GetXdgDirectoryList _ -> pure []

xdgRoot :: XdgDirectory -> OsPath
xdgRoot = \case
  XdgData -> [osp|/xdg/data|]
  XdgConfig -> [osp|/xdg/config|]
  XdgState -> [osp|/xdg/state|]
  XdgCache -> [osp|/xdg/cache|]

data Fake = Fake
  { input :: TQueue (Maybe ByteString)
  , inputClosed :: TVar Bool
  , output :: TQueue ByteString
  , errors :: TQueue ByteString
  , exited :: TMVar ExitCode
  , released :: TVar Bool
  , thread :: ThreadId
  }

runProcessFake :: (IOE :> es, Concurrent :> es) => World -> Eff (Process : es) a -> Eff es a
runProcessFake world action = do
  table <- newTVarIO Map.empty
  counter <- newTVarIO 0
  interpret_ (fakeProcess world table counter) action

fakeProcess :: (IOE :> es, Concurrent :> es) => World -> TVar (Map Int Fake) -> TVar Int -> Process (Eff localEs) a -> Eff es a
fakeProcess world table counter = \case
  SpawnChild spawned -> case Map.lookup spawned.program world.plugins of
    Nothing -> liftIO (ioError (userError "no such executable"))
    Just plugin -> do
      input <- newTQueueIO
      inputClosed <- newTVarIO False
      output <- newTQueueIO
      errors <- newTQueueIO
      exited <- newEmptyTMVarIO
      released <- newTVarIO False
      thread <- forkIO (serve plugin input output errors exited)
      atomically $ do
        n <- readTVar counter
        writeTVar counter (n + 1)
        modifyTVar' table (Map.insert n Fake {input, inputClosed, output, errors, exited, released, thread})
        pure (Child n)
  ProcessId (Child n) -> known (Child n) $> Just (10_000 + n)
  WriteInput child bytes ->
    known child >>= \fake ->
      atomically ((||) <$> readTVar fake.inputClosed <*> (not <$> isEmptyTMVar fake.exited)) >>= \case
        True -> liftIO (ioError (userError "broken pipe"))
        False -> atomically (writeTQueue fake.input (Just bytes))
  CloseInput child ->
    known child >>= \fake -> atomically $ do
      writeTVar fake.inputClosed True
      writeTQueue fake.input Nothing
  ReadChunk child stream size -> known child >>= \fake -> atomically (takeChunk stream size fake)
  ExitCodeOf child -> known child >>= \fake -> atomically (tryReadTMVar fake.exited)
  WaitExit child -> known child >>= \fake -> atomically (readTMVar fake.exited)
  Terminate _ child -> known child >>= stopped 15
  Kill _ child -> known child >>= stopped 9
  Release (Child n) ->
    atomically
      ( readTVar table >>= \entries -> case Map.lookup n entries of
          Nothing -> pure Nothing
          Just fake -> do
            writeTVar table (Map.delete n entries)
            writeTVar fake.released True
            pure (Just fake)
      )
      >>= \case
        Nothing -> liftIO (ioError (userError "unknown child"))
        Just fake -> void (forkIO (killThread fake.thread))
  where
    known (Child n) = readTVarIO table >>= maybe (liftIO (ioError (userError "unknown child"))) pure . Map.lookup n

stopped :: (Concurrent :> es) => Int -> Fake -> Eff es ()
stopped code fake = do
  ended <- atomically (tryPutTMVar fake.exited (ExitFailure code))
  when ended (void (forkIO (killThread fake.thread)))

takeChunk :: Output -> Int -> Fake -> STM ByteString
takeChunk stream size fake =
  tryReadTQueue queue >>= \case
    Just chunk
      | BS.length chunk <= size -> pure chunk
      | otherwise -> unGetTQueue queue (BS.drop size chunk) $> BS.take size chunk
    Nothing -> finished >>= \done -> if done then pure BS.empty else retry
  where
    queue = case stream of
      StandardOutput -> fake.output
      StandardError -> fake.errors
    finished = (||) <$> readTVar fake.released <*> (not <$> isEmptyTMVar fake.exited)

serve :: (Concurrent :> es) => FakePlugin -> TQueue (Maybe ByteString) -> TQueue ByteString -> TQueue ByteString -> TMVar ExitCode -> Eff es ()
serve (FakePlugin behaviour) input output errors exited = loop BS.empty
  where
    loop carry = case BS8.elemIndex '\n' carry of
      Just i -> handle (BS.take i carry) >>= \going -> when going (loop (BS.drop (i + 1) carry))
      Nothing ->
        atomically (readTQueue input) >>= \case
          Nothing -> exitWith ExitSuccess
          Just bytes -> loop (carry <> bytes)
    handle line = case Request.decode line of
      Left problem -> do
        atomically (writeTQueue errors ("fake plug-in: a request it cannot decode: " <> encodeUtf8 problem <> "\n"))
        exitWith (ExitFailure 70) $> False
      Right request -> answer request (behaviour request.method request.params)
    answer request = \case
      Answer value -> reply request (\requestId -> JsonRpc.encodeReply requestId value)
      Refuse problem -> reply request (\requestId -> JsonRpc.encodeFailure requestId Error.Error {code = -32000, message = problem})
      Crash code -> exitWith (ExitFailure code) $> False
      Logging message next -> do
        write (LBS.toStrict (encode (object ["jsonrpc" .= ("2.0" :: Text), "method" .= Method.log, "params" .= LogLine.LogLine {level = "info", message}])))
        answer request next
    reply request encoded = do
      forM_ request.requestId (\requestId -> write (encoded requestId))
      pure True
    write line = atomically (writeTQueue output (line <> "\n"))
    exitWith code = void (atomically (tryPutTMVar exited code))

fakeRunner :: World -> Runner
fakeRunner world =
  Runner
    ( \action ->
        action
          & InMemoryFS.runFileSystemMem world.disk
          & Hasher.run
          & runProcessFake world
          & runEnvironmentFake
          & runXdgFake
          & runConsoleQuiet
          & runNoLog
          & runTime
          & runTimeout
          & runConcurrent
          & runEff
    )

runConsoleQuiet :: Eff (Console : es) a -> Eff es a
runConsoleQuiet = interpret_ $ \case
  Console.WriteText _ _ -> pure ()
  Console.UseUtf8 -> pure ()
