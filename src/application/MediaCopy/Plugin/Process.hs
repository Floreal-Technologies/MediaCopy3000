module MediaCopy.Plugin.Process
  ( Launch (..)
  , Limit (..)
  , CallFault (..)
  , Connection
  , withConnection
  , call
  , stopSoftly
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, displayException, try, uninterruptibleMask_)
import Control.Monad (forM_, void, when)
import Data.Aeson (FromJSON, ToJSON, Value, object, parseJSON, toJSON)
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.Clock (getMonotonicTime)
import MediaCopy.Plugin.JsonRpc
import MediaCopy.Plugin.Protocol (LogLine (..), Progress, methodShutdown, notifyLog, notifyProgress)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.OsPath (OsPath, decodeFS)
import System.Process
import System.Timeout (timeout)

import MediaCopy.Domain.Plugin (oneLine)
import MediaCopy.Plugin.Process.Native (Group, childEnvironment, groupOf, killGroup, terminateGroup)
import MediaCopy.Plugin.Trace (Direction (..), TraceTarget, Tracer (..), openTracer, silentTracer)

data Launch = Launch
  { executable :: OsPath
  , folder :: OsPath
  , onLog :: Text -> IO ()
  , onProgress :: Progress -> IO ()
  , trace :: Maybe TraceTarget
  }

data Limit
  = Fixed Double
  | Silence Double

data CallFault
  = Unreachable Text
  | Malformed Text
  deriving stock (Eq, Show)

data Connection = Connection
  { process :: ProcessHandle
  , group :: Group
  , input :: Handle
  , nextId :: IORef Int
  , pending :: TVar (Map Int (TMVar (Either RpcError Value)))
  , lastHeard :: TVar Double
  , ended :: TVar (Maybe Text)
  , writeLock :: MVar ()
  , tracer :: Tracer
  }

withConnection :: Launch -> (Connection -> IO a) -> IO a
withConnection launch = bracket (open launch) hardStop

maxLine :: Int
maxLine = 16 * 1024 * 1024

data Line = Line ByteString | TooLong | Closed

open :: Launch -> IO Connection
open launch = do
  exe <- decodeFS launch.executable
  dir <- decodeFS launch.folder
  environment <- childEnvironment <$> getEnvironment
  (input, output, errors, process) <-
    createProcess (proc exe []) {cwd = Just dir, env = Just environment, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True, use_process_jobs = True} >>= \case
      (Just i, Just o, Just e, p) -> pure (i, o, e, p)
      (_, _, _, p) -> terminateProcess p >> ioError (userError "the plug-in started with no pipes")
  forM_ [input, output, errors] (\h -> hSetBinaryMode h True)
  now <- getMonotonicTime
  group <- groupOf process
  pid <- maybe "unknown" (T.show . toInteger) <$> getPid process
  tracer <- maybe (pure silentTracer) (\target -> openTracer target pid launch.onLog) launch.trace
  conn <-
    Connection process group input
      <$> newIORef 1
      <*> newTVarIO Map.empty
      <*> newTVarIO now
      <*> newTVarIO Nothing
      <*> newMVar ()
      <*> pure tracer
  replies <- lineReader output
  complaints <- lineReader errors
  void (asyncWithUnmask (\unmask -> unmask (readReplies launch conn replies)))
  void (asyncWithUnmask (\unmask -> unmask (readErrors launch conn complaints)))
  pure conn

lineReader :: Handle -> IO (IO Line)
lineReader h = do
  carry <- newIORef BS.empty
  finished <- newIORef False
  let next =
        readIORef finished >>= \case
          True -> pure Closed
          False -> readIORef carry >>= \rest -> scan [rest] (BS.length rest) rest
      scan chunks size latest = case BS8.elemIndex '\n' latest of
        Just i
          | size - BS.length latest + i > maxLine -> pure TooLong
          | otherwise -> do
              writeIORef carry (BS.drop (i + 1) latest)
              pure (Line (BS.concat (reverse (BS.take i latest : drop 1 chunks))))
        Nothing
          | size > maxLine -> pure TooLong
          | otherwise ->
              try @IOException (BS.hGetSome h 65_536) >>= \case
                Right chunk | not (BS.null chunk) -> scan (chunk : chunks) (size + BS.length chunk) chunk
                _ -> do
                  writeIORef finished True
                  writeIORef carry BS.empty
                  let rest = BS.concat (reverse chunks)
                  pure (if BS.null rest then Closed else Line rest)
  pure next

readReplies :: Launch -> Connection -> IO Line -> IO ()
readReplies launch conn next = loop
  where
    loop =
      next >>= \case
        Closed -> stopped
        TooLong -> tooLong conn "standard output"
        Line line -> do
          conn.tracer.write In line
          getMonotonicTime >>= \now -> atomically (writeTVar conn.lastHeard now)
          case decodeIncoming line of
            Left problem -> launch.onLog ("a line on standard output that is not JSON-RPC: " <> problem)
            Right (Reply requestId result) -> atomically $ do
              boxes <- readTVar conn.pending
              for_ (Map.lookup requestId boxes) (\box -> void (tryPutTMVar box result))
            Right (Notify method params)
              | method == notifyProgress -> for_ (parseMaybe parseJSON params) launch.onProgress
              | method == notifyLog -> for_ (parseMaybe parseJSON params) (\(entry :: LogLine) -> launch.onLog (entry.level <> ": " <> entry.message))
              | otherwise -> launch.onLog ("an unknown notification: " <> method)
          loop
    stopped = do
      code <- exitWithin 1 conn.process
      endWith conn $ case code of
        Just (ExitFailure n) -> "the plug-in stopped with exit code " <> T.show n
        Just ExitSuccess -> "the plug-in stopped"
        Nothing -> "the plug-in closed its standard output"

readErrors :: Launch -> Connection -> IO Line -> IO ()
readErrors launch conn next =
  next >>= \case
    Closed -> pure ()
    TooLong -> tooLong conn "standard error"
    Line line -> conn.tracer.write ErrorOutput line >> launch.onLog ("stderr: " <> decodeUtf8Lenient line) >> readErrors launch conn next

tooLong :: Connection -> Text -> IO ()
tooLong conn stream = do
  endWith conn ("the plug-in wrote a line longer than 16 MiB on its " <> stream)
  terminateGroup conn.process conn.group

endWith :: Connection -> Text -> IO ()
endWith conn reason = atomically (readTVar conn.ended >>= maybe (writeTVar conn.ended (Just reason)) (const (pure ())))

call :: (ToJSON p, FromJSON r) => Connection -> Limit -> Text -> p -> IO (Either CallFault r)
call conn limit method params =
  readTVarIO conn.ended >>= \case
    Just reason -> pure (Left (Unreachable reason))
    Nothing -> do
      requestId <- atomicModifyIORef' conn.nextId (\n -> (n + 1, n))
      box <- newEmptyTMVarIO
      atomically (modifyTVar' conn.pending (Map.insert requestId box))
      started <- getMonotonicTime
      atomically (writeTVar conn.lastHeard started)
      sent <-
        timeout (micro limitSeconds) . try @IOException $
          withMVar conn.writeLock $ \_ -> do
            let line = encodeRequest requestId method (toJSON params)
            conn.tracer.write Out line
            BS.hPut conn.input (line <> "\n")
            hFlush conn.input
      answer <- case sent of
        Nothing -> do
          let reason = "the plug-in did not read " <> method <> " for " <> seconds' limitSeconds
          endWith conn reason
          pure (Left reason)
        Just (Left e) -> pure (Left ("cannot write to the plug-in: " <> T.pack (displayException e)))
        Just (Right ()) -> wait started box
      atomically (modifyTVar' conn.pending (Map.delete requestId))
      pure (first Unreachable answer >>= decoded)
  where
    limitSeconds = case limit of
      Fixed seconds -> seconds
      Silence seconds -> seconds
    decoded value = case parseEither parseJSON value of
      Left problem -> Left (Malformed ("the answer to " <> method <> " does not follow the protocol: " <> T.pack problem))
      Right result -> Right result
    wait started box = do
      answer <-
        timeout 1_000_000 . atomically $
          (Right <$> readTMVar box) `orElse` (readTVar conn.ended >>= maybe retry (pure . Left))
      case answer of
        Just (Right (Right value)) -> pure (Right value)
        Just (Right (Left refusal)) -> pure (Left (method <> " failed: " <> oneLine refusal.message))
        Just (Left reason) -> pure (Left reason)
        Nothing -> do
          now <- getMonotonicTime
          heardAt <- readTVarIO conn.lastHeard
          case limit of
            Fixed seconds
              | now - started > seconds -> pure (Left (method <> " timed out after " <> seconds' seconds))
            Silence seconds
              | now - heardAt > seconds -> pure (Left ("no answer to " <> method <> " and no progress for " <> seconds' seconds))
            _ -> wait started box

seconds' :: Double -> Text
seconds' seconds = T.show (round seconds :: Int) <> " s"

micro :: Double -> Int
micro seconds = round (seconds * 1_000_000)

exitWithin :: Double -> ProcessHandle -> IO (Maybe ExitCode)
exitWithin seconds process = getMonotonicTime >>= \start -> poll (start + seconds)
  where
    poll deadline =
      getProcessExitCode process >>= \case
        Just code -> pure (Just code)
        Nothing -> do
          now <- getMonotonicTime
          if now >= deadline then pure Nothing else threadDelay 20_000 >> poll deadline

stopSoftly :: Connection -> IO ()
stopSoftly conn = do
  void (call @Value @Value conn (Fixed 2) methodShutdown (object []))
  void (timeout 2_000_000 (try @IOException (hClose conn.input)))
  void (exitWithin 2 conn.process)

hardStop :: Connection -> IO ()
hardStop conn = do
  uninterruptibleMask_ $ do
    running <- isNothing <$> getProcessExitCode conn.process
    when running $ do
      terminateGroup conn.process conn.group
      void (exitWithin 2 conn.process)
    killGroup conn.process conn.group
    void (exitWithin 1 conn.process)
  void (timeout 1_000_000 (try @IOException (hClose conn.input)))
  conn.tracer.close
