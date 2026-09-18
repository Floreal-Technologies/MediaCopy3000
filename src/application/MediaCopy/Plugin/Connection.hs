module MediaCopy.Plugin.Connection
  ( Connection
  , Host
  , Launch (..)
  , Limit (..)
  , CallFault (..)
  , with
  , call
  , decodeAnswer
  , stopSoftly
  ) where

import Control.Exception (displayException)
import Data.Aeson (FromJSON, ToJSON, Value, object, parseJSON, toJSON)
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (Async, asyncWithUnmask, cancelMany)
import Effectful.Concurrent.MVar (MVar, newMVar, withMVar)
import Effectful.Concurrent.STM
import Effectful.Exception qualified as E
import Effectful.Time (Time, monotonicTime)
import Effectful.Timeout (Timeout, timeout)
import Floreal.Commons.Environment (Environment, getEnvironment)
import Floreal.Commons.Process (Child, Detach (NewGroup), Output (..), Process, Reach (WholeGroup), Spawn (..), Stdio (Pipe), closeInput, exitCodeOf, kill, processId, release, spawn, terminate, writeInput)
import Floreal.Commons.Process.Lines (Line (..), lineReader)
import Floreal.Commons.Xdg (Xdg)
import MediaCopy.Plugin.JsonRpc qualified as JsonRpc
import MediaCopy.Plugin.JsonRpc.Error qualified as Error
import MediaCopy.Plugin.JsonRpc.Error qualified as JsonRpc (Error)
import MediaCopy.Plugin.JsonRpc.Incoming qualified as Incoming
import MediaCopy.Plugin.Protocol.LogLine qualified as LogLine
import MediaCopy.Plugin.Protocol.LogLine qualified as Protocol (LogLine)
import MediaCopy.Plugin.Protocol.Method qualified as Method
import System.Exit (ExitCode (..))

import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Plugin.Connection.Native qualified as Native
import MediaCopy.Plugin.Tracer qualified as Plugin (Tracer)
import MediaCopy.Plugin.Tracer qualified as Tracer

type Host es = (Process :> es, FileSystem :> es, Environment :> es, Xdg :> es, Time :> es, Timeout :> es, Concurrent :> es, IOE :> es)

data Launch es = Launch
  { executable :: OsPath
  , folder :: OsPath
  , onLog :: Text -> Eff es ()
  , trace :: Maybe Tracer.Target
  }

data Limit
  = Fixed Double
  | Silence Double

data CallFault
  = Ended Text
  | TimedOut Text
  | Refused Text
  | Malformed Text
  deriving stock (Eq, Show)

data Link es = Link
  { child :: Child
  , nextId :: TVar Int
  , pending :: TVar (Map Int (TMVar (Either JsonRpc.Error Value)))
  , lastHeard :: TVar Double
  , ended :: TVar (Maybe CallFault)
  , writeLock :: MVar ()
  , tracer :: Plugin.Tracer es
  }

data Connection es = Connection
  { link :: Link es
  , replyReader :: Async ()
  , errorReader :: Async ()
  }

with :: (Host es) => Launch es -> (Connection es -> Eff es a) -> Eff es a
with launch = E.bracket (open launch) hardStop

maxLine :: Int
maxLine = 16 * 1024 * 1024

open :: (Host es) => Launch es -> Eff es (Connection es)
open launch = do
  environment <- Native.childEnvironment <$> getEnvironment
  child <-
    spawn
      Spawn
        { program = launch.executable
        , arguments = []
        , folder = Just launch.folder
        , environment = Just environment
        , stdin = Pipe
        , stdout = Pipe
        , stderr = Pipe
        , detach = NewGroup
        , closeFds = False
        }
  connect launch child `E.onException` (kill WholeGroup child `E.finally` release child)

connect :: (Host es) => Launch es -> Child -> Eff es (Connection es)
connect launch child = do
  now <- monotonicTime
  pid <- maybe "unknown" T.show <$> processId child
  tracer <- maybe (pure Tracer.silent) (\target -> Tracer.open target pid launch.onLog) launch.trace
  link <-
    Link child
      <$> newTVarIO 1
      <*> newTVarIO Map.empty
      <*> newTVarIO now
      <*> newTVarIO Nothing
      <*> newMVar ()
      <*> pure tracer
  replies <- lineReader maxLine child StandardOutput
  complaints <- lineReader maxLine child StandardError
  Connection link
    <$> asyncWithUnmask (\unmask -> unmask (readReplies launch link replies))
    <*> asyncWithUnmask (\unmask -> unmask (readErrors launch link complaints))

readReplies :: (Process :> es, Time :> es, Concurrent :> es) => Launch es -> Link es -> Eff es Line -> Eff es ()
readReplies launch link next = loop
  where
    loop =
      next >>= \case
        Closed -> stopped
        TooLong -> tooLong link "standard output"
        Line line -> do
          link.tracer.write Tracer.In line
          monotonicTime >>= \now -> atomically (writeTVar link.lastHeard now)
          case Incoming.decode line of
            Left problem -> launch.onLog ("a line on standard output that is not JSON-RPC: " <> problem)
            Right (Incoming.Reply requestId result) -> atomically $ do
              boxes <- readTVar link.pending
              for_ (Map.lookup requestId boxes) (\box -> void (tryPutTMVar box result))
            Right (Incoming.Notify method params)
              | method == Method.progress -> pure ()
              | method == Method.log -> for_ (parseMaybe parseJSON params) (\(entry :: Protocol.LogLine) -> launch.onLog (entry.level <> ": " <> entry.message))
              | otherwise -> launch.onLog ("an unknown notification: " <> method)
          loop
    stopped = do
      code <- exitWithin 1 link.child
      endWith link . Ended $ case code of
        Just (ExitFailure n) -> "the plug-in stopped with exit code " <> T.show n
        Just ExitSuccess -> "the plug-in stopped"
        Nothing -> "the plug-in closed its standard output"

readErrors :: (Process :> es, Concurrent :> es) => Launch es -> Link es -> Eff es Line -> Eff es ()
readErrors launch link next =
  next >>= \case
    Closed -> pure ()
    TooLong -> tooLong link "standard error"
    Line line -> do
      link.tracer.write Tracer.ErrorOutput line
      launch.onLog ("stderr: " <> decodeUtf8Lenient line)
      readErrors launch link next

tooLong :: (Process :> es, Concurrent :> es) => Link es -> Text -> Eff es ()
tooLong link stream = do
  endWith link (Malformed ("the plug-in wrote a line longer than 16 MiB on its " <> stream))
  terminate WholeGroup link.child

endWith :: (Concurrent :> es) => Link es -> CallFault -> Eff es ()
endWith link fault = atomically (readTVar link.ended >>= maybe (writeTVar link.ended (Just fault)) (const (pure ())))

decodeAnswer :: (FromJSON r) => Text -> Value -> Either CallFault r
decodeAnswer method value =
  first (\problem -> Malformed ("the answer to " <> method <> " does not follow the protocol: " <> T.pack problem)) (parseEither parseJSON value)

call :: (Host es, ToJSON p, FromJSON r) => Connection es -> Limit -> Text -> p -> Eff es (Either CallFault r)
call conn limit method params =
  readTVarIO link.ended >>= \case
    Just fault -> pure (Left fault)
    Nothing -> do
      requestId <- atomically $ do
        n <- readTVar link.nextId
        writeTVar link.nextId (n + 1)
        pure n
      box <- newEmptyTMVarIO
      atomically (modifyTVar' link.pending (Map.insert requestId box))
      started <- monotonicTime
      atomically (writeTVar link.lastHeard started)
      sent <-
        timeout (micro limitSeconds) . E.try @IOException $
          withMVar link.writeLock $ \_ -> do
            let line = JsonRpc.encodeRequest requestId method (toJSON params)
            link.tracer.write Tracer.Out line
            writeInput link.child (line <> "\n")
      answer <- case sent of
        Nothing -> do
          let fault = TimedOut ("the plug-in did not read " <> method <> " for " <> seconds' limitSeconds)
          endWith link fault
          pure (Left fault)
        Just (Left e) -> pure (Left (Ended ("cannot write to the plug-in: " <> T.pack (displayException e))))
        Just (Right ()) -> wait started box
      atomically (modifyTVar' link.pending (Map.delete requestId))
      pure (answer >>= decodeAnswer method)
  where
    link = conn.link
    limitSeconds = case limit of
      Fixed seconds -> seconds
      Silence seconds -> seconds
    wait started box = do
      answer <-
        timeout 1_000_000 . atomically $
          (Right <$> readTMVar box) `orElse` (readTVar link.ended >>= maybe retry (pure . Left))
      case answer of
        Just (Right (Right value)) -> pure (Right value)
        Just (Right (Left refusal)) -> pure (Left (Refused (method <> " failed: " <> Prose.oneLine refusal.message)))
        Just (Left fault) -> pure (Left fault)
        Nothing -> do
          now <- monotonicTime
          heardAt <- readTVarIO link.lastHeard
          case limit of
            Fixed seconds
              | now - started > seconds -> pure (Left (TimedOut (method <> " timed out after " <> seconds' seconds)))
            Silence seconds
              | now - heardAt > seconds -> pure (Left (TimedOut ("no answer to " <> method <> " and no progress for " <> seconds' seconds)))
            _ -> wait started box

seconds' :: Double -> Text
seconds' seconds = T.show (round seconds :: Int) <> " s"

micro :: Double -> Int
micro seconds = round (seconds * 1_000_000)

exitWithin :: (Process :> es, Time :> es, Concurrent :> es) => Double -> Child -> Eff es (Maybe ExitCode)
exitWithin seconds child = monotonicTime >>= \start -> poll (start + seconds)
  where
    poll deadline =
      exitCodeOf child >>= \case
        Just code -> pure (Just code)
        Nothing -> do
          now <- monotonicTime
          if now >= deadline then pure Nothing else threadDelay 20_000 >> poll deadline

stopSoftly :: (Host es) => Connection es -> Eff es ()
stopSoftly conn = do
  void (call @_ @Value @Value conn (Fixed 2) Method.shutdown (object []))
  void (timeout 2_000_000 (E.try @IOException (closeInput conn.link.child)))
  void (exitWithin 2 conn.link.child)

hardStop :: (Process :> es, Time :> es, Timeout :> es, Concurrent :> es) => Connection es -> Eff es ()
hardStop conn = stopGroup conn.link.child `E.finally` disconnect conn

stopGroup :: (Process :> es, Time :> es, Timeout :> es, Concurrent :> es) => Child -> Eff es ()
stopGroup child = do
  E.uninterruptibleMask_ $ do
    running <- isNothing <$> exitCodeOf child
    when running $ do
      terminate WholeGroup child
      void (exitWithin 2 child)
    kill WholeGroup child
    void (exitWithin 1 child)
  void (timeout 1_000_000 (E.try @IOException (closeInput child)))

disconnect :: (Process :> es, Concurrent :> es) => Connection es -> Eff es ()
disconnect conn = do
  release conn.link.child
  E.uninterruptibleMask_ (cancelMany [conn.replyReader, conn.errorReader])
  conn.link.tracer.close
