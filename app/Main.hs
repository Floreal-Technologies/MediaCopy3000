{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.List (List)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Effectful (runEff)
import Options.Applicative
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.OsPath (OsPath, encodeUtf)

import MediaCopy.Demo (Scene (..), lookupScene, sceneNames)
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (planBlocked)
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Engine (planJob)
import MediaCopy.Gtk.Runtime qualified as Runtime
import MediaCopy.Gtk.Screenshot (Startup (..), defaultStartup)
import MediaCopy.Report (renderPlanText)

main :: IO ()
main = do
  args <- getArgs
  case args of
    first : _ | first `elem` ownArguments -> parseAndRun args
    -- Every other argument reaches GTK untouched, which the desktop file needs for
    -- @--gapplication-service@ and the toolkit needs for its own options.
    _ -> run Gui

-- | Every argument this project answers itself. A word that is not here belongs to GTK,
-- so a new command of our own goes in this list and in 'commandParser' together.
ownArguments :: List String
ownArguments =
  [ "plan"
  , "--list-scenes"
  , "--help"
  , "-h"
  ]
    <> completionArguments

-- | The flags optparse-applicative answers on its own, so a shell asking for completions reaches it
-- rather than opening a window.
completionArguments :: List String
completionArguments =
  [ "--bash-completion-index"
  , "--bash-completion-word"
  , "--bash-completion-enriched"
  , "--bash-completion-script"
  , "--zsh-completion-script"
  , "--fish-completion-script"
  ]

parseAndRun :: List String -> IO ()
parseAndRun args =
  case execParserPure defaultPrefs commandInfo args of
    Success cmd -> run cmd
    -- @--help@ is a failure to optparse with a success code; a wrong argument exits 2, as the manual says.
    Failure failure -> do
      let (message, code) = renderFailure failure "mediacopy3000"
      case code of
        ExitSuccess -> putStrLn message
        ExitFailure _ -> hPutStrLn stderr message >> exitWith (ExitFailure 2)
    CompletionInvoked completion -> execCompletion completion "mediacopy3000" >>= putStr

data Command
  = ListScenes
  | PlanOnly Job
  | Gui

run :: Command -> IO ()
run = \case
  -- The screenshot script asks for the scene names rather than repeating them.
  ListScenes -> mapM_ T.putStrLn sceneNames
  PlanOnly job -> planCommand job
  Gui -> do
    T.putStrLn banner
    startup >>= Runtime.start

commandInfo :: ParserInfo Command
commandInfo =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "Copy a media source and record every file it copied. With no command, the window opens.")

commandParser :: Parser Command
commandParser =
  flag' ListScenes (long "list-scenes" <> help "Print the demo scene names, one per line, and exit")
    <|> hsubparser (command "plan" (info (PlanOnly <$> planParser) (progDesc "Print a plan without the window")))

-- | The three shapes the @plan@ command takes.
planParser :: Parser Job
planParser =
  hsubparser
    ( command "offload" (info offloadParser (progDesc "Plan an offload of SOURCE into every DEST"))
        <> command "verify" (info (VerifyFolder . VerifyJob <$> folderArg) (progDesc "Plan a verify of FOLDER against its history"))
        <> command "seal" (info (SealMediaSource . SealJob <$> folderArg) (progDesc "Plan a seal of FOLDER"))
    )

offloadParser :: Parser Job
offloadParser =
  offloadOf
    <$> argument pathReader (metavar "SOURCE" <> help "The media source folder")
    <*> argument pathReader (metavar "DEST" <> help "A destination folder")
    <*> many (argument pathReader (metavar "DEST..."))
    <*> optional
      ( flag' Resume (long "resume" <> help "Keep the files a partial destination already holds")
          <|> flag' Replace (long "replace" <> help "Copy every file again over a partial destination")
      )
    <*> flag UseHistory (SealBeforeCopy StopBeforeCopy) (long "seal-first" <> help "Seal the media source first, and stop if the seal finds a problem")
  where
    offloadOf source firstDest moreDests existingCopy sealFirst =
      Offload OffloadJob {source, destinations = firstDest :| moreDests, sealFirst, existingCopy}

folderArg :: Parser OsPath
folderArg = argument pathReader (metavar "FOLDER")

-- | A path the host cannot encode is a usage error, so it exits 2 like a wrong flag.
pathReader :: ReadM OsPath
pathReader = eitherReader (\raw -> maybe (Left ("not a usable path: " <> raw)) Right (encodeUtf raw))

-- | Reads and prints; writes nothing. Exit 0 when the plan is ready, 1 when a blocker stands, 2 for a plan that could not be made.
planCommand :: Job -> IO ()
planCommand job = do
  -- The plan holds characters above ASCII, which a C locale's handle would refuse.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  now <- getCurrentTime
  let spec = JobSpec {jobId = JobId 1, job, createdAt = now}
  attempt <- try @SomeException (runEff (runFileSystemIO defaultChunkSize (planJob spec)))
  case attempt of
    Left err -> T.hPutStrLn stderr (T.pack (displayException err)) >> exitWith (ExitFailure 2)
    Right plan -> do
      T.putStr (renderPlanText spec plan)
      exitWith (if planBlocked plan then ExitFailure 1 else ExitSuccess)

-- | A run with no @MC3K_DEMO@ in its environment starts the empty application, as always. With
-- one, it shows a fixture screen from @manual/@. If @MC3K_SHOT@ names a file, the run photographs
-- the screen and leaves.
startup :: IO Startup
startup =
  lookupEnv "MC3K_DEMO" >>= \case
    Nothing -> pure defaultStartup
    Just wanted -> case lookupScene (T.pack wanted) of
      Nothing -> die ("no such demo scene: " <> wanted <> "\nknown scenes: " <> T.unpack (T.intercalate ", " sceneNames))
      Just scene -> do
        shot <- lookupEnv "MC3K_SHOT"
        pure
          defaultStartup
            { frames = NE.toList scene.frames
            , action = scene.action
            , shot
            , expand = scene.expand
            , scroll = scene.scroll
            }

banner :: T.Text
banner =
  """
   __  __          _ _        _____                  ____  ___   ___   ___
  |  \\/  |        | (_)      / ____|                |__ / / _ \\ / _ \\ / _ \\
  | \\  / | ___  __| |_  __ _| |     ___  _ __  _   _ |_ \\| (_) | (_) | (_) |
  | |\\/| |/ _ \\/ _` | |/ _` | |    / _ \\| '_ \\| | | |___/ \\___/ \\___/ \\___/
  | |  | |  __/ (_| | | (_| | |___| (_) | |_) | |_| |
  |_|  |_|\\___|\\__,_|_|\\__,_|\\_____\\___/| .__/ \\__, |
                                        | |     __/ |
                                        |_|    |___/
  """
