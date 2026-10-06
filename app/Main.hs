{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import Ascmhl.Path (pathText)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_)
import Data.List (List)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Effectful (runEff)
import MediaCopy.Plugin.Manifest (PluginManifest (..))
import Options.Applicative
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.OsPath (OsPath, encodeUtf)

import MediaCopy.Demo (Scene (..), lookupScene, sceneNames)
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan, planBlocked)
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Engine (planJob)
import MediaCopy.Gtk.Runtime qualified as Runtime
import MediaCopy.Gtk.Screenshot (Startup (..))
import MediaCopy.Plugin (PluginSetup (..), loadPluginSetup, planWithPlugins)
import MediaCopy.Plugin.Discovery (Installed (..), Rejected (..))
import MediaCopy.Plugin.Grants (Inactive (..))
import MediaCopy.Plugin.Session (SessionConfig (..))
import MediaCopy.Report (renderPlanText)

main :: IO ()
main = do
  args <- getArgs
  case args of
    first : _ | first `elem` ownArguments -> parseAndRun args
    _ -> run Gui

ownArguments :: List String
ownArguments =
  [ "plan"
  , "--list-scenes"
  , "--help"
  , "-h"
  ]
    <> completionArguments

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
    Failure failure -> do
      let (message, code) = renderFailure failure "mediacopy3000"
      case code of
        ExitSuccess -> putStrLn message
        ExitFailure _ -> hPutStrLn stderr message >> exitWith (ExitFailure 2)
    CompletionInvoked completion -> execCompletion completion "mediacopy3000" >>= putStr

data Command
  = ListScenes
  | PlanOnly Job PluginOptions
  | Gui

data PluginOptions = PluginOptions
  { enabled :: Bool
  , fields :: Map Text (Map Text Text)
  }

run :: Command -> IO ()
run = \case
  ListScenes -> mapM_ T.putStrLn sceneNames
  PlanOnly job options -> planCommand job options
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
    <|> hsubparser (command "plan" (info planParser (progDesc "Print a plan without the window")))

planParser :: Parser Command
planParser =
  hsubparser
    ( command "offload" (info (PlanOnly <$> offloadParser <*> pluginOptions) (progDesc "Plan an offload of SOURCE into every DEST"))
        <> command "verify" (info (PlanOnly . VerifyFolder . VerifyJob <$> folderArg <*> pluginOptions) (progDesc "Plan a verify of FOLDER against its history"))
        <> command "seal" (info (PlanOnly . SealMediaSource . SealJob <$> folderArg <*> pluginOptions) (progDesc "Plan a seal of FOLDER"))
    )

pluginOptions :: Parser PluginOptions
pluginOptions =
  PluginOptions
    <$> (not <$> switch (long "no-plugins" <> help "Start no plug-in"))
    <*> (foldr addField Map.empty <$> many (option fieldReader (long "plugin-field" <> metavar "ID:KEY=VALUE" <> help "Give the job field KEY of the plug-in ID")))
  where
    addField (pluginId, key, given) = Map.insertWith Map.union pluginId (Map.singleton key given)

fieldReader :: ReadM (Text, Text, Text)
fieldReader = eitherReader $ \raw ->
  let (pluginId, rest) = T.breakOn ":" (T.pack raw)
      (key, given) = T.breakOn "=" (T.drop 1 rest)
  in if T.null pluginId || T.null rest || T.null key || T.null given
       then Left ("not ID:KEY=VALUE: " <> raw)
       else Right (pluginId, key, T.drop 1 given)

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

pathReader :: ReadM OsPath
pathReader = eitherReader (\raw -> maybe (Left ("not a usable path: " <> raw)) Right (encodeUtf raw))

planCommand :: Job -> PluginOptions -> IO ()
planCommand job options = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  now <- getCurrentTime
  let spec = JobSpec {jobId = JobId 1, job, createdAt = now, pluginFields = options.fields}
  attempt <- try @SomeException $ do
    plan <- runEff (runFileSystemIO defaultChunkSize (planJob spec))
    if options.enabled then withPlugins plan else pure plan
  case attempt of
    Left err -> T.hPutStrLn stderr (T.pack (displayException err)) >> exitWith (ExitFailure 2)
    Right plan -> do
      T.putStr (renderPlanText spec plan)
      exitWith (if planBlocked plan then ExitFailure 1 else ExitSuccess)

withPlugins :: JobPlan -> IO JobPlan
withPlugins plan = do
  setup <- loadPluginSetup
  forM_ setup.grantsProblem (T.hPutStrLn stderr)
  forM_ setup.rejected (\rejected -> T.hPutStrLn stderr ("plug-in folder " <> pathText rejected.folder <> " " <> rejected.reason))
  forM_ setup.inactive (\inactive -> T.hPutStrLn stderr ("plug-in " <> inactive.installed.manifest.name <> " " <> inactive.reason))
  planWithPlugins
    SessionConfig
      { plugins = setup.ready
      , locale = "en"
      , report = T.hPutStrLn stderr . display
      }
    plan

startup :: IO (Maybe Startup)
startup =
  lookupEnv "MC3K_DEMO" >>= \case
    Nothing -> pure Nothing
    Just wanted -> case lookupScene (T.pack wanted) of
      Nothing -> die ("no such demo scene: " <> wanted <> "\nknown scenes: " <> T.unpack (T.intercalate ", " sceneNames))
      Just scene -> do
        shot <- lookupEnv "MC3K_SHOT"
        pure
          ( Just
              Startup
                { frame = scene.frame
                , action = scene.action
                , shot
                , expand = scene.expand
                , scroll = scene.scroll
                }
          )

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
