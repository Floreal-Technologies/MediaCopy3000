{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import Ascmhl.OsPath qualified as OsPath
import Control.Exception (displayException)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful.Exception (try)
import Effectful.Time (currentTime)
import Floreal.Commons.Environment (getArgs, lookupEnv)
import Floreal.Commons.Log (withStderrLogger)
import MediaCopy.Plugin.Manifest qualified as Manifest
import Options.Applicative
import System.Exit (ExitCode (..), die, exitWith)
import System.OsPath (encodeUtf)
import Prelude hiding (first, try)

import MediaCopy.Demo (Scene (..), lookupScene, sceneNames)
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Effects.AppStack (AppStack)
import MediaCopy.Effects.AppStack qualified as AppStack
import MediaCopy.Effects.Console qualified as Console
import MediaCopy.Engine.Planner qualified as Planner
import MediaCopy.Gtk.Runtime qualified as Runtime
import MediaCopy.Gtk.Screenshot (Startup (..))
import MediaCopy.Plugin.Discovery qualified as Discovery
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Session qualified as Session
import MediaCopy.Plugin.Setup qualified as Setup
import MediaCopy.Report (renderPlanText)

type Outcome = Either String ExitCode

main :: IO ()
main =
  withStderrLogger (\logger -> AppStack.run logger program) >>= \case
    Left message -> die message
    Right code -> exitWith code

program :: Eff AppStack Outcome
program = do
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

parseAndRun :: List String -> Eff AppStack Outcome
parseAndRun args =
  case execParserPure defaultPrefs commandInfo args of
    Success cmd -> run cmd
    Failure failure -> do
      let (message, code) = renderFailure failure "mediacopy3000"
      case code of
        ExitSuccess -> Console.writeLine Console.Stdout (T.pack message) >> pure (Right ExitSuccess)
        ExitFailure _ -> Console.writeLine Console.Stderr (T.pack message) >> pure (Right (ExitFailure 2))
    CompletionInvoked completion -> do
      output <- liftIO (execCompletion completion "mediacopy3000")
      Console.writeText Console.Stdout (T.pack output)
      pure (Right ExitSuccess)

data Command
  = ListScenes
  | PlanOnly Job PluginOptions
  | Gui

data PluginOptions = PluginOptions
  { enabled :: Bool
  , fields :: Map Text (Map Text Text)
  }

run :: Command -> Eff AppStack Outcome
run = \case
  ListScenes -> forM_ sceneNames (Console.writeLine Console.Stdout) >> pure (Right ExitSuccess)
  PlanOnly job options -> planCommand job options
  Gui -> do
    Console.writeLine Console.Stdout banner
    startup >>= \case
      Left message -> pure (Left message)
      Right chosen -> liftIO (Runtime.start chosen) >> pure (Right ExitSuccess)

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
        <> command "verify" (info ((folderArg <&> Job.Verify <&> Job.VerifyFolder <&> PlanOnly) <*> pluginOptions) (progDesc "Plan a verify of FOLDER against its history"))
        <> command "seal" (info ((folderArg <&> Job.Seal <&> Job.SealMediaSource <&> PlanOnly) <*> pluginOptions) (progDesc "Plan a seal of FOLDER"))
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
      ( flag' Offload.Resume (long "resume" <> help "Keep the files a partial destination already holds")
          <|> flag' Offload.Replace (long "replace" <> help "Copy every file again over a partial destination")
      )
    <*> flag Offload.UseHistory (Offload.SealBeforeCopy Offload.StopBeforeCopy) (long "seal-first" <> help "Seal the media source first, and stop if the seal finds a problem")
  where
    offloadOf source firstDest moreDests existingCopy sealFirst =
      Job.Offload Offload.Offload {source, destinations = firstDest :| moreDests, sealFirst, existingCopy}

folderArg :: Parser OsPath
folderArg = argument pathReader (metavar "FOLDER")

pathReader :: ReadM OsPath
pathReader = eitherReader (\raw -> maybe (Left ("not a usable path: " <> raw)) Right (encodeUtf raw))

planCommand :: Job -> PluginOptions -> Eff AppStack Outcome
planCommand job options = do
  Console.useUtf8
  now <- currentTime
  let spec = JobSpec.JobSpec {id = JobId.JobId 1, job, createdAt = now, pluginFields = options.fields}
  attempt <- try @SomeException $ do
    plan <- Planner.plan spec
    if options.enabled then withPlugins plan else pure plan
  case attempt of
    Left err -> Console.writeLine Console.Stderr (T.pack (displayException err)) >> pure (Right (ExitFailure 2))
    Right plan -> do
      Console.writeText Console.Stdout (renderPlanText spec plan)
      pure (Right (if Plan.blocked plan then ExitFailure 1 else ExitSuccess))

withPlugins :: Plan -> Eff AppStack Plan
withPlugins plan = do
  setup <- Setup.load
  forM_ setup.grantsProblem (Console.writeLine Console.Stderr)
  forM_ setup.rejected (\rejected -> Console.writeLine Console.Stderr ("plug-in folder " <> OsPath.toText rejected.folder <> " " <> rejected.reason))
  forM_ setup.inactive (\inactive -> Console.writeLine Console.Stderr ("plug-in " <> inactive.installed.manifest.name <> " " <> inactive.reason))
  Session.inspectPlan
    Session.Config
      { plugins = setup.ready
      , locale = "en"
      , report = Console.writeLine Console.Stderr . display
      }
    plan

startup :: Eff AppStack (Either String (Maybe Startup))
startup =
  lookupEnv "MC3K_DEMO" >>= \case
    Nothing -> pure (Right Nothing)
    Just wanted -> case lookupScene (T.pack wanted) of
      Nothing -> pure (Left ("no such demo scene: " <> wanted <> "\nknown scenes: " <> T.unpack (T.intercalate ", " sceneNames)))
      Just scene -> do
        shot <- lookupEnv "MC3K_SHOT"
        pure (Right (Just Startup {scene = scene.frame, shot}))

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
