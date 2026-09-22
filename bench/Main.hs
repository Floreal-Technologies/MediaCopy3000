{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import Ascmhl.Path (RelPath)
import Control.Monad (foldM, unless, when)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (List, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Numeric (showFFloat)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Printf (printf)

import MediaCopy.Demo.Fixtures qualified as Fixtures
import MediaCopy.Domain.Job
import MediaCopy.Gtk.Environment (Environment (Production))
import MediaCopy.Gtk.Reload (loadCss)
import MediaCopy.Gtk.Theme (apply, loadPalettes, newThemeAdapter)
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Interface.Theme (PaletteMode (..), themeSections)
import MediaCopy.Model

benchJob :: JobId
benchJob = JobId 1

defaultFileCount :: Int
defaultFileCount = 500

defaultResultsPath :: FilePath
defaultResultsPath = "bench/results.csv"

main :: IO ()
main = do
  args <- getArgs
  let fileCount = readFileCount args
      resultsPath = readResultsPath args
  app <-
    new
      Adw.Application
      [ #applicationId := "eu.choutri.MediaCopy3000.RenderBench"
      , #flags := [Gio.ApplicationFlagsNonUnique]
      , On #activate (bench fileCount resultsPath ?self)
      ]
  status <- Gio.applicationRun app Nothing
  when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))

readFileCount :: List String -> Int
readFileCount = \case
  raw : _ -> case reads raw of
    (parsed, "") : _ | parsed > 0 -> parsed
    _ -> defaultFileCount
  [] -> defaultFileCount

readResultsPath :: List String -> FilePath
readResultsPath = \case
  _ : raw : _ -> raw
  _ -> defaultResultsPath

bench :: Int -> FilePath -> Adw.Application -> IO ()
bench fileCount resultsPath app = do
  themeAdapter <- newThemeAdapter Production
  palettes <- loadPalettes Production
  let lightSections = themeSections LightPalette palettes
      darkSections = themeSections DarkPalette palettes
  widgets <- buildWidgets app (apply themeAdapter) lightSections darkSections (\_intent -> pure ())
  loadCss Production
  Gtk.windowPresent widgets.window
  settle 500
  counters <- newCounters widgets.window
  let stream = streamOf (benchFiles fileCount)
  samples <- measure widgets stream
  settle 500
  result <- resultOf fileCount (length stream) counters samples
  baseline <- readBaseline resultsPath fileCount
  report result baseline resultsPath
  appendResult resultsPath result
  Gtk.windowDestroy widgets.window

benchFiles :: Int -> Vector (RelPath, FileSize)
benchFiles fileCount = V.generate fileCount (\index -> (Fixtures.rel (nameOf index), sizeOf index))
  where
    nameOf index = "A001C" <> T.justifyRight 4 '0' (T.pack (show index)) <> "_260912_R1AB.mov"
    sizeOf index = fromIntegral (1_000_000_000 + index * 7_654_321)

streamOf :: Vector (RelPath, FileSize) -> List Message
streamOf files = setupMessages files <> copyMessages files <> [EngineEvent benchJob (JobFinished AllOk)]

setupMessages :: Vector (RelPath, FileSize) -> List Message
setupMessages files =
  [ RequestPlan job
  , PlanComputed spec (Right (Fixtures.readyPlan spec))
  , Ui ConfirmPlan
  , EngineEvent benchJob (Planned (PlannedWork files (V.sum (V.map (\entry -> snd entry) files))))
  ]
  where
    job = Fixtures.offloadJob UseHistory
    spec = Fixtures.specFor benchJob job

copyMessages :: Vector (RelPath, FileSize) -> List Message
copyMessages files = files & V.toList & zip [0 ..] & walk 0
  where
    walk _ [] = []
    walk doneSoFar ((index, (path, size)) : rest) =
      let done = doneSoFar + size
      in fileMessages path done <> tickAt index <> walk done rest

fileMessages :: RelPath -> FileSize -> List Message
fileMessages path done =
  [ EngineEvent benchJob (FileStatusChanged path Copying)
  , EngineEvent benchJob (FileStatusChanged path Verifying)
  , EngineEvent benchJob (FileStatusChanged path (Done Ok))
  , EngineEvent benchJob (Progress done)
  ]

tickAt :: Int -> List Message
tickAt index
  | index `mod` 10 == 9 = [Tick (addUTCTime (fromIntegral index) Fixtures.at)]
  | otherwise = []

data Counters = Counters
  { layouts :: IORef Int
  , paints :: IORef Int
  }

newCounters :: Adw.ApplicationWindow -> IO (Maybe Counters)
newCounters window =
  Gtk.widgetGetFrameClock window >>= \case
    Nothing -> pure Nothing
    Just clock -> do
      layouts <- newIORef 0
      paints <- newIORef 0
      on clock #layout (bump layouts)
      on clock #afterPaint (bump paints)
      pure (Just Counters {layouts, paints})
  where
    bump counter = modifyIORef' counter (\seen -> seen + 1)

measure :: Widgets -> List Message -> IO (List Word64)
measure widgets stream = do
  samples <- newIORef []
  let step model msg = do
        let (next, _cmds) = update msg model
        before <- getMonotonicTimeNSec
        widgets.render next
        drain
        after <- getMonotonicTimeNSec
        modifyIORef' samples (\seen -> (after - before) : seen)
        pure next
  _final <- foldM step (initialModel Fixtures.at LightPalette) stream
  readIORef samples <&> reverse

drain :: IO ()
drain = do
  more <- GLib.mainContextIteration (Nothing @GLib.MainContext) False
  when more drain

settle :: Int -> IO ()
settle milliseconds = do
  loop <- GLib.mainLoopNew Nothing False
  GLib.timeoutAdd GLib.PRIORITY_DEFAULT (fromIntegral milliseconds) (GLib.mainLoopQuit loop >> pure False)
  GLib.mainLoopRun loop

data Result = Result
  { files :: Int
  , messages :: Int
  , renders :: Int
  , layouts :: Maybe Int
  , paints :: Maybe Int
  , totalNs :: Word64
  , meanNs :: Word64
  , p50Ns :: Word64
  , p99Ns :: Word64
  , maxNs :: Word64
  }

resultOf :: Int -> Int -> Maybe Counters -> List Word64 -> IO Result
resultOf files messages counters samples = do
  layouts <- traverse (\seen -> readIORef seen.layouts) counters
  paints <- traverse (\seen -> readIORef seen.paints) counters
  let sorted = samples & sort & V.fromList
      total = V.sum sorted
  pure
    Result
      { files
      , messages
      , renders = V.length sorted
      , layouts
      , paints
      , totalNs = total
      , meanNs = if V.null sorted then 0 else total `div` fromIntegral (V.length sorted)
      , p50Ns = percentile sorted 0.50
      , p99Ns = percentile sorted 0.99
      , maxNs = percentile sorted 1.00
      }

percentile :: Vector Word64 -> Double -> Word64
percentile sorted quantile
  | V.null sorted = 0
  | otherwise = sorted V.! place
  where
    place = min (V.length sorted - 1) (floor (quantile * fromIntegral (V.length sorted)))

resultsHeader :: Text
resultsHeader = "timestamp,files,messages,renders,layout,after_paint,total_ns,mean_ns,p50_ns,p99_ns,max_ns"

appendResult :: FilePath -> Result -> IO ()
appendResult path result = do
  exists <- doesFileExist path
  unless exists (T.writeFile path (resultsHeader <> "\n"))
  stamp <- getCurrentTime <&> \now -> T.pack (iso8601Show now)
  T.appendFile path (rowOf stamp result <> "\n")

rowOf :: Text -> Result -> Text
rowOf stamp result =
  T.intercalate
    ","
    [ stamp
    , number result.files
    , number result.messages
    , number result.renders
    , maybe "" (\seen -> number seen) result.layouts
    , maybe "" (\seen -> number seen) result.paints
    , number result.totalNs
    , number result.meanNs
    , number result.p50Ns
    , number result.p99Ns
    , number result.maxNs
    ]
  where
    number value = T.pack (show value)

readBaseline :: FilePath -> Int -> IO (Maybe (Text, Result))
readBaseline path fileCount = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      recorded <- T.readFile path <&> \text -> text & T.lines & drop 1 & mapMaybe parseRow
      recorded & filter (\pair -> (snd pair).files == fileCount) & lastOrNothing & pure

lastOrNothing :: List a -> Maybe a
lastOrNothing = \case
  [] -> Nothing
  values -> Just (last values)

parseRow :: Text -> Maybe (Text, Result)
parseRow line = case T.splitOn "," line of
  [stamp, files, messages, renders, layouts, paints, total, mean, p50, p99, peak] -> do
    parsedFiles <- readNumber files
    parsedMessages <- readNumber messages
    parsedRenders <- readNumber renders
    parsedTotal <- readNumber total
    parsedMean <- readNumber mean
    parsedP50 <- readNumber p50
    parsedP99 <- readNumber p99
    parsedMax <- readNumber peak
    pure
      ( stamp
      , Result
          { files = parsedFiles
          , messages = parsedMessages
          , renders = parsedRenders
          , layouts = readNumber layouts
          , paints = readNumber paints
          , totalNs = parsedTotal
          , meanNs = parsedMean
          , p50Ns = parsedP50
          , p99Ns = parsedP99
          , maxNs = parsedMax
          }
      )
  _ -> Nothing

readNumber :: (Read a) => Text -> Maybe a
readNumber raw = case reads (T.unpack raw) of
  (parsed, "") : _ -> Just parsed
  _ -> Nothing

report :: Result -> Maybe (Text, Result) -> FilePath -> IO ()
report result baseline path = do
  printf "render-bench  files=%d  messages=%d\n" result.files result.messages
  case baseline of
    Nothing -> printf "no baseline for files=%d in %s\n\n" result.files path
    Just (stamp, _) -> printf "baseline %s from %s\n\n" (T.unpack stamp) path
  printf "%-24s %12s %12s %11s\n" ("" :: String) ("this run" :: String) ("baseline" :: String) ("change" :: String)
  mapM_ (\metric -> printMetric metric) (metricsOf result (fmap (\pair -> snd pair) baseline))

data Metric = Metric
  { name :: String
  , decimals :: Int
  , current :: Maybe Double
  , baseline :: Maybe Double
  }

metricsOf :: Result -> Maybe Result -> List Metric
metricsOf result baseline =
  [ count "renders" (\seen -> Just seen.renders)
  , count "frameclock layout" (\seen -> seen.layouts)
  , count "frameclock after-paint" (\seen -> seen.paints)
  , duration "render total ms" (\seen -> seen.totalNs)
  , duration "render mean ms" (\seen -> seen.meanNs)
  , duration "render p50 ms" (\seen -> seen.p50Ns)
  , duration "render p99 ms" (\seen -> seen.p99Ns)
  , duration "render max ms" (\seen -> seen.maxNs)
  ]
  where
    count name readOut =
      Metric
        { name
        , decimals = 0
        , current = readOut result <&> \seen -> fromIntegral seen
        , baseline = case baseline of
            Nothing -> Nothing
            Just earlier -> readOut earlier <&> \seen -> fromIntegral seen
        }
    duration name readOut =
      Metric
        { name
        , decimals = 3
        , current = Just (millis (readOut result))
        , baseline = baseline <&> \earlier -> millis (readOut earlier)
        }

printMetric :: Metric -> IO ()
printMetric metric =
  printf
    "%-24s %12s %12s %11s\n"
    metric.name
    (numberOf metric.decimals metric.current)
    (numberOf metric.decimals metric.baseline)
    (changeOf metric.current metric.baseline)

numberOf :: Int -> Maybe Double -> String
numberOf decimals = \case
  Nothing -> "n/a"
  Just value -> showFFloat (Just decimals) value ""

changeOf :: Maybe Double -> Maybe Double -> String
changeOf current baseline = case (current, baseline) of
  (Just now, Just before) | before /= 0 -> printf "%+.1f%%" ((now - before) / before * 100)
  _ -> "n/a"

millis :: Word64 -> Double
millis nanoseconds = fromIntegral nanoseconds / 1e6
