module MediaCopy.Gtk.Widgets.JobDetail
  ( JobDetail (..)
  , newJobDetail
  ) where

import Ascmhl.Path (RelPath, pathText)
import Ascmhl.Types (Generation (..), MhlHistory (..), algosText)
import Control.Monad (void, when)
import Data.Function ((&))
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job
  ( Counts (..)
  , ExistingCopy (..)
  , FileEntry (..)
  , Job (..)
  , JobId
  , JobPhase (..)
  , JobSpec (..)
  , JobState (..)
  , OffloadJob (..)
  , countOutcomes
  , fractionOf
  , historyFolder
  , isDone
  , isFailure
  , jobKind
  , jobLabel
  , jobRoot
  , rateOf
  )
import MediaCopy.Gtk.Actions (actionButton)
import MediaCopy.Gtk.Widgets.Common (nameAccessible, newLabel, paddedBox, suppressing, toggleClass, unlessSuppressed)
import MediaCopy.Gtk.Widgets.FileRow (FileRow (..), newFileRow)
import MediaCopy.Gtk.Widgets.History (HistoryView (..), newHistoryView, renderHistory)
import MediaCopy.Interface.Wording (KindUi (..), count, generationsText, humanBytes, humanEta, humanRate, kindUi, quietText)
import MediaCopy.Model (FileFilter (..), JobEntry (..), Model (..), UiMessage (..))

data JobDetail = JobDetail
  { root :: Gtk.Box
  , render :: Model -> Maybe JobEntry -> IO ()
  }

newJobDetail :: (UiMessage -> IO ()) -> IO JobDetail
newJobDetail dispatch = do
  heading <- newHeading
  progress <- newProgress
  counters <- newCounters
  history <- newHistoryView
  suppress <- newIORef False
  filterButtons <- newFilterButtons suppress dispatch
  files <- newFileListPane
  actions <- newActionBar
  root <- paddedBox Gtk.OrientationVertical 12 20
  Gtk.boxAppend root heading.title
  Gtk.boxAppend root heading.pathLine
  Gtk.boxAppend root heading.originsLine
  Gtk.boxAppend root progress.bar
  Gtk.boxAppend root progress.line
  Gtk.boxAppend root counters.box
  Gtk.boxAppend root history.root
  Gtk.boxAppend root filterButtons.box
  Gtk.boxAppend root files.header
  Gtk.boxAppend root files.scroll
  Gtk.boxAppend root actions
  let render model newEntry = case newEntry of
        Nothing -> renderHistory history Nothing
        Just entry -> do
          let state = entry.state
              loaded = entry.history
          renderHeading heading loaded state
          renderProgress progress model.now state (rateOf state)
          renderCounters counters loaded state
          renderHistory history (historyFor loaded state)
          suppressing suppress (selectFilter filterButtons model.fileFilter)
          diffFileList files model state
  pure JobDetail {root, render}

data Heading = Heading
  { title :: Gtk.Label
  , pathLine :: Gtk.Label
  , originsLine :: Gtk.Label
  }

newHeading :: IO Heading
newHeading = do
  title <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["title-2"]
  pathLine <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeMiddle] ["dim-label", "monospace"]
  originsLine <- newLabel "" [#xalign := 0, #ellipsize := Pango.EllipsizeModeEnd] ["dim-label", "caption"]
  pure Heading {title, pathLine, originsLine}

renderHeading :: Heading -> Maybe MhlHistory -> JobState -> IO ()
renderHeading heading loaded state = do
  set heading.title [#label := jobLabel state.spec.job]
  set heading.pathLine [#label := pathLineText loaded state]
  let origins = originsLineText state
  set heading.originsLine [#label := fromMaybe "" origins]
  Gtk.widgetSetVisible heading.originsLine (isJust origins)

data Progress = Progress
  { bar :: Gtk.ProgressBar
  , left :: Gtk.Label
  , right :: Gtk.Label
  , line :: Gtk.Box
  }

newProgress :: IO Progress
newProgress = do
  bar <- new Gtk.ProgressBar []
  nameAccessible bar "Job progress"
  left <- newLabel "" [#xalign := 0, #hexpand := True, #ellipsize := Pango.EllipsizeModeEnd] ["caption"]
  right <- newLabel "" [#xalign := 1] ["caption"]
  line <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Gtk.boxAppend line left
  Gtk.boxAppend line right
  pure Progress {bar, left, right, line}

renderProgress :: Progress -> UTCTime -> JobState -> Double -> IO ()
renderProgress progress now state rate = do
  Gtk.progressBarSetFraction progress.bar (fractionOf state)
  set progress.left [#label := progressLeftText state]
  set progress.right [#label := progressRightText now state rate]

data Counters = Counters
  { box :: Gtk.Box
  , verified :: Gtk.Label
  , failed :: Gtk.Label
  , missing :: Gtk.Label
  , newFiles :: Gtk.Label
  , algo :: Gtk.Label
  }

newCounters :: IO Counters
newCounters = do
  (verifiedBox, verified) <- newCounter "Verified"
  (failedBox, failed) <- newCounter "Failed"
  (missingBox, missing) <- newCounter "Missing"
  (newBox, newFiles) <- newCounter "New"
  (algoBox, algo) <- newCounter "Hash"
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 24]
  mapM_ (Gtk.boxAppend box) [verifiedBox, failedBox, missingBox, newBox, algoBox]
  pure Counters {box, verified, failed, missing, newFiles, algo}

renderCounters :: Counters -> Maybe MhlHistory -> JobState -> IO ()
renderCounters counters loaded state = do
  let counts = countOutcomes state
  set counters.verified [#label := count (counts.verified + counts.replaced)]
  set counters.failed [#label := count counts.failed]
  set counters.missing [#label := count counts.missing]
  set counters.newFiles [#label := count counts.new]
  set counters.algo [#label := algoText loaded state]
  toggleClass counters.verified "success" (counts.verified + counts.replaced > 0)
  toggleClass counters.failed "error" (counts.failed > 0)

data FileListPane = FileListPane
  { list :: Gtk.ListBox
  , header :: Gtk.Box
  , scroll :: Gtk.ScrolledWindow
  , rows :: IORef (Map RelPath FileRow)
  , lastRendered :: IORef (Maybe (JobId, Int, FileFilter))
  }

data FilterButtons = FilterButtons
  { box :: Gtk.Box
  , allButton :: Gtk.ToggleButton
  , failedButton :: Gtk.ToggleButton
  }

newFilterButtons :: IORef Bool -> (UiMessage -> IO ()) -> IO FilterButtons
newFilterButtons suppress dispatch = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignStart]
  Gtk.widgetAddCssClass box "linked"
  allButton <- new Gtk.ToggleButton [#label := "All"]
  failedButton <- new Gtk.ToggleButton [#label := "Failed Only"]
  Gtk.toggleButtonSetGroup failedButton (Just allButton)
  Gtk.boxAppend box allButton
  Gtk.boxAppend box failedButton
  let buttons = FilterButtons {box, allButton, failedButton}
  reportFilter suppress dispatch allButton AllFiles
  reportFilter suppress dispatch failedButton FailedOnly
  suppressing suppress (selectFilter buttons AllFiles)
  pure buttons

reportFilter :: IORef Bool -> (UiMessage -> IO ()) -> Gtk.ToggleButton -> FileFilter -> IO ()
reportFilter suppress dispatch button wanted =
  void $
    on button #toggled $
      unlessSuppressed suppress $ do
        active <- Gtk.toggleButtonGetActive button
        when active (dispatch (SetFileFilter wanted))

selectFilter :: FilterButtons -> FileFilter -> IO ()
selectFilter buttons = \case
  AllFiles -> Gtk.toggleButtonSetActive buttons.allButton True
  FailedOnly -> Gtk.toggleButtonSetActive buttons.failedButton True

newFileListPane :: IO FileListPane
newFileListPane = do
  header <- newFileListHeader
  list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  Gtk.widgetAddCssClass list "boxed-list"
  scroll <-
    new
      Gtk.ScrolledWindow
      [ #child := list
      , #hscrollbarPolicy := Gtk.PolicyTypeNever
      , #vexpand := True
      ]
  rows <- newIORef Map.empty
  lastRendered <- newIORef Nothing
  pure FileListPane {list, header, scroll, rows, lastRendered}

newActionBar :: IO Gtk.Box
newActionBar = do
  cancelBtn <- actionButton "win.cancel-job" ["destructive-action"]
  reviewBtn <- actionButton "win.review-job" []
  reportBtn <- actionButton "win.save-report" []
  actions <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd]
  Gtk.boxAppend actions cancelBtn
  Gtk.boxAppend actions reviewBtn
  Gtk.boxAppend actions reportBtn
  pure actions

diffFileList :: FileListPane -> Model -> JobState -> IO ()
diffFileList files model state = do
  rendered <- readIORef files.lastRendered
  let wanted = (state.spec.jobId, state.revision, model.fileFilter)
  when (rendered /= Just wanted) $ do
    when (fmap (\(jobId, _, _) -> jobId) rendered /= Just state.spec.jobId) (clearFileList files)
    writeIORef files.lastRendered (Just wanted)
    renderFileList files model state

renderFileList :: FileListPane -> Model -> JobState -> IO ()
renderFileList files model state = do
  let visible = visibleFiles model.fileFilter state
  let wanted = visible & V.toList & Map.fromList
  existing <- readIORef files.rows
  let gone = Map.difference existing wanted
  mapM_ (\fileRow -> Gtk.listBoxRemove files.list fileRow.row) gone
  writeIORef files.rows (Map.difference existing gone)
  V.imapM_ (syncFileRow files) visible

syncFileRow :: FileListPane -> Int -> (RelPath, FileEntry) -> IO ()
syncFileRow files index (path, entry) = do
  rows <- readIORef files.rows
  case Map.lookup path rows of
    Just fileRow -> fileRow.update entry.size entry.status
    Nothing -> do
      fileRow <- newFileRow path
      fileRow.update entry.size entry.status
      Gtk.listBoxInsert files.list fileRow.row (fromIntegral index)
      writeIORef files.rows (Map.insert path fileRow rows)

clearFileList :: FileListPane -> IO ()
clearFileList files = do
  existing <- readIORef files.rows
  mapM_ (\fileRow -> Gtk.listBoxRemove files.list fileRow.row) existing
  writeIORef files.rows Map.empty

newCounter :: Text -> IO (Gtk.Box, Gtk.Label)
newCounter caption = do
  value <- newLabel "0" [#xalign := 0] ["title-3"]
  captionLabel <- newLabel caption [#xalign := 0] ["caption", "dim-label"]
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2]
  Gtk.boxAppend box value
  Gtk.boxAppend box captionLabel
  pure (box, value)

newFileListHeader :: IO Gtk.Box
newFileListHeader = do
  fileCaption <- newHeaderLabel "File" 0 (-1) True
  sizeCaption <- newHeaderLabel "Size" 1 10 False
  statusCaption <- newHeaderLabel "Status" 0 22 False
  header <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 10
      , #marginStart := 10
      , #marginEnd := 10
      ]
  Gtk.boxAppend header fileCaption
  Gtk.boxAppend header sizeCaption
  Gtk.boxAppend header statusCaption
  pure header

newHeaderLabel :: Text -> Float -> Int32 -> Bool -> IO Gtk.Label
newHeaderLabel caption alignment chars expands =
  newLabel caption [#xalign := alignment, #widthChars := chars, #hexpand := expands] ["dim-label", "caption"]

visibleFiles :: FileFilter -> JobState -> Vector (RelPath, FileEntry)
visibleFiles wanted st = st.files & Map.toList & filter matches & V.fromList
  where
    matches (_, entry) = case wanted of
      AllFiles -> True
      FailedOnly -> isFailure entry.status

historyFor :: Maybe MhlHistory -> JobState -> Maybe MhlHistory
historyFor loaded state = case historyFolder state.spec.job of
  Nothing -> Nothing
  Just _ -> loaded

pathLineText :: Maybe MhlHistory -> JobState -> Text
pathLineText loaded state = case state.spec.job of
  Offload offload ->
    pathText offload.source
      <> " → "
      <> (offload.destinations & NE.toList & map pathText & T.intercalate " · ")
  (VerifyFolder _; SealMediaSource _) -> folderLine
  where
    folderLine =
      pathText (jobRoot state.spec.job) <> " · ascmhl/ chain: " <> chainText loaded

originsLineText :: JobState -> Maybe Text
originsLineText state
  | (kindUi (jobKind state.spec.job)).showsOriginals = fmap (\origin -> "Originals: " <> origin <> existingText state.spec.job) state.originsUsed
  | otherwise = Nothing

existingText :: Job -> Text
existingText = \case
  Offload oj | Just choice <- oj.existingCopy -> "\nExisting copy: " <> pastOf choice
  _ -> ""
  where
    pastOf = \case
      Resume -> "resumed"
      Replace -> "replaced"

chainText :: Maybe MhlHistory -> Text
chainText = \case
  Nothing -> "—"
  Just loaded -> generationsText (V.length loaded.generations)

algoText :: Maybe MhlHistory -> JobState -> Text
algoText loaded state = case state.originsAlgo of
  Just algo -> display algo
  Nothing -> algoOfLatestGeneration loaded

algoOfLatestGeneration :: Maybe MhlHistory -> Text
algoOfLatestGeneration = \case
  Nothing -> "—"
  Just loaded -> case V.unsnoc loaded.generations of
    Nothing -> "—"
    Just (_earlier, latest) -> algosText latest.algos

progressLeftText :: JobState -> Text
progressLeftText state =
  verbOf state
    <> " "
    <> count (doneCount state)
    <> " / "
    <> count (Map.size state.files)
    <> " files · "
    <> humanBytes state.bytesDone
    <> " of "
    <> humanBytes state.bytesTotal

progressRightText :: UTCTime -> JobState -> Double -> Text
progressRightText now state rate = case state.phase of
  Running
    | Just quiet <- quietText now state -> quiet
    | rate > 0 -> humanRate rate <> " · ETA " <> humanEta (remainingSeconds state rate)
    | otherwise -> humanRate rate <> " · ETA —"
  Finished _ -> "done"
  _ -> ""

remainingSeconds :: JobState -> Double -> Double
remainingSeconds state rate = fromIntegral (state.bytesTotal - state.bytesDone) / rate

verbOf :: JobState -> Text
verbOf state = case state.phase of
  Queued -> "Queued"
  NeedsReview -> "Needs review"
  Finished _ -> "Finished"
  Failed _ -> "Failed"
  Cancelled -> "Cancelled"
  _ -> (kindUi (jobKind state.spec.job)).runningVerb

doneCount :: JobState -> Int
doneCount state = state.files & Map.elems & filter (\entry -> isDone entry.status) & length
