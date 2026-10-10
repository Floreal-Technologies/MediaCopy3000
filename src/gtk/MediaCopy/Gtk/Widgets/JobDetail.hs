module MediaCopy.Gtk.Widgets.JobDetail
  ( JobDetail (..)
  , newJobDetail
  ) where

import Ascmhl.Path (RelPath)
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import MediaCopy.Domain.Job hiding (Progress)
import MediaCopy.Gtk.Widgets.Bind (bind, toggleRadio)
import MediaCopy.Gtk.Widgets.Common (RowHost (..), nameAccessible, newCell, newLabel, newRowsCell, paddedBox, renderCell, toggleClass)
import MediaCopy.Gtk.Widgets.FileRow (FileRow (..), newFileRow)
import MediaCopy.Gtk.Widgets.History (HistoryView (..), newHistoryView, renderHistory)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Translation
import MediaCopy.Interface.View.JobDetail (DetailView (..), detailView, historyFor, pluginStatusRows)
import MediaCopy.Model (FileFilter (..), JobEntry (..), Model (..), UiMessage (..))

data JobDetail = JobDetail
  { root :: Gtk.Box
  , render :: Model -> Maybe JobEntry -> IO ()
  }

newJobDetail :: (Command.Command -> List Text -> IO Gtk.Button) -> (UiMessage -> IO ()) -> IO JobDetail
newJobDetail button dispatch = do
  heading <- newHeading
  progress <- newProgress
  counters <- newCounters
  history <- newHistoryView
  pluginGroup <- new Adw.PreferencesGroup [#title := "Plug-ins", #visible := False]
  pluginRows <- newRowsCell (InGroup pluginGroup)
  (filterBox, paintFilter) <- newFilterButtons dispatch
  files <- newFileListPane
  actions <- newActionBar button
  root <- paddedBox Gtk.OrientationVertical 12 20
  Gtk.boxAppend root heading.title
  Gtk.boxAppend root heading.pathLine
  Gtk.boxAppend root heading.originsLine
  Gtk.boxAppend root progress.bar
  Gtk.boxAppend root progress.line
  Gtk.boxAppend root counters.box
  Gtk.boxAppend root pluginGroup
  Gtk.boxAppend root history.root
  Gtk.boxAppend root filterBox
  Gtk.boxAppend root files.header
  Gtk.boxAppend root files.scroll
  Gtk.boxAppend root actions
  detailCell <- newCell $ \view -> do
    set heading.title [#label := view.title]
    set heading.pathLine [#label := view.pathLine]
    set heading.originsLine [#label := fromMaybe "" view.origins]
    Gtk.widgetSetVisible heading.originsLine (isJust view.origins)
    Gtk.progressBarSetFraction progress.bar view.fraction
    set progress.left [#label := view.progressLeft]
    set progress.right [#label := view.progressRight]
    set counters.verified [#label := view.verified]
    set counters.failed [#label := view.failed]
    set counters.missing [#label := view.missing]
    set counters.newFiles [#label := view.new]
    set counters.algo [#label := view.algo]
    toggleClass counters.verified "success" view.anyVerified
    toggleClass counters.failed "error" view.anyFailed
  let render model newEntry = case newEntry of
        Nothing -> renderHistory history model.wording Nothing
        Just entry -> do
          let state = entry.state
              loaded = entry.history
          renderCell detailCell (detailView model.wording model.now loaded state)
          let statusRows = pluginStatusRows model.wording state.plugins
          renderCell pluginRows statusRows
          Gtk.widgetSetVisible pluginGroup (not (V.null statusRows))
          renderHistory history model.wording (historyFor loaded state)
          paintFilter (Just model.fileFilter)
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
  forM_ [verifiedBox, failedBox, missingBox, newBox, algoBox] (Gtk.boxAppend box)
  pure Counters {box, verified, failed, missing, newFiles, algo}

data FileListPane = FileListPane
  { list :: Gtk.ListBox
  , header :: Gtk.Box
  , scroll :: Gtk.ScrolledWindow
  , rows :: IORef (Map RelPath FileRow)
  , lastRendered :: IORef (Maybe (JobId, Int, FileFilter, Wording))
  }

newFilterButtons :: (UiMessage -> IO ()) -> IO (Gtk.Box, Maybe FileFilter -> IO ())
newFilterButtons dispatch = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignStart]
  Gtk.widgetAddCssClass box "linked"
  allButton <- new Gtk.ToggleButton [#label := "All"]
  failedButton <- new Gtk.ToggleButton [#label := "Failed Only"]
  Gtk.toggleButtonSetGroup failedButton (Just allButton)
  Gtk.boxAppend box allButton
  Gtk.boxAppend box failedButton
  paintFilter <- bind (toggleRadio [(AllFiles, allButton), (FailedOnly, failedButton)]) (mapM_ (dispatch . SetFileFilter))
  paintFilter (Just AllFiles)
  pure (box, paintFilter)

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

newActionBar :: (Command.Command -> List Text -> IO Gtk.Button) -> IO Gtk.Box
newActionBar button = do
  cancelBtn <- button Command.CancelJob ["destructive-action"]
  reviewBtn <- button Command.ReviewJob []
  reportBtn <- button Command.SaveReport []
  actions <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd]
  Gtk.boxAppend actions cancelBtn
  Gtk.boxAppend actions reviewBtn
  Gtk.boxAppend actions reportBtn
  pure actions

diffFileList :: FileListPane -> Model -> JobState -> IO ()
diffFileList files model state = do
  rendered <- readIORef files.lastRendered
  let wanted = (state.spec.jobId, state.revision, model.fileFilter, model.wording)
  when (rendered /= Just wanted) $ do
    when (fmap (\(jobId, _, _, _) -> jobId) rendered /= Just state.spec.jobId) (clearFileList files)
    writeIORef files.lastRendered (Just wanted)
    renderFileList files model state

renderFileList :: FileListPane -> Model -> JobState -> IO ()
renderFileList files model state = do
  let visible = visibleFiles model.fileFilter state
  let wanted = visible & V.toList & Map.fromList
  existing <- readIORef files.rows
  let gone = Map.difference existing wanted
  forM_ gone (\fileRow -> Gtk.listBoxRemove files.list fileRow.row)
  writeIORef files.rows (Map.difference existing gone)
  V.imapM_ (syncFileRow files model.wording) visible

syncFileRow :: FileListPane -> Wording -> Int -> (RelPath, FileEntry) -> IO ()
syncFileRow files wording index (path, entry) = do
  rows <- readIORef files.rows
  case Map.lookup path rows of
    Just fileRow -> fileRow.update wording entry.size entry.status
    Nothing -> do
      fileRow <- newFileRow path
      fileRow.update wording entry.size entry.status
      Gtk.listBoxInsert files.list fileRow.row (fromIntegral index)
      writeIORef files.rows (Map.insert path fileRow rows)

clearFileList :: FileListPane -> IO ()
clearFileList files = do
  existing <- readIORef files.rows
  forM_ existing (\fileRow -> Gtk.listBoxRemove files.list fileRow.row)
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
