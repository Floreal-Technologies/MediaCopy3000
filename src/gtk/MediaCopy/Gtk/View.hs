-- | The window shell components:
--
-- * header bar;
-- * toast overlay;
-- * split view;
-- * sidebar rows;
-- * detail pane;
-- * dialogs.
module MediaCopy.Gtk.View
  ( Widgets (..)
  , buildWidgets
  ) where

import Control.Monad (void)
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.GI.Base.GValue (toGValue)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (JobId, JobState (..))
import MediaCopy.Gtk.Actions (headerAction, installActions)
import MediaCopy.Gtk.Widgets.CloseConfirm (newCloseConfirm, renderCloseConfirm)
import MediaCopy.Gtk.Widgets.Common (Cell, flatNamed, newCell, renderCell, suppressing, unlessSuppressed)
import MediaCopy.Gtk.Widgets.JobDetail (JobDetail (..), newJobDetail)
import MediaCopy.Gtk.Widgets.JobRow (JobRow (..), jobIdOfRow, newJobRow)
import MediaCopy.Gtk.Widgets.OffloadDialog (newOffloadDialog, renderOffloadDialog)
import MediaCopy.Gtk.Widgets.PlanSheet (newPlanSheet, renderPlanSheet)
import MediaCopy.Gtk.Widgets.Preferences (newPreferences)
import MediaCopy.Interface.Theme (Appearance, PaletteMode, ThemeSection)
import MediaCopy.Model (JobEntry (..), Model (..), UiMessage (..), selectedEntry)

data Sample = Sample
  { at :: UTCTime
  , bytes :: Int64
  , rate :: Double
  }

-- | The window and the one function that paints a model into it.
data Widgets = Widgets
  { window :: Adw.ApplicationWindow
  , render :: Model -> IO ()
  }

-- | 'applyTheme' dresses the window.
buildWidgets
  :: Adw.Application
  -- ^ The application
  -> (Appearance -> PaletteMode -> IO ())
  -- ^ the @applyTheme@ callback
  -> Vector ThemeSection
  -- ^ Light sections
  -> Vector ThemeSection
  -- ^ Dark sections
  -> (UiMessage -> IO ())
  -- ^ The message dispatcher
  -> IO Widgets
buildWidgets app applyTheme lightSections darkSections dispatch = do
  window <- newAppWindow app
  (menuModel, renderActions) <- installActions app window dispatch
  toolbar <- newHeaderToolbar menuModel
  toastOverlay <- new Adw.ToastOverlay []
  detail <- newJobDetail dispatch
  (sidebar, sidebarPage) <- newSidebar dispatch
  (contentStack, jobPage) <- newContentStack detail
  splitView <- new Adw.NavigationSplitView [#sidebar := sidebarPage, #content := jobPage]
  addNarrowBreakpoint window splitView
  set toolbar [#content := splitView]
  set toastOverlay [#child := toolbar]
  set window [#content := toastOverlay]
  toastCell <- newToastCell toastOverlay dispatch
  themeCell <- newCell (\(appearance, desktop) -> applyTheme appearance desktop)
  offloadDialog <- newOffloadDialog window dispatch
  planSheet <- newPlanSheet window dispatch
  paintPreferences <- newPreferences app window lightSections darkSections dispatch
  closeConfirm <- newCloseConfirm window dispatch
  let render current = do
        -- The stylesheet goes on before the widgets are painted, so no frame shows the theme it left.
        renderCell themeCell (current.appearance, current.desktopBase)
        rate <- renderSidebar sidebar current
        let selected = selectedEntry current
        Gtk.stackSetVisibleChildName contentStack (if isJust selected then "detail" else "empty")
        detail.render current selected rate
        renderActions current
        renderOffloadDialog offloadDialog current
        renderPlanSheet planSheet current
        paintPreferences current.appearance
        renderCloseConfirm closeConfirm current
        renderCell toastCell current.toast
  pure Widgets {window, render}

newAppWindow :: Adw.Application -> IO Adw.ApplicationWindow
newAppWindow app =
  new
    Adw.ApplicationWindow
    [ #application := app
    , #defaultWidth := 1100
    , #defaultHeight := 720
    , #title := "MediaCopy 3000"
    ]

-- | The header bar's three actions and the primary menu, in a toolbar that holds nothing under them yet.
newHeaderToolbar :: Gio.Menu -> IO Adw.ToolbarView
newHeaderToolbar menuModel = do
  toolbar <- new Adw.ToolbarView []
  headerBar <- new Adw.HeaderBar []
  headerAction headerBar "win.new-offload" ["suggested-action"]
  headerAction headerBar "win.verify" []
  headerAction headerBar "win.seal" []
  -- The primary menu. 'primary' is what makes F10 open it.
  menuButton <-
    new
      Gtk.MenuButton
      [ #iconName := "open-menu-symbolic"
      , #tooltipText := "Main Menu"
      , #primary := True
      , #menuModel := menuModel
      ]
  flatNamed menuButton "Main Menu"
  Adw.headerBarPackEnd headerBar menuButton
  Adw.toolbarViewAddTopBar toolbar headerBar
  pure toolbar

-- | The two faces of the content pane: the selected job, and the page shown when there is none.
newContentStack :: JobDetail -> IO (Gtk.Stack, Adw.NavigationPage)
newContentStack detail = do
  contentStack <- new Gtk.Stack []
  emptyPage <-
    new
      Adw.StatusPage
      [ #title := "No Job Selected"
      , #description := "Start an offload, verify a folder, or seal a media source"
      , #iconName := "drive-harddisk-symbolic"
      ]
  Gtk.stackAddNamed contentStack emptyPage (Just "empty")
  Gtk.stackAddNamed contentStack detail.root (Just "detail")
  jobPage <- Adw.navigationPageNew contentStack "Job"
  set jobPage [#widthRequest := 360]
  pure (contentStack, jobPage)

-- | Below this width the window cannot show the sidebar and the job together, so the split view
-- stacks them.
addNarrowBreakpoint :: Adw.ApplicationWindow -> Adw.NavigationSplitView -> IO ()
addNarrowBreakpoint window splitView = do
  narrow <- Adw.breakpointConditionParse "max-width: 620sp"
  breakpoint <- Adw.breakpointNew narrow
  collapsed <- toGValue True
  Adw.breakpointAddSetter breakpoint splitView "collapsed" (Just collapsed)
  Adw.applicationWindowAddBreakpoint window breakpoint

-- | The dismissal lands after the current paint, never inside it.
newToastCell :: Adw.ToastOverlay -> (UiMessage -> IO ()) -> IO (Cell (Maybe Text))
newToastCell toastOverlay dispatch =
  newCell $ \message ->
    mapM_
      ( \text -> do
          toast <- new Adw.Toast [#title := text, #useMarkup := False]
          Adw.toastOverlayAddToast toastOverlay toast
          void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatch DismissToast >> pure False))
      )
      message

-- * The sidebar

-- | The list of jobs, the rows it shows, and what a render needs to move them without dispatching.
data Sidebar = Sidebar
  { list :: Gtk.ListBox
  , rows :: IORef (Map JobId JobRow)
  , lastBytes :: IORef (Map JobId Sample)
  , suppress :: IORef Bool
  , selection :: Cell (Maybe JobId)
  }

newSidebar :: (UiMessage -> IO ()) -> IO (Sidebar, Adw.NavigationPage)
newSidebar dispatch = do
  -- `suppress` is True while a render moves the sidebar's rows, so a
  -- selection the render itself caused never dispatches.
  suppress <- newIORef False
  list <-
    new
      Gtk.ListBox
      [ #selectionMode := Gtk.SelectionModeSingle
      , On #rowSelected $ \picked ->
          unlessSuppressed suppress $ do
            chosen <- maybe (pure Nothing) jobIdOfRow picked
            dispatch (SelectJob chosen)
      ]
  Gtk.widgetAddCssClass list "navigation-sidebar"
  scroll <- new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  page <- Adw.navigationPageNew scroll "Jobs"
  set page [#widthRequest := 260]
  rows <- newIORef Map.empty
  lastBytes <- newIORef Map.empty
  selection <- newSelectionCell list rows
  pure (Sidebar {list, rows, lastBytes, suppress, selection}, page)

-- | The closure reads the row map itself, so the cell needs only the choice. A job id is never
-- reused, so a row the cell recorded as selected is the row that still carries that id.
newSelectionCell :: Gtk.ListBox -> IORef (Map JobId JobRow) -> IO (Cell (Maybe JobId))
newSelectionCell list rows =
  newCell $ \case
    Nothing -> Gtk.listBoxUnselectAll list
    Just jobId -> do
      current <- readIORef rows
      mapM_ (\jobRow -> Gtk.listBoxSelectRow list (Just jobRow.row)) (Map.lookup jobId current)

-- | Every row change and the selection happen under `suppress`, because removing the
-- selected row or selecting another one emits `rowSelected`. Returns the selected job's rate.
renderSidebar :: Sidebar -> Model -> IO Double
renderSidebar sidebar current = suppressing sidebar.suppress $ do
  existing <- readIORef sidebar.rows
  let gone = Map.difference existing current.jobs
      kept = Map.intersectionWith (,) existing current.jobs
      missing = Map.difference current.jobs existing
  mapM_ (\jobRow -> Gtk.listBoxRemove sidebar.list jobRow.row) gone
  added <- traverse (\entry -> newJobRow entry.state) missing
  mapM_ (\jobRow -> Gtk.listBoxAppend sidebar.list jobRow.row) added
  let rows = Map.union (Map.map fst kept) added
  writeIORef sidebar.rows rows
  modifyIORef' sidebar.lastBytes (\samples -> Map.intersection samples current.jobs)
  rates <-
    Map.traverseWithKey
      ( \jobId (jobRow, entry) -> do
          rate <- rateFor sidebar current jobId entry.state
          jobRow.update current.now entry.state rate
          pure rate
      )
      kept
  -- The cell holds the choice the list shows, so a job whose row is gone reads as none.
  let chosen = case current.selected of
        Nothing -> Nothing
        Just jobId -> if Map.member jobId rows then Just jobId else Nothing
  renderCell sidebar.selection chosen
  pure (fromMaybe 0 (current.selected >>= \jobId -> Map.lookup jobId rates))

-- | Only the running job moves bytes, so no other row asks for a rate.
rateFor :: Sidebar -> Model -> JobId -> JobState -> IO Double
rateFor sidebar current jobId state
  | current.running == Just jobId = throughput sidebar.lastBytes current.now jobId state.bytesDone
  | otherwise = pure 0

-- | A sample less than half a second old is too close to measure, so the last rate stands.
throughput :: IORef (Map JobId Sample) -> UTCTime -> JobId -> Int64 -> IO Double
throughput lastBytes now jobId bytes = do
  samples <- readIORef lastBytes
  case Map.lookup jobId samples of
    Nothing -> do
      writeIORef lastBytes (Map.insert jobId Sample {at = now, bytes, rate = 0} samples)
      pure 0
    Just previous -> do
      let elapsed = realToFrac (diffUTCTime now previous.at) :: Double
      if elapsed < 0.5
        then pure previous.rate
        else do
          let measured = fromIntegral (bytes - previous.bytes) / elapsed
          writeIORef lastBytes (Map.insert jobId Sample {at = now, bytes, rate = measured} samples)
          pure measured
