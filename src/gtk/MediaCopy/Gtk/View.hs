module MediaCopy.Gtk.View
  ( Widgets (..)
  , buildWidgets
  ) where

import Control.Monad (void)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Vector (Vector)
import Effectful (Eff, IOE, MonadIO, liftIO, (:>))
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (JobId)
import MediaCopy.Gtk.Actions (headerAction, installActions)
import MediaCopy.Gtk.Eff (idleE, onE)
import MediaCopy.Gtk.Environment (Ui)
import MediaCopy.Gtk.Widgets.CloseConfirm (newCloseConfirm)
import MediaCopy.Gtk.Widgets.Common (Cell, flatNamed, newCell, renderCell, suppressing, unlessSuppressed)
import MediaCopy.Gtk.Widgets.JobDetail (JobDetail (..), newJobDetail)
import MediaCopy.Gtk.Widgets.JobRow (JobRow (..), jobIdOfRow, newJobRow)
import MediaCopy.Gtk.Widgets.OffloadDialog (newOffloadDialog, renderOffloadDialog)
import MediaCopy.Gtk.Widgets.PlanSheet (newPlanSheet, renderPlanSheet)
import MediaCopy.Gtk.Widgets.Preferences (newPreferences)
import MediaCopy.Interface.Theme (Appearance, PaletteMode, ThemeSection)
import MediaCopy.Model (JobEntry (..), Model (..), UiMessage (..), selectedEntry)

data Widgets es = Widgets
  { window :: Adw.ApplicationWindow
  , render :: Model -> Eff es ()
  }

buildWidgets
  :: (Ui es)
  => Adw.Application
  -> (Appearance -> PaletteMode -> Eff es ())
  -> Vector ThemeSection
  -> Vector ThemeSection
  -> (UiMessage -> Eff es ())
  -> Eff es (Widgets es)
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
        renderCell themeCell (current.appearance, current.desktopBase)
        renderSidebar sidebar current
        let selected = selectedEntry current
        Gtk.stackSetVisibleChildName contentStack (if isJust selected then "detail" else "empty")
        detail.render current selected
        renderActions current
        renderOffloadDialog offloadDialog current
        renderPlanSheet planSheet current
        paintPreferences current.appearance
        renderCell closeConfirm current.closeConfirm
        renderCell toastCell current.toast
  pure Widgets {window, render}

newAppWindow :: (MonadIO m) => Adw.Application -> m Adw.ApplicationWindow
newAppWindow app =
  new
    Adw.ApplicationWindow
    [ #application := app
    , #defaultWidth := 1100
    , #defaultHeight := 720
    , #title := "MediaCopy 3000"
    ]

newHeaderToolbar :: (MonadIO m) => Gio.Menu -> m Adw.ToolbarView
newHeaderToolbar menuModel = do
  toolbar <- new Adw.ToolbarView []
  headerBar <- new Adw.HeaderBar []
  headerAction headerBar "win.new-offload" ["suggested-action"]
  headerAction headerBar "win.verify" []
  headerAction headerBar "win.seal" []
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

newContentStack :: (MonadIO m) => JobDetail es -> m (Gtk.Stack, Adw.NavigationPage)
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

addNarrowBreakpoint :: (Ui es) => Adw.ApplicationWindow -> Adw.NavigationSplitView -> Eff es ()
addNarrowBreakpoint window splitView = do
  narrow <- Adw.breakpointConditionParse "max-width: 620sp"
  breakpoint <- Adw.breakpointNew narrow
  void $ onE breakpoint #apply $ set splitView [#collapsed := True]
  void $ onE breakpoint #unapply $ set splitView [#collapsed := False]
  Adw.applicationWindowAddBreakpoint window breakpoint

newToastCell :: (Ui es) => Adw.ToastOverlay -> (UiMessage -> Eff es ()) -> Eff es (Cell es (Maybe Text))
newToastCell toastOverlay dispatch =
  newCell $ \message ->
    mapM_
      ( \text -> do
          toast <- new Adw.Toast [#title := text, #useMarkup := False]
          Adw.toastOverlayAddToast toastOverlay toast
          idleE (dispatch DismissToast)
      )
      message

data Sidebar es = Sidebar
  { list :: Gtk.ListBox
  , rows :: IORef (Map JobId (JobRow es))
  , suppress :: IORef Bool
  , selection :: Cell es (Maybe JobId)
  }

newSidebar :: (Ui es) => (UiMessage -> Eff es ()) -> Eff es (Sidebar es, Adw.NavigationPage)
newSidebar dispatch = do
  suppress <- liftIO (newIORef False)
  list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
  _ <- onE list #rowSelected $ \picked ->
    unlessSuppressed suppress $ do
      chosen <- maybe (pure Nothing) jobIdOfRow picked
      dispatch (SelectJob chosen)
  Gtk.widgetAddCssClass list "navigation-sidebar"
  scroll <- new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  page <- Adw.navigationPageNew scroll "Jobs"
  set page [#widthRequest := 260]
  rows <- liftIO (newIORef Map.empty)
  selection <- newSelectionCell list rows
  pure (Sidebar {list, rows, suppress, selection}, page)

newSelectionCell :: (IOE :> es) => Gtk.ListBox -> IORef (Map JobId (JobRow es)) -> Eff es (Cell es (Maybe JobId))
newSelectionCell list rows =
  newCell $ \case
    Nothing -> Gtk.listBoxUnselectAll list
    Just jobId -> do
      current <- liftIO (readIORef rows)
      mapM_ (\jobRow -> Gtk.listBoxSelectRow list (Just jobRow.row)) (Map.lookup jobId current)

renderSidebar :: (IOE :> es) => Sidebar es -> Model -> Eff es ()
renderSidebar sidebar current = suppressing sidebar.suppress $ do
  existing <- liftIO (readIORef sidebar.rows)
  let gone = Map.difference existing current.jobs
      kept = Map.intersectionWith (,) existing current.jobs
      missing = Map.difference current.jobs existing
  mapM_ (\jobRow -> Gtk.listBoxRemove sidebar.list jobRow.row) gone
  added <- traverse (\entry -> newJobRow entry.state) missing
  mapM_ (\jobRow -> Gtk.listBoxAppend sidebar.list jobRow.row) added
  let rows = Map.union (Map.map fst kept) added
  liftIO (writeIORef sidebar.rows rows)
  mapM_ (\(jobRow, entry) -> jobRow.update current.now entry.state) kept
  let chosen = case current.selected of
        Nothing -> Nothing
        Just jobId -> if Map.member jobId rows then Just jobId else Nothing
  renderCell sidebar.selection chosen
