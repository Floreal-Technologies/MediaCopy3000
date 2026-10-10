module MediaCopy.Gtk.View
  ( Widgets (..)
  , buildWidgets
  ) where

import Control.Monad (forM_, void)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Vector (Vector)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (JobId)
import MediaCopy.Gtk.Actions (Actions (..), installActions, presentAbout)
import MediaCopy.Gtk.Widgets.Bind (bindQuietly, listSelection)
import MediaCopy.Gtk.Widgets.CloseConfirm (newCloseConfirm)
import MediaCopy.Gtk.Widgets.CommandPalette (newCommandPalette)
import MediaCopy.Gtk.Widgets.Common (Cell, flatNamed, newCell, renderCell)
import MediaCopy.Gtk.Widgets.JobDetail (JobDetail (..), newJobDetail)
import MediaCopy.Gtk.Widgets.JobRow (JobRow (..), newJobRow)
import MediaCopy.Gtk.Widgets.OffloadDialog (newOffloadDialog, renderOffloadDialog)
import MediaCopy.Gtk.Widgets.PlanSheet (newPlanSheet, renderPlanSheet)
import MediaCopy.Gtk.Widgets.Preferences (Preferences (..), newPreferences)
import MediaCopy.Interface.Command (mainMenuLabel)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Palette (paletteView)
import MediaCopy.Interface.Theme (Appearance, PaletteMode, ThemeSection)
import MediaCopy.Interface.Translation
import MediaCopy.Model (Chrome (..), JobEntry (..), Model (..), UiMessage (..), commandEnabled, selectedEntry)

data Widgets = Widgets
  { window :: Adw.ApplicationWindow
  , render :: Model -> IO ()
  , present :: Chrome -> IO ()
  }

buildWidgets
  :: Adw.Application
  -> (Appearance -> PaletteMode -> IO ())
  -> Wording
  -> Vector ThemeSection
  -> Vector ThemeSection
  -> (UiMessage -> IO ())
  -> IO Widgets
buildWidgets app applyTheme wording lightSections darkSections dispatch = do
  window <- newAppWindow app
  actions <- installActions app window dispatch
  toolbar <- newHeaderToolbar actions wording
  toastOverlay <- new Adw.ToastOverlay []
  detail <- newJobDetail actions.button dispatch
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
  preferences <- newPreferences window wording lightSections darkSections dispatch
  closeConfirm <- newCloseConfirm window dispatch
  paintPalette <- newCommandPalette window actions.accelLabel dispatch
  let render current = do
        renderCell themeCell (current.appearance, current.desktopBase)
        let selected = selectedEntry current
        renderSidebar sidebar current (if isJust selected then current.selected else Nothing)
        Gtk.stackSetVisibleChildName contentStack (if isJust selected then "detail" else "empty")
        detail.render current selected
        actions.render current
        renderOffloadDialog offloadDialog current
        renderPlanSheet planSheet current
        preferences.render current
        closeConfirm current.closeConfirm
        paintPalette (paletteView current.wording (commandEnabled current) current.palette)
        renderCell toastCell current.toast
      present = \case
        ShowPreferences page -> preferences.showPage page
        ShowAbout -> presentAbout window
        ShowShortcuts -> void (Gtk.widgetActivateAction window "win.show-help-overlay" Nothing)
  pure Widgets {window, render, present}

newAppWindow :: Adw.Application -> IO Adw.ApplicationWindow
newAppWindow app =
  new
    Adw.ApplicationWindow
    [ #application := app
    , #defaultWidth := 1100
    , #defaultHeight := 720
    , #title := "MediaCopy 3000"
    ]

newHeaderToolbar :: Actions -> Wording -> IO Adw.ToolbarView
newHeaderToolbar actions wording = do
  toolbar <- new Adw.ToolbarView []
  headerBar <- new Adw.HeaderBar []
  actions.button Command.NewOffload ["suggested-action"] >>= Adw.headerBarPackStart headerBar
  actions.button Command.VerifyFolder [] >>= Adw.headerBarPackStart headerBar
  actions.button Command.SealMedia [] >>= Adw.headerBarPackStart headerBar
  menuButton <-
    new
      Gtk.MenuButton
      [ #iconName := "open-menu-symbolic"
      , #tooltipText := mainMenuLabel wording
      , #primary := True
      , #menuModel := actions.menu
      ]
  flatNamed menuButton (mainMenuLabel wording)
  Adw.headerBarPackEnd headerBar menuButton
  Adw.toolbarViewAddTopBar toolbar headerBar
  pure toolbar

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

addNarrowBreakpoint :: Adw.ApplicationWindow -> Adw.NavigationSplitView -> IO ()
addNarrowBreakpoint window splitView = do
  narrow <- Adw.breakpointConditionParse "max-width: 620sp"
  breakpoint <- Adw.breakpointNew narrow
  void $ on breakpoint #apply $ set splitView [#collapsed := True]
  void $ on breakpoint #unapply $ set splitView [#collapsed := False]
  Adw.applicationWindowAddBreakpoint window breakpoint

newToastCell :: Adw.ToastOverlay -> (UiMessage -> IO ()) -> IO (Cell (Maybe Text))
newToastCell toastOverlay dispatch =
  newCell $ \message ->
    forM_
      message
      ( \text -> do
          toast <- new Adw.Toast [#title := text, #useMarkup := False]
          Adw.toastOverlayAddToast toastOverlay toast
          void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatch DismissToast >> pure False))
      )

data Sidebar = Sidebar
  { list :: Gtk.ListBox
  , rows :: IORef (Map JobId JobRow)
  , paintSelection :: Maybe JobId -> IO ()
  , quietly :: IO () -> IO ()
  }

newSidebar :: (UiMessage -> IO ()) -> IO (Sidebar, Adw.NavigationPage)
newSidebar dispatch = do
  list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
  Gtk.widgetAddCssClass list "navigation-sidebar"
  scroll <- new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  page <- Adw.navigationPageNew scroll "Jobs"
  set page [#widthRequest := 260]
  rows <- newIORef Map.empty
  let rowOf jobId = fmap (\jobRow -> jobRow.row) . Map.lookup jobId <$> readIORef rows
      keyOf listRow = readIORef rows <&> \jobs -> jobs & Map.filter (\jobRow -> jobRow.row == listRow) & Map.lookupMin & fmap fst
  (paintSelection, quietly) <- bindQuietly (listSelection list keyOf rowOf) (dispatch . SelectJob)
  pure (Sidebar {list, rows, paintSelection, quietly}, page)

renderSidebar :: Sidebar -> Model -> Maybe JobId -> IO ()
renderSidebar sidebar current selected = sidebar.quietly $ do
  existing <- readIORef sidebar.rows
  let gone = Map.difference existing current.jobs
      kept = Map.intersectionWith (,) existing current.jobs
      missing = Map.difference current.jobs existing
  traverse_ (\jobRow -> Gtk.listBoxRemove sidebar.list jobRow.row) gone
  added <- traverse (\entry -> newJobRow current.wording entry.state) missing
  traverse_ (\jobRow -> Gtk.listBoxAppend sidebar.list jobRow.row) added
  writeIORef sidebar.rows (Map.union (Map.map fst kept) added)
  traverse_ (\(jobRow, entry) -> jobRow.update current.wording current.now entry.state) kept
  sidebar.paintSelection selected
