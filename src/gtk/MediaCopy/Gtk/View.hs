module MediaCopy.Gtk.View
  ( Widgets (..)
  , buildWidgets
  ) where

import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Floreal.Commons.Gtk.Bind (bind, bindQuietly, dialog)
import Floreal.Commons.Gtk.CommandPalette (PaletteEvents (..), newCommandPalette)
import Floreal.Commons.Gtk.Common (Cell, newCell, renderCell)
import Floreal.Commons.Gtk.ListStore (listModel)
import Floreal.Commons.Gtk.Signal (onSelf)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Gtk.Actions (Actions (..), installActions, presentAbout)
import MediaCopy.Gtk.Widgets.Common (flatNamed)
import MediaCopy.Gtk.Widgets.JobDetail (JobDetail (..), newJobDetail)
import MediaCopy.Gtk.Widgets.JobRow (JobRow (..), newJobRow)
import MediaCopy.Gtk.Widgets.KeyCaps (newKeyCaps)
import MediaCopy.Gtk.Widgets.Lists (KeyedList (..), keyedSelection, newKeyedList, renderKeyed)
import MediaCopy.Gtk.Widgets.OffloadDialog (newOffloadDialog, renderOffloadDialog)
import MediaCopy.Gtk.Widgets.PlanSheet (newPlanSheet, renderPlanSheet)
import MediaCopy.Gtk.Widgets.Preferences (Preferences (..), newPreferences)
import MediaCopy.Interface.Command (mainMenuLabel)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Palette (paletteView)
import MediaCopy.Interface.Theme (Accent, Appearance, PaletteMode)
import MediaCopy.Interface.Translation
import MediaCopy.Model (Chrome (..), CommandSpec (..), JobEntry (..), Model (..), UiMessage (..), commandEnabled, commandSpec, selectedEntry)

data Widgets = Widgets
  { window :: Adw.ApplicationWindow
  , render :: Model -> IO ()
  , present :: Chrome -> IO ()
  }

buildWidgets
  :: Adw.Application
  -> (Appearance -> PaletteMode -> Accent -> IO ())
  -> Wording
  -> (UiMessage -> IO ())
  -> (UiMessage -> IO ())
  -> IO Widgets
buildWidgets app applyTheme wording dispatch postUi = do
  window <- newAppWindow app
  actions <- installActions app window dispatch
  toolbar <- newHeaderToolbar actions wording
  toastOverlay <- new Adw.ToastOverlay []
  detail <- newJobDetail actions.button dispatch
  (sidebar, sidebarPage) <- newSidebar dispatch
  (contentStack, jobPage) <- newContentStack actions detail
  splitView <- new Adw.NavigationSplitView [#sidebar := sidebarPage, #content := jobPage, #cssClasses := ["stripes"]]
  addNarrowBreakpoint window splitView
  set toolbar [#content := splitView]
  set toastOverlay [#child := toolbar]
  set window [#content := toastOverlay]
  toastCell <- newToastCell toastOverlay postUi
  themeCell <- newCell (\(appearance, desktop, desktopAccent) -> applyTheme appearance desktop desktopAccent)
  offloadDialog <- newOffloadDialog window dispatch
  planSheet <- newPlanSheet window dispatch
  preferences <- newPreferences window wording dispatch
  closeConfirm <- newCloseConfirm window dispatch
  paintPalette <-
    newCommandPalette
      window
      actions.accelLabel
      PaletteEvents {closed = dispatch ClosePalette, queryChanged = dispatch . SetPaletteQuery, run = dispatch . RunCommand}
  let render current = do
        renderCell themeCell (current.appearance, current.desktopBase, current.desktopAccent)
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
        ShowShortcuts -> actions.showShortcuts
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

newContentStack :: Actions -> JobDetail -> IO (Gtk.Stack, Adw.NavigationPage)
newContentStack actions detail = do
  contentStack <- new Gtk.Stack []
  emptyPage <- newEmptyPage actions
  Gtk.stackAddNamed contentStack emptyPage (Just "empty")
  Gtk.stackAddNamed contentStack detail.root (Just "detail")
  jobPage <- Adw.navigationPageNew contentStack "Job"
  set jobPage [#widthRequest := 360]
  pure (contentStack, jobPage)

newEmptyPage :: Actions -> IO Gtk.Grid
newEmptyPage actions = do
  grid <- new Gtk.Grid [#halign := Gtk.AlignCenter, #valign := Gtk.AlignCenter, #columnSpacing := 24, #rowSpacing := 6]
  forM_ (zip [0 ..] [Command.NewOffload, Command.VerifyFolder, Command.SealMedia, Command.Preferences, Command.Plugins]) $ \(row, command) -> do
    button <- actions.button command ["flat"]
    set button [#halign := Gtk.AlignEnd]
    keys <- newKeyCaps (fromMaybe "" (listToMaybe (commandSpec command).accels))
    set keys [#halign := Gtk.AlignStart]
    when (command == Command.Preferences) (set button [#marginTop := 12] >> set keys [#marginTop := 12])
    Gtk.gridAttach grid button 0 row 1 1
    Gtk.gridAttach grid keys 1 row 1 1
  pure grid

addNarrowBreakpoint :: Adw.ApplicationWindow -> Adw.NavigationSplitView -> IO ()
addNarrowBreakpoint window splitView = do
  narrow <- Adw.breakpointConditionParse "max-width: 620sp"
  breakpoint <- Adw.breakpointNew narrow
  void $ onSelf breakpoint #apply $ set splitView [#collapsed := True]
  void $ onSelf breakpoint #unapply $ set splitView [#collapsed := False]
  Adw.applicationWindowAddBreakpoint window breakpoint

newToastCell :: Adw.ToastOverlay -> (UiMessage -> IO ()) -> IO (Cell (Maybe Text))
newToastCell toastOverlay postUi =
  newCell $ \message ->
    forM_
      message
      ( \text -> do
          toast <- new Adw.Toast [#title := text, #useMarkup := False]
          Adw.toastOverlayAddToast toastOverlay toast
          postUi DismissToast
      )

newCloseConfirm
  :: Adw.ApplicationWindow
  -> (UiMessage -> IO ())
  -> IO (Bool -> IO ())
newCloseConfirm window dispatch = do
  alert <-
    new
      Adw.AlertDialog
      [ #heading := "Stop the running job?"
      , #body := "A job is still running. Closing the window stops it. The files already copied stay where they are."
      , On #response $ \answer ->
          if answer == "stop" then dispatch ConfirmClose else dispatch CancelClose
      ]
  Adw.alertDialogAddResponse alert "keep" "_Keep Running"
  Adw.alertDialogAddResponse alert "stop" "_Stop and Close"
  Adw.alertDialogSetResponseAppearance alert "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse alert (Just "keep")
  Adw.alertDialogSetCloseResponse alert "keep"
  asDialog <- Adw.toDialog alert
  openControl <- dialog asDialog window (pure ())
  bind openControl (\_ -> pure ())

data Sidebar = Sidebar
  { rows :: KeyedList JobId (Wording, UTCTime) JobEntry JobRow
  , paintSelection :: Maybe JobId -> IO ()
  , quietly :: IO () -> IO ()
  }

newSidebar :: (UiMessage -> IO ()) -> IO (Sidebar, Adw.NavigationPage)
newSidebar dispatch = do
  rows <-
    newKeyedList
      (newJobRow <&> \jobRow -> (jobRow.row, jobRow))
      (\(wording, now) _ entry jobRow -> jobRow.update wording now entry.state)
  items <- listModel rows.store
  selection <- new Gtk.SingleSelection [#model := items, #autoselect := False, #canUnselect := True]
  list <- new Gtk.ListView [#model := selection, #factory := rows.factory, #cssClasses := ["navigation-sidebar"]]
  scroll <- new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  page <- Adw.navigationPageNew scroll "Jobs"
  set page [#widthRequest := 260]
  (paintSelection, quietly) <- bindQuietly (keyedSelection selection rows) (dispatch . SelectJob)
  pure (Sidebar {rows, paintSelection, quietly}, page)

renderSidebar :: Sidebar -> Model -> Maybe JobId -> IO ()
renderSidebar sidebar current selected = sidebar.quietly $ do
  renderKeyed sidebar.rows (current.wording, current.now) current.jobs (V.fromList (Map.keys current.jobs))
  sidebar.paintSelection selected
