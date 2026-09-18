-- | Every action the window offers. The accelerators, menu items and shortcut rows come from it.
module MediaCopy.Gtk.Actions
  ( actionLabel
  , actionButton
  , headerAction
  , installActions
  ) where

import Control.Monad (unless)
import Data.Function ((&))
import Data.GI.Base (AttrOp (On, (:=)), new)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Model
import Paths_mediacopy3000 (version)

-- | A group of the keyboard-shortcuts window.
data Section = JobsSection | NavigationSection | GeneralSection
  deriving stock (Eq, Show)

instance Display Section where
  displayBuilder = \case
    JobsSection -> "Jobs"
    NavigationSection -> "Navigation"
    GeneralSection -> "General"

-- | What an action does.
data Effect = Send UiMessage | ShowAbout

-- | One row of 'actionTable' is the only source of an action's name,
-- keys, label, row and enabled rule.
data ActionSpec = ActionSpec
  { name :: Maybe Text
  -- ^ 'Nothing' for a keyboard convention the toolkit implements without an action, such as F10.
  , label :: Text
  , accels :: List Text
  , section :: Maybe Section
  -- ^ 'Nothing' keeps the row out of the shortcuts window.
  , effect :: Maybe Effect
  -- ^ 'Nothing' when the toolkit already provides the action, such as @win.show-help-overlay@.
  , enabled :: Maybe (Model -> Bool)
  -- ^ 'Nothing' for an action the model never disables.
  }

-- | An action whose enabled state follows the model.
data Gated = Gated
  { action :: Gio.SimpleAction
  , rule :: Model -> Bool
  }

-- | Every action, its label and its accelerator. The header bar reads it too.
actionTable :: Vector ActionSpec
actionTable =
  V.fromList
    [ always "win.new-offload" "New Offload…" ["<Control>n"] (Just JobsSection) (Just (Send OpenOffloadDialog))
    , always "win.verify" "Verify Folder…" ["<Control>o"] (Just JobsSection) (Just (Send PickVerifyFolder))
    , always "win.seal" "Seal Media…" ["<Control>l"] (Just JobsSection) (Just (Send PickSealFolder))
    , (always "win.save-report" "Save Report…" ["<Control>s"] (Just JobsSection) (Just (Send SaveSelectedReport))) {enabled = Just canReportSelected}
    , (always "win.cancel-job" "Cancel Job" [] Nothing (Just (Send CancelSelectedJob))) {enabled = Just canCancelSelected}
    , (always "win.clear-finished" "Clear Finished" [] Nothing (Just (Send ClearFinished))) {enabled = Just hasFinishedJobs}
    , always "win.next-job" "Next Job" ["<Control>Page_Down"] (Just NavigationSection) (Just (Send SelectNextJob))
    , always "win.previous-job" "Previous Job" ["<Control>Page_Up"] (Just NavigationSection) (Just (Send SelectPreviousJob))
    , -- The Preferences dialog installs @app.preferences@ itself, because it owns whether it is open.
      always "app.preferences" "Preferences" ["<Control>comma"] (Just GeneralSection) Nothing
    , always "app.about" "About MediaCopy 3000" [] Nothing (Just ShowAbout)
    , always "win.show-help-overlay" "Keyboard Shortcuts" ["<Control>question"] (Just GeneralSection) Nothing
    , always "window.close" "Close Window" ["<Control>w"] (Just GeneralSection) Nothing
    , always "app.quit" "Quit" ["<Control>q"] (Just GeneralSection) (Just (Send RequestClose))
    , (always "" "Main Menu" ["F10"] (Just GeneralSection) Nothing) {name = Nothing}
    ]

-- | A row for an action the model never disables.
always :: Text -> Text -> List Text -> Maybe Section -> Maybe Effect -> ActionSpec
always actionName label accels section effect =
  ActionSpec {name = Just actionName, label, accels, section, effect, enabled = Nothing}

-- | Get the label for the given action.
actionLabel :: Text -> Text
actionLabel wanted =
  actionTable
    & V.find (\spec -> spec.name == Just wanted)
    & maybe wanted (\spec -> spec.label)

-- | Get the button from the provided action
actionButton :: Text -> List Text -> IO Gtk.Button
actionButton actionName classes = do
  button <- new Gtk.Button [#label := actionLabel actionName, #actionName := actionName]
  mapM_ (\klass -> Gtk.widgetAddCssClass button klass) classes
  pure button

-- | Add an action button to a header bar
headerAction :: Adw.HeaderBar -> Text -> List Text -> IO ()
headerAction header actionName classes = do
  button <- actionButton actionName classes
  Adw.headerBarPackStart header button

-- | Installs every action, the shortcuts window and the primary menu. Returns
-- the menu and the render that keeps each gated action's enabled state in step
-- with the model.
installActions :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO (Gio.Menu, Model -> IO ())
installActions app window dispatch = do
  gated <- V.foldM (\acc spec -> installOne app window dispatch acc spec) [] actionTable
  overlay <- buildShortcutsWindow
  appWindow <- Gtk.toApplicationWindow window
  Gtk.applicationWindowSetHelpOverlay appWindow (Just overlay)
  menu <- buildMenu
  pure (menu, \model -> mapM_ (\g -> Gio.simpleActionSetEnabled g.action (g.rule model)) gated)

installOne
  :: Adw.Application
  -> Adw.ApplicationWindow
  -> (UiMessage -> IO ())
  -> List Gated
  -> ActionSpec
  -> IO (List Gated)
installOne app window dispatch gated spec = case spec.name of
  Nothing -> pure gated
  Just full -> do
    unless (null spec.accels) (Gtk.applicationSetAccelsForAction app full spec.accels)
    case spec.effect of
      Nothing -> pure gated
      Just wanted -> do
        let (prefix, dotted) = T.breakOn "." full
        action <-
          new
            Gio.SimpleAction
            [ #name := T.drop 1 dotted
            , On #activate (\_param -> runEffect window dispatch wanted)
            ]
        if prefix == "app"
          then Gio.actionMapAddAction app action
          else Gio.actionMapAddAction window action
        pure (maybe gated (\rule -> Gated {action, rule} : gated) spec.enabled)

runEffect :: Adw.ApplicationWindow -> (UiMessage -> IO ()) -> Effect -> IO ()
runEffect window dispatch = \case
  Send intent -> dispatch intent
  ShowAbout -> presentAbout window

buildMenu :: IO Gio.Menu
buildMenu = do
  menu <- Gio.menuNew
  jobs <- Gio.menuNew
  menuRow jobs "win.clear-finished"
  general <- Gio.menuNew
  menuRow general "app.preferences"
  menuRow general "win.show-help-overlay"
  menuRow general "app.about"
  Gio.menuAppendSection menu Nothing jobs
  Gio.menuAppendSection menu Nothing general
  pure menu

-- | A menu row's label is its action's label, so the menu cannot drift from the table.
menuRow :: Gio.Menu -> Text -> IO ()
menuRow menu actionName = Gio.menuAppend menu (Just (actionLabel actionName)) (Just actionName)

buildShortcutsWindow :: IO Gtk.ShortcutsWindow
buildShortcutsWindow = do
  shortcuts <- new Gtk.ShortcutsWindow []
  section <- new Gtk.ShortcutsSection [#sectionName := "shortcuts", #maxHeight := 12]
  mapM_ (\wanted -> addGroup section wanted) [JobsSection, NavigationSection, GeneralSection]
  Gtk.shortcutsWindowAddSection shortcuts section
  pure shortcuts

addGroup :: Gtk.ShortcutsSection -> Section -> IO ()
addGroup section wanted = do
  let rows = V.filter (\spec -> spec.section == Just wanted) actionTable
  unless (V.null rows) $ do
    group <- new Gtk.ShortcutsGroup [#title := display wanted]
    V.mapM_ (\spec -> addShortcut group spec) rows
    Gtk.shortcutsSectionAddGroup section group

-- | A named row shows the key GTK really bound. A nameless row can only state its own.
addShortcut :: Gtk.ShortcutsGroup -> ActionSpec -> IO ()
addShortcut group spec = do
  shortcut <- case spec.name of
    Just full -> new Gtk.ShortcutsShortcut [#title := spec.label, #actionName := full]
    Nothing -> new Gtk.ShortcutsShortcut [#title := spec.label, #accelerator := T.unwords spec.accels]
  Gtk.shortcutsGroupAddShortcut group shortcut

-- | The icon name is the application id, which is what the desktop file installs.
presentAbout :: Adw.ApplicationWindow -> IO ()
presentAbout window = do
  dialog <-
    new
      Adw.AboutDialog
      [ #applicationName := "MediaCopy 3000"
      , #applicationIcon := "eu.choutri.MediaCopy3000"
      , #version := T.pack (showVersion version)
      , #developerName := "Feriel Choutri de Tarlé"
      , #comments := "Verified media offload and ASC MHL verification"
      , #licenseType := Gtk.LicenseGpl30Only
      ]
  Adw.dialogPresent dialog (Just window)
