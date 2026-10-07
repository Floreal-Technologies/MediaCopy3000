module MediaCopy.Gtk.Actions
  ( Actions (..)
  , installActions
  ) where

import Control.Monad (unless, void, when)
import Data.GI.Base (AttrOp (On, (:=)), new)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (newCell, renderCell)
import MediaCopy.Interface.Command (Section (..), commandId, commandLabel, commands, mainMenuLabel, sectionLabel)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Model (Model (..), UiMessage (..), commandEnabled)
import Paths_mediacopy3000 (version)

data Scope = App | Win | Builtin Text

data Effect = Send UiMessage | ShowAbout

data ActionSpec = ActionSpec
  { command :: Command.Command
  , scope :: Scope
  , accels :: List Text
  , section :: Maybe Section
  , effect :: Maybe Effect
  }

data Actions = Actions
  { menu :: Gio.Menu
  , button :: Command.Command -> List Text -> IO Gtk.Button
  , accelLabel :: Command.Command -> Maybe Text
  , activate :: Command.Command -> IO ()
  , render :: Model -> IO ()
  }

commandSpec :: Command.Command -> ActionSpec
commandSpec command = ActionSpec {command, scope, accels, section, effect}
  where
    (scope, accels, section, effect) = case command of
      Command.NewOffload -> (Win, ["<Control>n"], Just JobsSection, Just (Send OpenOffloadDialog))
      Command.VerifyFolder -> (Win, ["<Control>o"], Just JobsSection, Just (Send PickVerifyFolder))
      Command.SealMedia -> (Win, ["<Control>l"], Just JobsSection, Just (Send PickSealFolder))
      Command.SaveReport -> (Win, ["<Control>s"], Just JobsSection, Just (Send SaveSelectedReport))
      Command.CancelJob -> (Win, [], Nothing, Just (Send CancelSelectedJob))
      Command.ReviewJob -> (Win, [], Nothing, Just (Send ReviewSelectedJob))
      Command.ClearFinished -> (Win, [], Nothing, Just (Send ClearFinished))
      Command.NextJob -> (Win, ["<Control>Page_Down"], Just NavigationSection, Just (Send SelectNextJob))
      Command.PreviousJob -> (Win, ["<Control>Page_Up"], Just NavigationSection, Just (Send SelectPreviousJob))
      Command.Preferences -> (App, ["<Control>comma"], Just GeneralSection, Nothing)
      Command.Plugins -> (App, [], Nothing, Nothing)
      Command.KeyboardShortcuts -> (Builtin "win.show-help-overlay", ["<Control>question"], Just GeneralSection, Nothing)
      Command.About -> (App, [], Nothing, Just ShowAbout)
      Command.CloseWindow -> (Builtin "window.close", ["<Control>w"], Just GeneralSection, Nothing)
      Command.Quit -> (App, ["<Control>q"], Just GeneralSection, Just (Send RequestClose))
      Command.CommandPalette -> (Win, ["<Control>k"], Just GeneralSection, Just (Send OpenPalette))

actionTable :: Vector ActionSpec
actionTable = V.fromList (map commandSpec commands)

actionName :: Command.Command -> Text
actionName command = case (commandSpec command).scope of
  App -> "app." <> commandId command
  Win -> "win." <> commandId command
  Builtin full -> full

installActions :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO Actions
installActions app window dispatch = do
  gated <- V.mapMaybeM (installOne app window dispatch) actionTable
  accels <- Map.fromList <$> traverse (\command -> (command,) <$> acceleratorLabel (commandSpec command).accels) commands
  buttons <- newIORef []
  menu <- Gio.menuNew
  wordingCell <- newCell (relabel window buttons menu)
  enabledCell <- newCell (V.zipWithM_ (\(action, _) enabled -> Gio.simpleActionSetEnabled action enabled) gated)
  pure
    Actions
      { menu
      , button = newButton buttons
      , accelLabel = \command -> Map.findWithDefault Nothing command accels
      , activate = \command -> void (Gtk.widgetActivateAction window (actionName command) Nothing)
      , render = \model -> do
          renderCell wordingCell model.wording
          renderCell enabledCell (V.map (commandEnabled model . snd) gated)
      }

installOne
  :: Adw.Application
  -> Adw.ApplicationWindow
  -> (UiMessage -> IO ())
  -> ActionSpec
  -> IO (Maybe (Gio.SimpleAction, Command.Command))
installOne app window dispatch candidate = do
  unless (null candidate.accels) (Gtk.applicationSetAccelsForAction app (actionName candidate.command) candidate.accels)
  case candidate.effect of
    Nothing -> pure Nothing
    Just wanted -> do
      action <-
        new
          Gio.SimpleAction
          [ #name := commandId candidate.command
          , On #activate (\_param -> runEffect window dispatch wanted)
          ]
      case candidate.scope of
        App -> Gio.actionMapAddAction app action
        _ -> Gio.actionMapAddAction window action
      pure (Just (action, candidate.command))

runEffect :: Adw.ApplicationWindow -> (UiMessage -> IO ()) -> Effect -> IO ()
runEffect window dispatch = \case
  Send intent -> dispatch intent
  ShowAbout -> presentAbout window

acceleratorLabel :: List Text -> IO (Maybe Text)
acceleratorLabel = \case
  [] -> pure Nothing
  accel : _ -> do
    (parsed, key, modifiers) <- Gtk.acceleratorParse accel
    if parsed then Just <$> Gtk.acceleratorGetLabel key modifiers else pure Nothing

newButton :: IORef (List (Gtk.Button, Command.Command)) -> Command.Command -> List Text -> IO Gtk.Button
newButton buttons command classes = do
  button <- new Gtk.Button [#actionName := actionName command]
  mapM_ (Gtk.widgetAddCssClass button) classes
  modifyIORef' buttons ((button, command) :)
  pure button

relabel :: Adw.ApplicationWindow -> IORef (List (Gtk.Button, Command.Command)) -> Gio.Menu -> Wording -> IO ()
relabel window buttons menu wording = do
  readIORef buttons >>= mapM_ (\(button, command) -> Gtk.buttonSetLabel button (commandLabel wording command))
  Gio.menuRemoveAll menu
  fillMenu menu wording
  overlay <- buildShortcutsWindow wording
  Gtk.applicationWindowSetHelpOverlay window (Just overlay)

fillMenu :: Gio.Menu -> Wording -> IO ()
fillMenu menu wording = do
  jobs <- Gio.menuNew
  menuRow jobs wording Command.ClearFinished
  general <- Gio.menuNew
  mapM_ (menuRow general wording) [Command.CommandPalette, Command.Preferences, Command.Plugins, Command.KeyboardShortcuts, Command.About]
  Gio.menuAppendSection menu Nothing jobs
  Gio.menuAppendSection menu Nothing general

menuRow :: Gio.Menu -> Wording -> Command.Command -> IO ()
menuRow menu wording command = Gio.menuAppend menu (Just (commandLabel wording command)) (Just (actionName command))

buildShortcutsWindow :: Wording -> IO Gtk.ShortcutsWindow
buildShortcutsWindow wording = do
  shortcuts <- new Gtk.ShortcutsWindow []
  section <- new Gtk.ShortcutsSection [#sectionName := "shortcuts", #maxHeight := 12]
  mapM_ (addGroup wording section) [minBound .. maxBound]
  Gtk.shortcutsWindowAddSection shortcuts section
  pure shortcuts

addGroup :: Wording -> Gtk.ShortcutsSection -> Section -> IO ()
addGroup wording section wanted = do
  let rows = V.filter (\candidate -> candidate.section == Just wanted) actionTable
  group <- new Gtk.ShortcutsGroup [#title := sectionLabel wording wanted]
  V.mapM_ (addShortcut wording group) rows
  when (wanted == GeneralSection) $ do
    mainMenu <- new Gtk.ShortcutsShortcut [#title := mainMenuLabel wording, #accelerator := "F10"]
    Gtk.shortcutsGroupAddShortcut group mainMenu
  Gtk.shortcutsSectionAddGroup section group

addShortcut :: Wording -> Gtk.ShortcutsGroup -> ActionSpec -> IO ()
addShortcut wording group candidate = do
  shortcut <-
    new
      Gtk.ShortcutsShortcut
      [ #title := commandLabel wording candidate.command
      , #actionName := actionName candidate.command
      ]
  Gtk.shortcutsGroupAddShortcut group shortcut

presentAbout :: Adw.ApplicationWindow -> IO ()
presentAbout window = do
  dialog <-
    new
      Adw.AboutDialog
      [ #applicationName := "MediaCopy 3000"
      , #applicationIcon := "tech.floreal.MediaCopy3000"
      , #version := T.pack (showVersion version)
      , #developerName := "Floréal Technologies"
      , #comments := "Verified media offload and ASC MHL verification"
      , #licenseType := Gtk.LicenseGpl30Only
      ]
  Adw.dialogPresent dialog (Just window)
