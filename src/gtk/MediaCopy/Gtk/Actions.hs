module MediaCopy.Gtk.Actions
  ( Actions (..)
  , installActions
  , presentAbout
  ) where

import Control.Monad (forM_, unless, when, zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (newCell, renderCell)
import MediaCopy.Interface.Command (Section (..), commandId, commandLabel, commands, mainMenuLabel, sectionLabel)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Model (CommandSpec (..), Model (..), Scope (..), UiMessage (..), commandEnabled, commandSpec)
import Paths_mediacopy3000 (version)

data Actions = Actions
  { menu :: Gio.Menu
  , button :: Command.Command -> List Text -> IO Gtk.Button
  , accelLabel :: Command.Command -> Maybe Text
  , render :: Model -> IO ()
  }

actionName :: Command.Command -> Text
actionName command = case (commandSpec command).scope of
  App -> "app." <> commandId command
  Win -> "win." <> commandId command
  Builtin full -> full

installActions :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO Actions
installActions app window dispatch = do
  installed <- traverse (installOne app window dispatch) commands
  accels <- Map.fromList <$> traverse (\command -> fmap (\label -> (command, label)) (acceleratorLabel (commandSpec command).accels)) commands
  buttons <- newIORef []
  menu <- Gio.menuNew
  wordingCell <- newCell (relabel window buttons menu)
  enabledCell <- newCell (zipWithM_ (\held enabled -> forM_ held (\action -> Gio.simpleActionSetEnabled action enabled)) installed)
  pure
    Actions
      { menu
      , button = newButton buttons
      , accelLabel = \command -> Map.findWithDefault Nothing command accels
      , render = \model -> do
          renderCell wordingCell model.wording
          renderCell enabledCell (map (commandEnabled model) commands)
      }

installOne :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> Command.Command -> IO (Maybe Gio.SimpleAction)
installOne app window dispatch command = do
  let spec = commandSpec command
  unless (null spec.accels) (Gtk.applicationSetAccelsForAction app (actionName command) spec.accels)
  case spec.scope of
    Builtin _ -> pure Nothing
    owner -> do
      action <-
        new
          Gio.SimpleAction
          [ #name := commandId command
          , On #activate (\_param -> dispatch (RunCommand command))
          ]
      case owner of
        App -> Gio.actionMapAddAction app action
        _ -> Gio.actionMapAddAction window action
      pure (Just action)

acceleratorLabel :: List Text -> IO (Maybe Text)
acceleratorLabel = \case
  [] -> pure Nothing
  accel : _ -> do
    (parsed, key, modifiers) <- Gtk.acceleratorParse accel
    if parsed then Just <$> Gtk.acceleratorGetLabel key modifiers else pure Nothing

newButton :: IORef (List (Gtk.Button, Command.Command)) -> Command.Command -> List Text -> IO Gtk.Button
newButton buttons command classes = do
  button <- new Gtk.Button [#actionName := actionName command]
  forM_ classes (Gtk.widgetAddCssClass button)
  modifyIORef' buttons ((button, command) :)
  pure button

relabel :: Adw.ApplicationWindow -> IORef (List (Gtk.Button, Command.Command)) -> Gio.Menu -> Wording -> IO ()
relabel window buttons menu wording = do
  labelled <- readIORef buttons
  forM_ labelled $ \(button, command) -> Gtk.buttonSetLabel button (commandLabel wording command)
  Gio.menuRemoveAll menu
  fillMenu menu wording
  overlay <- buildShortcutsWindow wording
  Gtk.applicationWindowSetHelpOverlay window (Just overlay)

fillMenu :: Gio.Menu -> Wording -> IO ()
fillMenu menu wording =
  forM_ [minBound .. maxBound] $ \group -> do
    let rows = filter (\command -> (commandSpec command).menu == Just group) commands
    unless (null rows) $ do
      submenu <- Gio.menuNew
      forM_ rows (menuRow submenu wording)
      Gio.menuAppendSection menu Nothing submenu

menuRow :: Gio.Menu -> Wording -> Command.Command -> IO ()
menuRow menu wording command = Gio.menuAppend menu (Just (commandLabel wording command)) (Just (actionName command))

buildShortcutsWindow :: Wording -> IO Gtk.ShortcutsWindow
buildShortcutsWindow wording = do
  shortcuts <- new Gtk.ShortcutsWindow []
  section <- new Gtk.ShortcutsSection [#sectionName := "shortcuts", #maxHeight := 12]
  forM_ [minBound .. maxBound] (addGroup wording section)
  Gtk.shortcutsWindowAddSection shortcuts section
  pure shortcuts

addGroup :: Wording -> Gtk.ShortcutsSection -> Section -> IO ()
addGroup wording section wanted = do
  let rows = filter (\command -> (commandSpec command).section == Just wanted) commands
  group <- new Gtk.ShortcutsGroup [#title := sectionLabel wording wanted]
  forM_ rows (addShortcut wording group)
  when (wanted == GeneralSection) $ do
    mainMenu <- new Gtk.ShortcutsShortcut [#title := mainMenuLabel wording, #accelerator := "F10"]
    Gtk.shortcutsGroupAddShortcut group mainMenu
  Gtk.shortcutsSectionAddGroup section group

addShortcut :: Wording -> Gtk.ShortcutsGroup -> Command.Command -> IO ()
addShortcut wording group command = do
  shortcut <-
    new
      Gtk.ShortcutsShortcut
      [ #title := commandLabel wording command
      , #actionName := actionName command
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
