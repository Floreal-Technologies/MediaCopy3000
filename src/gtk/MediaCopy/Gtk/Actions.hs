module MediaCopy.Gtk.Actions
  ( Actions (..)
  , installActions
  , presentAbout
  ) where

import Control.Monad (zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Version (showVersion)
import Floreal.Commons.Gtk.Common (newCell, renderCell)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.KeyCaps (keyNames, newKeyCaps)
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
  , showShortcuts :: IO ()
  }

actionName :: Command.Command -> Text
actionName command = case (commandSpec command).scope of
  App -> "app." <> commandId command
  Win -> "win." <> commandId command

installActions :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO Actions
installActions app window dispatch = do
  installed <- traverse (installOne app window dispatch) commands
  accels <- Map.fromList <$> traverse (\command -> fmap (\label -> (command, label)) (acceleratorLabel (commandSpec command).accels)) commands
  buttons <- newIORef []
  menu <- Gio.menuNew
  current <- newIORef Nothing
  shortcutsOpen <- newIORef False
  wordingCell <- newCell (relabel current buttons menu)
  enabledCell <- newCell (zipWithM_ Gio.simpleActionSetEnabled installed)
  pure
    Actions
      { menu
      , button = newButton buttons
      , accelLabel = \command -> Map.findWithDefault Nothing command accels
      , render = \model -> do
          renderCell wordingCell model.wording
          renderCell enabledCell (map (commandEnabled model) commands)
      , showShortcuts = do
          open <- readIORef shortcutsOpen
          unless open (readIORef current >>= traverse_ (presentShortcuts window shortcutsOpen))
      }

installOne :: Adw.Application -> Adw.ApplicationWindow -> (UiMessage -> IO ()) -> Command.Command -> IO Gio.SimpleAction
installOne app window dispatch command = do
  let spec = commandSpec command
  unless (null spec.accels) (Gtk.applicationSetAccelsForAction app (actionName command) spec.accels)
  action <-
    new
      Gio.SimpleAction
      [ #name := commandId command
      , On #activate (\_param -> dispatch (RunCommand command))
      ]
  case spec.scope of
    App -> Gio.actionMapAddAction app action
    Win -> Gio.actionMapAddAction window action
  pure action

acceleratorLabel :: List Text -> IO (Maybe Text)
acceleratorLabel = \case
  [] -> pure Nothing
  accel : _ -> do
    names <- keyNames accel
    pure (if null names then Nothing else Just (T.intercalate "+" names))

newButton :: IORef (List (Gtk.Button, Command.Command)) -> Command.Command -> List Text -> IO Gtk.Button
newButton buttons command classes = do
  button <- new Gtk.Button [#actionName := actionName command]
  forM_ classes (Gtk.widgetAddCssClass button)
  modifyIORef' buttons ((button, command) :)
  pure button

relabel :: IORef (Maybe Wording) -> IORef (List (Gtk.Button, Command.Command)) -> Gio.Menu -> Wording -> IO ()
relabel current buttons menu wording = do
  writeIORef current (Just wording)
  labelled <- readIORef buttons
  forM_ labelled $ \(button, command) -> Gtk.buttonSetLabel button (commandLabel wording command)
  Gio.menuRemoveAll menu
  fillMenu menu wording

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

presentShortcuts :: Adw.ApplicationWindow -> IORef Bool -> Wording -> IO ()
presentShortcuts window shortcutsOpen wording = do
  page <- new Adw.PreferencesPage []
  forM_ [minBound .. maxBound] (addGroup wording page)
  toolbar <- new Adw.ToolbarView [#content := page]
  new Adw.HeaderBar [] >>= Adw.toolbarViewAddTopBar toolbar
  dialog <-
    new
      Adw.Dialog
      [ #title := commandLabel wording Command.KeyboardShortcuts
      , #contentWidth := 460
      , #contentHeight := 640
      , #child := toolbar
      , On #closed (writeIORef shortcutsOpen False)
      ]
  writeIORef shortcutsOpen True
  Adw.dialogPresent dialog (Just window)

addGroup :: Wording -> Adw.PreferencesPage -> Section -> IO ()
addGroup wording page wanted = do
  let rows = filter (\command -> (commandSpec command).section == Just wanted) commands
  group <- new Adw.PreferencesGroup [#title := sectionLabel wording wanted]
  forM_ rows $ \command -> forM_ (listToMaybe (commandSpec command).accels) (addShortcut group (commandLabel wording command))
  when (wanted == GeneralSection) (addShortcut group (mainMenuLabel wording) "F10")
  Adw.preferencesPageAdd page group

addShortcut :: Adw.PreferencesGroup -> Text -> Text -> IO ()
addShortcut group title accel = do
  row <- new Adw.ActionRow [#title := title]
  newKeyCaps accel >>= Adw.actionRowAddSuffix row
  Adw.preferencesGroupAdd group row

presentAbout :: Adw.ApplicationWindow -> IO ()
presentAbout window = do
  dialog <-
    new
      Adw.AboutDialog
      [ #applicationName := "MediaCopy 3000"
      , #applicationIcon := "tech.floreal.MediaCopy3000-symbolic"
      , #version := T.pack (showVersion version)
      , #developerName := "Floréal Technologies"
      , #comments := "Verified media offload and ASC MHL verification"
      , #licenseType := Gtk.LicenseGpl30Only
      ]
  Adw.aboutDialogAddLegalSection
    dialog
    "Inter"
    (Just "Copyright © 2016 The Inter Project Authors")
    Gtk.LicenseCustom
    (Just "This font is licensed under the <a href=\"https://openfontlicense.org\">SIL Open Font License, Version 1.1</a>.")
  Adw.dialogPresent dialog (Just window)
