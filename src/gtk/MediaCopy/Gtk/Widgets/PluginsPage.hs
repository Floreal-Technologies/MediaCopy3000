module MediaCopy.Gtk.Widgets.PluginsPage
  ( PluginsPage (..)
  , newPluginsPage
  ) where

import Control.Monad (forM, forM_, void, when, zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import MediaCopy.Plugin.Manifest (Capability, FieldKind (..), capabilityName)

import MediaCopy.Domain.Plugin (PluginRef (..))
import MediaCopy.Domain.PluginCatalog
import MediaCopy.Gtk.Widgets.Bind (bind, comboRow, switch, switchRow)
import MediaCopy.Gtk.Widgets.Common (Cell, newCell)
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), authorsRow, fieldRow)
import MediaCopy.Interface.View.Plugins (answerAt, answerIndex, capabilitySubtitle, entrySubtitle)
import MediaCopy.Model (UiMessage (..))

data PluginsPage = PluginsPage
  { page :: Adw.PreferencesPage
  , cell :: Cell PluginCatalog
  }

data Groups = Groups
  { dialog :: Adw.PreferencesDialog
  , installed :: Adw.PreferencesGroup
  , invalid :: Adw.PreferencesGroup
  , installedRows :: IORef [Adw.PreferencesRow]
  , invalidRows :: IORef [Adw.PreferencesRow]
  , open :: IORef (Maybe Subpage)
  , shown :: IORef [EntryRows]
  , current :: IORef (Maybe PluginCatalog)
  }

type Layout = (Maybe Text, [(Text, [Capability], [(Text, Text, FieldKind, Bool, Int)])])

data EntryRows = EntryRows
  { row :: Adw.ActionRow
  , paintEnabled :: Bool -> IO ()
  }

data Subpage = Subpage
  { pluginId :: Text
  , body :: Adw.PreferencesPage
  , detail :: DetailRows
  }

data DetailRows = DetailRows
  { groups :: [Adw.PreferencesGroup]
  , paintTrace :: Bool -> IO ()
  , capabilityRows :: [(Adw.ComboRow, Answer -> IO ())]
  , settingRows :: [FieldRow]
  }

newPluginsPage :: Adw.PreferencesDialog -> (UiMessage -> IO ()) -> IO PluginsPage
newPluginsPage dialog dispatch = do
  page <- new Adw.PreferencesPage [#name := "plugins", #title := "Plug-ins", #iconName := "system-run-symbolic"]
  installed <-
    new
      Adw.PreferencesGroup
      [ #title := "Installed"
      , #description := "A plug-in runs only when it is enabled and each capability that it asks for is granted or declined"
      ]
  lookAgain <- new Gtk.Button [#label := "Look Again", #valign := Gtk.AlignCenter, On #clicked (dispatch ReloadPlugins)]
  Gtk.widgetAddCssClass lookAgain "flat"
  Adw.preferencesGroupSetHeaderSuffix installed (Just lookAgain)
  invalid <- new Adw.PreferencesGroup [#title := "Not Valid", #visible := False]
  Adw.preferencesPageAdd page installed
  Adw.preferencesPageAdd page invalid
  groups <- Groups dialog installed invalid <$> newIORef [] <*> newIORef [] <*> newIORef Nothing <*> newIORef [] <*> newIORef Nothing
  cell <- newCell (renderCatalog groups dispatch)
  pure PluginsPage {page, cell}

layoutOf :: PluginCatalog -> Layout
layoutOf catalog =
  ( catalog.problem
  , [ (entry.plugin.id, [view.capability | view <- V.toList entry.capabilities], [(field.key, field.label, field.kind, field.required, V.length (authorSlots field.value)) | field <- V.toList entry.settings])
    | entry <- V.toList catalog.entries
    ]
  )

renderCatalog :: Groups -> (UiMessage -> IO ()) -> PluginCatalog -> IO ()
renderCatalog groups dispatch catalog = do
  previous <- readIORef groups.current
  writeIORef groups.current (Just catalog)
  renderInvalid groups catalog.rejected
  if fmap layoutOf previous == Just (layoutOf catalog)
    then do
      rows <- readIORef groups.shown
      zipWithM_ refreshEntry rows (V.toList catalog.entries)
      withOpenEntry groups catalog $ \subpage entry -> refreshDetail subpage.detail entry
    else do
      rebuild groups dispatch catalog
      withOpenEntry groups catalog $ \subpage entry -> do
        forM_ subpage.detail.groups (Adw.preferencesPageRemove subpage.body)
        fresh <- fillDetail dispatch subpage.body entry
        writeIORef groups.open (Just subpage {detail = fresh})

withOpenEntry :: Groups -> PluginCatalog -> (Subpage -> PluginEntry -> IO ()) -> IO ()
withOpenEntry groups catalog act =
  readIORef groups.open >>= \case
    Nothing -> pure ()
    Just subpage -> case entryById subpage.pluginId catalog of
      Just entry -> act subpage entry
      Nothing -> do
        writeIORef groups.open Nothing
        void (Adw.preferencesDialogPopSubpage groups.dialog)

rebuild :: Groups -> (UiMessage -> IO ()) -> PluginCatalog -> IO ()
rebuild groups dispatch catalog = do
  readIORef groups.installedRows >>= mapM_ (Adw.preferencesGroupRemove groups.installed)
  problemRows <- traverse (\problem -> plainRow "plugins.json cannot be read" problem ["error"]) (maybe [] pure catalog.problem)
  entries <- traverse (entryRows groups dispatch) (V.toList catalog.entries)
  entryRowsShown <- traverse (\entry -> Adw.toPreferencesRow entry.row) entries
  emptyRows <-
    if V.null catalog.entries
      then pure <$> plainRow "No plug-in is installed" "The manual says where to put a plug-in" []
      else pure []
  let rows = problemRows <> entryRowsShown <> emptyRows
  forM_ rows (Adw.preferencesGroupAdd groups.installed)
  writeIORef groups.installedRows rows
  writeIORef groups.shown entries

renderInvalid :: Groups -> Vector (Text, Text) -> IO ()
renderInvalid groups rejected = do
  readIORef groups.invalidRows >>= mapM_ (Adw.preferencesGroupRemove groups.invalid)
  invalidRows <- traverse (\(folder, reason) -> plainRow folder reason ["warning"]) (V.toList rejected)
  forM_ invalidRows (Adw.preferencesGroupAdd groups.invalid)
  writeIORef groups.invalidRows invalidRows
  Gtk.widgetSetVisible groups.invalid (not (null invalidRows))

plainRow :: Text -> Text -> [Text] -> IO Adw.PreferencesRow
plainRow title subtitle classes = do
  row <- new Adw.ActionRow [#useMarkup := False, #titleSelectable := True]
  set row [#title := title, #subtitle := subtitle]
  forM_ classes (Gtk.widgetAddCssClass row)
  Adw.toPreferencesRow row

entryRows :: Groups -> (UiMessage -> IO ()) -> PluginEntry -> IO EntryRows
entryRows groups dispatch entry = do
  let pluginId = entry.plugin.id
  row <- new Adw.ActionRow [#useMarkup := False, #activatable := True]
  set row [#title := entry.plugin.name, #subtitle := entrySubtitle entry]
  enabledSwitch <- new Gtk.Switch [#valign := Gtk.AlignCenter, #active := entry.enabled]
  paintEnabled <- bind (switch enabledSwitch) (dispatch . ChangePlugin . CatalogChange pluginId . SetEnabled)
  chevron <- new Gtk.Image [#iconName := "go-next-symbolic"]
  Adw.actionRowAddSuffix row enabledSwitch
  Adw.actionRowAddSuffix row chevron
  void $ on row #activated (openSubpage groups dispatch pluginId)
  pure EntryRows {row, paintEnabled}

refreshEntry :: EntryRows -> PluginEntry -> IO ()
refreshEntry rows entry = do
  Adw.preferencesRowSetTitle rows.row entry.plugin.name
  Adw.actionRowSetSubtitle rows.row (entrySubtitle entry)
  rows.paintEnabled entry.enabled

openSubpage :: Groups -> (UiMessage -> IO ()) -> Text -> IO ()
openSubpage groups dispatch pluginId = do
  open <- readIORef groups.open
  catalog <- readIORef groups.current
  when (maybe True (\subpage -> subpage.pluginId /= pluginId) open) $
    forM_ (catalog >>= entryById pluginId) $ \entry -> do
      body <- new Adw.PreferencesPage []
      detail <- fillDetail dispatch body entry
      header <- new Adw.HeaderBar []
      toolbar <- new Adw.ToolbarView [#content := body]
      Adw.toolbarViewAddTopBar toolbar header
      navigation <- Adw.navigationPageNew toolbar entry.plugin.name
      void $ on navigation #hidden $ do
        shown <- readIORef groups.open
        when (maybe False (\subpage -> subpage.pluginId == pluginId) shown) (writeIORef groups.open Nothing)
      writeIORef groups.open (Just Subpage {pluginId, body, detail})
      Adw.preferencesDialogPushSubpage groups.dialog navigation

fillDetail :: (UiMessage -> IO ()) -> Adw.PreferencesPage -> PluginEntry -> IO DetailRows
fillDetail dispatch body entry = do
  rows <- detailRows dispatch entry
  forM_ rows.groups (Adw.preferencesPageAdd body)
  pure rows

detailRows :: (UiMessage -> IO ()) -> PluginEntry -> IO DetailRows
detailRows dispatch entry = do
  let pluginId = entry.plugin.id
      change = dispatch . ChangePlugin . CatalogChange pluginId
  general <- new Adw.PreferencesGroup [#description := entry.description]
  folderRow <- new Adw.ActionRow [#useMarkup := False, #subtitleSelectable := True]
  set folderRow [#title := "Folder", #subtitle := entry.folder]
  openFolder <- new Gtk.Button [#label := "Open", #valign := Gtk.AlignCenter, On #clicked (dispatch (OpenPluginFolder entry.folder))]
  Adw.actionRowAddSuffix folderRow openFolder
  set folderRow [#activatableWidget := openFolder]
  Adw.preferencesGroupAdd general folderRow
  traceRow <- new Adw.SwitchRow [#useMarkup := False, #active := entry.trace]
  set traceRow [#title := "Trace Messages", #subtitle := "Writes each message to and from the plug-in to a file, for its developer"]
  paintTrace <- bind (switchRow traceRow) (change . SetTrace)
  Adw.preferencesGroupAdd general traceRow
  permissions <-
    new
      Adw.PreferencesGroup
      [ #title := "Permissions"
      , #description := "Each capability that the plug-in asks for is granted or declined"
      ]
  capabilityRows <- forM (V.toList entry.capabilities) $ \capability -> do
    shown <- capabilityRow change capability
    Adw.preferencesGroupAdd permissions (fst shown)
    pure shown
  settings <- new Adw.PreferencesGroup [#title := "Settings", #visible := not (V.null entry.settings)]
  settingRows <- forM (V.toList entry.settings) $ \field -> do
    row <- settingRow change dispatch pluginId field
    forM_ row.rows (Adw.preferencesGroupAdd settings)
    pure row
  pure DetailRows {groups = [general, permissions, settings], paintTrace, capabilityRows, settingRows}

refreshDetail :: DetailRows -> PluginEntry -> IO ()
refreshDetail rows entry = do
  rows.paintTrace entry.trace
  zipWithM_
    ( \(row, paintAnswer) capability -> do
        paintAnswer capability.answer
        Adw.actionRowSetSubtitle row (capabilitySubtitle capability)
    )
    rows.capabilityRows
    (V.toList entry.capabilities)
  zipWithM_ (\row field -> row.refresh field.value) rows.settingRows (V.toList entry.settings)

capabilityRow :: (Edit -> IO ()) -> CapabilityView -> IO (Adw.ComboRow, Answer -> IO ())
capabilityRow change view = do
  choices <- Gtk.stringListNew (Just ["Not answered", "Granted", "Declined"])
  row <-
    new
      Adw.ComboRow
      [ #useMarkup := False
      , #model := choices
      , #selected := answerIndex view.answer
      ]
  set row [#title := capabilityName view.capability, #subtitle := capabilitySubtitle view]
  paintIndex <- bind (comboRow row) (mapM_ (change . SetAnswer view.capability) . answerAt)
  pure (row, paintIndex . answerIndex)

settingRow :: (Edit -> IO ()) -> (UiMessage -> IO ()) -> Text -> FieldView -> IO FieldRow
settingRow send dispatch pluginId field
  | field.kind == AuthorsField =
      authorsRow (\slots -> if V.null slots then send (ClearSetting field.key) else send (SetSetting field.key (SettingAuthors slots))) "" field
  | otherwise =
      fieldRow
        FieldActions
          { setText = send . SetSetting field.key . SettingText
          , setBool = send . SetSetting field.key . SettingBool
          , clear = send (ClearSetting field.key)
          , pickPath = dispatch (PickPluginPath pluginId field.key)
          }
        ""
        field
