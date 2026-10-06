module MediaCopy.Gtk.Widgets.PluginsPage
  ( PluginsPage (..)
  , newPluginsPage
  ) where

import Control.Monad (forM, forM_, void, when, zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Plugin (PluginRef (..))
import MediaCopy.Domain.PluginCatalog
import MediaCopy.Gtk.Widgets.Common (Cell, newCell, suppressing, unlessSuppressed)
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), authorsRow, fieldRow, later)
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

type Layout = (Maybe Text, [(Text, [Text], [(Text, Text, FieldShape, Bool, Int)])])

data EntryRows = EntryRows
  { row :: Adw.ActionRow
  , enabledSwitch :: Gtk.Switch
  , suppress :: IORef Bool
  }

data Subpage = Subpage
  { pluginId :: Text
  , body :: Adw.PreferencesPage
  , detail :: DetailRows
  }

data DetailRows = DetailRows
  { groups :: [Adw.PreferencesGroup]
  , traceRow :: Adw.SwitchRow
  , capabilityRows :: [Adw.ComboRow]
  , settingRows :: [FieldRow]
  , suppress :: IORef Bool
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
  , [ (entry.plugin.id, [capability.name | capability <- V.toList entry.capabilities], [(field.key, field.label, field.shape, field.required, V.length (authorSlots field.value)) | field <- V.toList entry.settings])
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
        mapM_ (Adw.preferencesPageRemove subpage.body) subpage.detail.groups
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
  mapM_ (Adw.preferencesGroupAdd groups.installed) rows
  writeIORef groups.installedRows rows
  writeIORef groups.shown entries

renderInvalid :: Groups -> Vector (Text, Text) -> IO ()
renderInvalid groups rejected = do
  readIORef groups.invalidRows >>= mapM_ (Adw.preferencesGroupRemove groups.invalid)
  invalidRows <- traverse (\(folder, reason) -> plainRow folder reason ["warning"]) (V.toList rejected)
  mapM_ (Adw.preferencesGroupAdd groups.invalid) invalidRows
  writeIORef groups.invalidRows invalidRows
  Gtk.widgetSetVisible groups.invalid (not (null invalidRows))

plainRow :: Text -> Text -> [Text] -> IO Adw.PreferencesRow
plainRow title subtitle classes = do
  row <- new Adw.ActionRow [#useMarkup := False, #titleSelectable := True]
  set row [#title := title, #subtitle := subtitle]
  mapM_ (Gtk.widgetAddCssClass row) classes
  Adw.toPreferencesRow row

subtitleOf :: PluginEntry -> Text
subtitleOf entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList (V.map (.name) entry.capabilities)) : maybe [] pure entry.problem)

entryRows :: Groups -> (UiMessage -> IO ()) -> PluginEntry -> IO EntryRows
entryRows groups dispatch entry = do
  let pluginId = entry.plugin.id
  suppress <- newIORef False
  row <- new Adw.ActionRow [#useMarkup := False, #activatable := True]
  set row [#title := entry.plugin.name, #subtitle := subtitleOf entry]
  enabledSwitch <- new Gtk.Switch [#valign := Gtk.AlignCenter, #active := entry.enabled]
  void $ on enabledSwitch (Gtk.PropertyNotify #active) $ \_ ->
    unlessSuppressed suppress (Gtk.switchGetActive enabledSwitch >>= later . dispatch . ChangePlugin . SetEnabled pluginId)
  chevron <- new Gtk.Image [#iconName := "go-next-symbolic"]
  Adw.actionRowAddSuffix row enabledSwitch
  Adw.actionRowAddSuffix row chevron
  void $ on row #activated (openSubpage groups dispatch pluginId)
  pure EntryRows {row, enabledSwitch, suppress}

refreshEntry :: EntryRows -> PluginEntry -> IO ()
refreshEntry rows entry = do
  Adw.preferencesRowSetTitle rows.row entry.plugin.name
  Adw.actionRowSetSubtitle rows.row (subtitleOf entry)
  suppressing rows.suppress (Gtk.switchSetActive rows.enabledSwitch entry.enabled)

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
  mapM_ (Adw.preferencesPageAdd body) rows.groups
  pure rows

detailRows :: (UiMessage -> IO ()) -> PluginEntry -> IO DetailRows
detailRows dispatch entry = do
  let pluginId = entry.plugin.id
      change = later . dispatch . ChangePlugin
  suppress <- newIORef False
  general <- new Adw.PreferencesGroup [#description := entry.description]
  folderRow <- new Adw.ActionRow [#useMarkup := False, #subtitleSelectable := True]
  set folderRow [#title := "Folder", #subtitle := entry.folder]
  openFolder <- new Gtk.Button [#label := "Open", #valign := Gtk.AlignCenter, On #clicked (dispatch (OpenPluginFolder entry.folder))]
  Adw.actionRowAddSuffix folderRow openFolder
  set folderRow [#activatableWidget := openFolder]
  Adw.preferencesGroupAdd general folderRow
  traceRow <- new Adw.SwitchRow [#useMarkup := False, #active := entry.trace]
  set traceRow [#title := "Trace Messages", #subtitle := "Writes each message to and from the plug-in to a file, for its developer"]
  void $ on traceRow (Adw.PropertyNotify #active) $ \_ ->
    unlessSuppressed suppress (Adw.switchRowGetActive traceRow >>= change . SetTrace pluginId)
  Adw.preferencesGroupAdd general traceRow
  permissions <-
    new
      Adw.PreferencesGroup
      [ #title := "Permissions"
      , #description := "Each capability that the plug-in asks for is granted or declined"
      ]
  capabilityRows <- forM (V.toList entry.capabilities) $ \capability -> do
    row <- capabilityRow suppress change pluginId capability
    Adw.preferencesGroupAdd permissions row
    pure row
  settings <- new Adw.PreferencesGroup [#title := "Settings", #visible := not (V.null entry.settings)]
  settingRows <- forM (V.toList entry.settings) $ \field -> do
    row <- settingRow (dispatch . ChangePlugin) dispatch pluginId field
    mapM_ (Adw.preferencesGroupAdd settings) row.rows
    pure row
  pure DetailRows {groups = [general, permissions, settings], traceRow, capabilityRows, settingRows, suppress}

refreshDetail :: DetailRows -> PluginEntry -> IO ()
refreshDetail rows entry = do
  suppressing rows.suppress $ do
    Adw.switchRowSetActive rows.traceRow entry.trace
    zipWithM_
      ( \row capability -> do
          Adw.comboRowSetSelected row (answerIndex capability.answer)
          Adw.actionRowSetSubtitle row (capabilitySubtitle capability)
      )
      rows.capabilityRows
      (V.toList entry.capabilities)
  zipWithM_ (\row field -> row.refresh field.value) rows.settingRows (V.toList entry.settings)

capabilityRow :: IORef Bool -> (CatalogChange -> IO ()) -> Text -> CapabilityView -> IO Adw.ComboRow
capabilityRow suppress change pluginId capability = do
  choices <- Gtk.stringListNew (Just ["Not answered", "Granted", "Declined"])
  row <-
    new
      Adw.ComboRow
      [ #useMarkup := False
      , #model := choices
      , #selected := answerIndex capability.answer
      ]
  set row [#title := capability.name, #subtitle := capabilitySubtitle capability]
  void $ on row (Adw.PropertyNotify #selected) $ \_ ->
    unlessSuppressed suppress $
      Adw.comboRowGetSelected row >>= \case
        0 -> change (SetAnswer pluginId capability.name Unanswered)
        1 -> change (SetAnswer pluginId capability.name Granted)
        2 -> change (SetAnswer pluginId capability.name Declined)
        _ -> pure ()
  pure row

capabilitySubtitle :: CapabilityView -> Text
capabilitySubtitle capability = capabilityText capability.name <> (if capability.answer == Unanswered then " – not answered yet" else "")

answerIndex :: Answer -> Word32
answerIndex = \case
  Unanswered -> 0
  Granted -> 1
  Declined -> 2

capabilityText :: Text -> Text
capabilityText = \case
  "files.read" -> "Read the media files"
  "plan.inspect" -> "Add findings to the plan"
  "files.inspect" -> "Add notes about each verified file to the report"
  "block" -> "Stop a job with a blocker"
  "manifest.write" -> "Add data to the manifest"
  other -> other

settingRow :: (CatalogChange -> IO ()) -> (UiMessage -> IO ()) -> Text -> FieldView -> IO FieldRow
settingRow send dispatch pluginId field
  | field.shape == AuthorsShape =
      authorsRow (\slots -> if V.null slots then send (ClearSetting pluginId field.key) else send (SetSetting pluginId field.key (SettingAuthors slots))) "" field
  | otherwise =
      fieldRow
        FieldActions
          { setText = send . SetSetting pluginId field.key . SettingText
          , setBool = send . SetSetting pluginId field.key . SettingBool
          , clear = send (ClearSetting pluginId field.key)
          , pickPath = dispatch (PickPluginPath pluginId field.key)
          }
        ""
        field
