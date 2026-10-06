module MediaCopy.Gtk.Widgets.PluginsPage
  ( PluginsPage (..)
  , newPluginsPage
  ) where

import Control.Monad (forM, void, zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Set (Set)
import Data.Set qualified as Set
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
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), fieldRow, later)
import MediaCopy.Model (UiMessage (..))

data PluginsPage = PluginsPage
  { page :: Adw.PreferencesPage
  , cell :: Cell PluginCatalog
  }

data Groups = Groups
  { installed :: Adw.PreferencesGroup
  , invalid :: Adw.PreferencesGroup
  , installedRows :: IORef [Adw.PreferencesRow]
  , invalidRows :: IORef [Adw.PreferencesRow]
  , expanded :: IORef (Set Text)
  , shown :: IORef (Maybe (Layout, [EntryRows]))
  }

type Layout = (Maybe Text, [(Text, [Text], [(Text, Text, FieldShape, Bool)])])

data EntryRows = EntryRows
  { expander :: Adw.ExpanderRow
  , enabledRow :: Adw.SwitchRow
  , traceRow :: Adw.SwitchRow
  , capabilityRows :: [Adw.ComboRow]
  , settingRows :: [FieldRow]
  , suppress :: IORef Bool
  }

newPluginsPage :: (UiMessage -> IO ()) -> IO PluginsPage
newPluginsPage dispatch = do
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
  groups <- Groups installed invalid <$> newIORef [] <*> newIORef [] <*> newIORef Set.empty <*> newIORef Nothing
  cell <- newCell (renderCatalog groups dispatch)
  pure PluginsPage {page, cell}

layoutOf :: PluginCatalog -> Layout
layoutOf catalog =
  ( catalog.problem
  , [ (entry.plugin.id, [capability.name | capability <- V.toList entry.capabilities], [(field.key, field.label, field.shape, field.required) | field <- V.toList entry.settings])
    | entry <- V.toList catalog.entries
    ]
  )

renderCatalog :: Groups -> (UiMessage -> IO ()) -> PluginCatalog -> IO ()
renderCatalog groups dispatch catalog = do
  renderInvalid groups catalog.rejected
  previous <- readIORef groups.shown
  case previous of
    Just (layout, rows) | layout == layoutOf catalog -> zipWithM_ refreshEntry rows (V.toList catalog.entries)
    _ -> rebuild groups dispatch catalog

rebuild :: Groups -> (UiMessage -> IO ()) -> PluginCatalog -> IO ()
rebuild groups dispatch catalog = do
  readIORef groups.installedRows >>= mapM_ (Adw.preferencesGroupRemove groups.installed)
  open <- readIORef groups.expanded
  problemRows <- traverse (\problem -> plainRow "plugins.json cannot be read" problem ["error"]) (maybe [] pure catalog.problem)
  entries <- traverse (entryRows groups dispatch open) (V.toList catalog.entries)
  entryRowsShown <- traverse (\entry -> Adw.toPreferencesRow entry.expander) entries
  emptyRows <-
    if V.null catalog.entries
      then pure <$> plainRow "No plug-in is installed" "The manual says where to put a plug-in" []
      else pure []
  let rows = problemRows <> entryRowsShown <> emptyRows
  mapM_ (Adw.preferencesGroupAdd groups.installed) rows
  writeIORef groups.installedRows rows
  writeIORef groups.shown (Just (layoutOf catalog, entries))

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
subtitleOf entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList entry.roles) : maybe [] pure entry.problem)

entryRows :: Groups -> (UiMessage -> IO ()) -> Set Text -> PluginEntry -> IO EntryRows
entryRows groups dispatch open entry = do
  let pluginId = entry.plugin.id
      change = later . dispatch . ChangePlugin
  suppress <- newIORef False
  expander <- new Adw.ExpanderRow [#useMarkup := False, #expanded := Set.member pluginId open]
  set expander [#title := entry.plugin.name, #subtitle := subtitleOf entry]
  void $ on expander (Adw.PropertyNotify #expanded) $ \_ -> do
    now <- Adw.expanderRowGetExpanded expander
    modifyIORef' groups.expanded (if now then Set.insert pluginId else Set.delete pluginId)
  enabledRow <- new Adw.SwitchRow [#useMarkup := False, #active := entry.enabled]
  set enabledRow [#title := "Enabled", #subtitle := entry.folder]
  void $ on enabledRow (Adw.PropertyNotify #active) $ \_ ->
    unlessSuppressed suppress (Adw.switchRowGetActive enabledRow >>= change . SetEnabled pluginId)
  Adw.expanderRowAddRow expander enabledRow
  traceRow <- new Adw.SwitchRow [#useMarkup := False, #active := entry.trace]
  set traceRow [#title := "Trace Messages", #subtitle := "Writes each message to and from the plug-in to a file, for its developer"]
  void $ on traceRow (Adw.PropertyNotify #active) $ \_ ->
    unlessSuppressed suppress (Adw.switchRowGetActive traceRow >>= change . SetTrace pluginId)
  Adw.expanderRowAddRow expander traceRow
  capabilityRows <- forM (V.toList entry.capabilities) $ \capability -> do
    row <- capabilityRow suppress change pluginId capability
    Adw.expanderRowAddRow expander row
    pure row
  settingRows <- forM (V.toList entry.settings) $ \field -> do
    row <- settingRow (dispatch . ChangePlugin) dispatch pluginId field
    Adw.expanderRowAddRow expander row.row
    pure row
  pure EntryRows {expander, enabledRow, traceRow, capabilityRows, settingRows, suppress}

refreshEntry :: EntryRows -> PluginEntry -> IO ()
refreshEntry rows entry = do
  Adw.preferencesRowSetTitle rows.expander entry.plugin.name
  Adw.expanderRowSetSubtitle rows.expander (subtitleOf entry)
  Adw.actionRowSetSubtitle rows.enabledRow entry.folder
  suppressing rows.suppress $ do
    Adw.switchRowSetActive rows.enabledRow entry.enabled
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
  "block" -> "Stop a job with a blocker"
  "manifest.write" -> "Add data to the manifest"
  other -> other

settingRow :: (CatalogChange -> IO ()) -> (UiMessage -> IO ()) -> Text -> FieldView -> IO FieldRow
settingRow send dispatch pluginId field =
  fieldRow
    FieldActions
      { setText = send . SetSetting pluginId field.key . SettingText
      , setBool = send . SetSetting pluginId field.key . SettingBool
      , clear = send (ClearSetting pluginId field.key)
      , pickPath = dispatch (PickPluginPath pluginId field.key)
      }
    ""
    field
