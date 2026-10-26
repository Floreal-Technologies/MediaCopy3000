module MediaCopy.Gtk.Widgets.PluginsPage
  ( PluginsPage (..)
  , newPluginsPage
  ) where

import Control.Monad (zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.Vector qualified as V
import Floreal.Commons.Gtk.Bind (bind, switch, switchRow)
import Floreal.Commons.Gtk.Common (Cell, newCell)
import Floreal.Commons.Gtk.Signal (onSelf)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Field qualified as Field

import MediaCopy.Domain.FieldView (FieldView)
import MediaCopy.Domain.FieldView qualified as FieldView
import MediaCopy.Domain.PluginCatalogue (PluginCatalogue)
import MediaCopy.Domain.PluginCatalogue qualified as PluginCatalogue
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Gtk.Widgets.FieldRows (FieldActions (..), FieldRow (..), authorsRow, fieldRow)
import MediaCopy.Gtk.Widgets.Lists (choiceRow)
import MediaCopy.Interface.View.Plugins (answerText, answers, capabilitySubtitle, entrySubtitle)
import MediaCopy.Model (UiMessage (..))

data PluginsPage = PluginsPage
  { page :: Adw.PreferencesPage
  , cell :: Cell PluginCatalogue
  }

data Groups = Groups
  { dialog :: Adw.PreferencesDialog
  , installed :: Adw.PreferencesGroup
  , invalid :: Adw.PreferencesGroup
  , installedRows :: IORef [Adw.PreferencesRow]
  , invalidRows :: IORef [Adw.PreferencesRow]
  , open :: IORef (Maybe Subpage)
  , shown :: IORef [EntryRows]
  , current :: IORef (Maybe PluginCatalogue)
  }

type Layout = (Maybe Text, [(Text, [Plugin.Capability], [(Text, Text, Field.Kind, Bool, Int)])])

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
  , capabilityRows :: [(Adw.ComboRow, PluginCatalogue.Answer -> IO ())]
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
  cell <- newCell (renderCatalogue groups dispatch)
  pure PluginsPage {page, cell}

layoutOf :: PluginCatalogue -> Layout
layoutOf catalogue =
  ( catalogue.problem
  , [ (entry.plugin.id, [view.capability | view <- V.toList entry.capabilities], [(field.key, field.label, field.kind, field.required, V.length (FieldView.authorSlots field.value)) | field <- V.toList entry.settings])
    | entry <- V.toList catalogue.entries
    ]
  )

renderCatalogue :: Groups -> (UiMessage -> IO ()) -> PluginCatalogue -> IO ()
renderCatalogue groups dispatch catalogue = do
  previous <- readIORef groups.current
  writeIORef groups.current (Just catalogue)
  renderInvalid groups catalogue.rejected
  if fmap layoutOf previous == Just (layoutOf catalogue)
    then do
      rows <- readIORef groups.shown
      zipWithM_ refreshEntry rows (V.toList catalogue.entries)
      withOpenEntry groups catalogue $ \subpage entry -> refreshDetail subpage.detail entry
    else do
      rebuild groups dispatch catalogue
      withOpenEntry groups catalogue $ \subpage entry -> do
        forM_ subpage.detail.groups (Adw.preferencesPageRemove subpage.body)
        fresh <- fillDetail dispatch subpage.body entry
        writeIORef groups.open (Just subpage {detail = fresh})

withOpenEntry :: Groups -> PluginCatalogue -> (Subpage -> PluginCatalogue.Entry -> IO ()) -> IO ()
withOpenEntry groups catalogue act =
  readIORef groups.open >>= \case
    Nothing -> pure ()
    Just subpage -> case PluginCatalogue.entryById subpage.pluginId catalogue of
      Just entry -> act subpage entry
      Nothing -> do
        writeIORef groups.open Nothing
        void (Adw.preferencesDialogPopSubpage groups.dialog)

rebuild :: Groups -> (UiMessage -> IO ()) -> PluginCatalogue -> IO ()
rebuild groups dispatch catalogue = do
  readIORef groups.installedRows >>= mapM_ (Adw.preferencesGroupRemove groups.installed)
  problemRows <- traverse (\problem -> plainRow "plugins.json cannot be read" problem ["error"]) (maybe [] pure catalogue.problem)
  entries <- traverse (entryRows groups dispatch) (V.toList catalogue.entries)
  entryRowsShown <- traverse (\entry -> Adw.toPreferencesRow entry.row) entries
  emptyRows <-
    if V.null catalogue.entries
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

entryRows :: Groups -> (UiMessage -> IO ()) -> PluginCatalogue.Entry -> IO EntryRows
entryRows groups dispatch entry = do
  let pluginId = entry.plugin.id
  row <- new Adw.ActionRow [#useMarkup := False, #activatable := True]
  set row [#title := entry.plugin.name, #subtitle := entrySubtitle entry]
  enabledSwitch <- new Gtk.Switch [#valign := Gtk.AlignCenter, #active := entry.enabled]
  paintEnabled <- bind (switch enabledSwitch) (\flag -> flag & PluginCatalogue.SetEnabled & PluginCatalogue.Change pluginId & ChangePlugin & dispatch)
  chevron <- new Gtk.Image [#iconName := "go-next-symbolic"]
  Adw.actionRowAddSuffix row enabledSwitch
  Adw.actionRowAddSuffix row chevron
  void $ onSelf row #activated (openSubpage groups dispatch pluginId)
  pure EntryRows {row, paintEnabled}

refreshEntry :: EntryRows -> PluginCatalogue.Entry -> IO ()
refreshEntry rows entry = do
  Adw.preferencesRowSetTitle rows.row entry.plugin.name
  Adw.actionRowSetSubtitle rows.row (entrySubtitle entry)
  rows.paintEnabled entry.enabled

openSubpage :: Groups -> (UiMessage -> IO ()) -> Text -> IO ()
openSubpage groups dispatch pluginId = do
  open <- readIORef groups.open
  catalogue <- readIORef groups.current
  when (maybe True (\subpage -> subpage.pluginId /= pluginId) open) $
    forM_ (catalogue >>= PluginCatalogue.entryById pluginId) $ \entry -> do
      body <- new Adw.PreferencesPage []
      detail <- fillDetail dispatch body entry
      header <- new Adw.HeaderBar []
      toolbar <- new Adw.ToolbarView [#content := body]
      Adw.toolbarViewAddTopBar toolbar header
      navigation <- Adw.navigationPageNew toolbar entry.plugin.name
      void $ onSelf navigation #hidden $ do
        shown <- readIORef groups.open
        when (maybe False (\subpage -> subpage.pluginId == pluginId) shown) (writeIORef groups.open Nothing)
      writeIORef groups.open (Just Subpage {pluginId, body, detail})
      Adw.preferencesDialogPushSubpage groups.dialog navigation

fillDetail :: (UiMessage -> IO ()) -> Adw.PreferencesPage -> PluginCatalogue.Entry -> IO DetailRows
fillDetail dispatch body entry = do
  rows <- detailRows dispatch entry
  forM_ rows.groups (Adw.preferencesPageAdd body)
  pure rows

detailRows :: (UiMessage -> IO ()) -> PluginCatalogue.Entry -> IO DetailRows
detailRows dispatch entry = do
  let pluginId = entry.plugin.id
      change edit = edit & PluginCatalogue.Change pluginId & ChangePlugin & dispatch
  general <- new Adw.PreferencesGroup [#description := entry.description]
  folderRow <- new Adw.ActionRow [#useMarkup := False, #subtitleSelectable := True]
  set folderRow [#title := "Folder", #subtitle := entry.folder]
  openFolder <- new Gtk.Button [#label := "Open", #valign := Gtk.AlignCenter, On #clicked (dispatch (OpenPluginFolder entry.folder))]
  Adw.actionRowAddSuffix folderRow openFolder
  set folderRow [#activatableWidget := openFolder]
  Adw.preferencesGroupAdd general folderRow
  traceRow <- new Adw.SwitchRow [#useMarkup := False, #active := entry.trace]
  set traceRow [#title := "Trace Messages", #subtitle := "Writes each message to and from the plug-in to a file, for its developer"]
  paintTrace <- bind (switchRow traceRow) (change . PluginCatalogue.SetTrace)
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

refreshDetail :: DetailRows -> PluginCatalogue.Entry -> IO ()
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

capabilityRow :: (PluginCatalogue.Edit -> IO ()) -> PluginCatalogue.CapabilityView -> IO (Adw.ComboRow, PluginCatalogue.Answer -> IO ())
capabilityRow change view = do
  row <- new Adw.ComboRow [#useMarkup := False]
  set row [#title := display view.capability, #subtitle := capabilitySubtitle view]
  choices <- choiceRow answerText answers row
  paintAnswer <- bind choices (change . PluginCatalogue.SetAnswer view.capability)
  paintAnswer view.answer
  pure (row, paintAnswer)

settingRow :: (PluginCatalogue.Edit -> IO ()) -> (UiMessage -> IO ()) -> Text -> FieldView -> IO FieldRow
settingRow send dispatch pluginId field
  | field.kind == Field.AuthorsField =
      authorsRow (\slots -> if V.null slots then send (PluginCatalogue.ClearSetting field.key) else send (PluginCatalogue.SetSetting field.key (PluginCatalogue.SettingAuthors slots))) field
  | otherwise =
      fieldRow
        FieldActions
          { setText = \text -> text & PluginCatalogue.SettingText & PluginCatalogue.SetSetting field.key & send
          , setBool = \flag -> flag & PluginCatalogue.SettingBool & PluginCatalogue.SetSetting field.key & send
          , clear = send (PluginCatalogue.ClearSetting field.key)
          , pickPath = dispatch (PickPluginPath pluginId field.key)
          }
        ""
        field
