module MediaCopy.Gtk.Widgets.Preferences
  ( Preferences (..)
  , newPreferences
  ) where

import Control.Monad (forM_)
import Data.GI.Base (AttrOp (On, (:=)), new, set, unsafeCastTo)
import Data.GI.Base.BasicTypes (glibType)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Bind (bind, comboRow, dropDown)
import MediaCopy.Gtk.Widgets.Common (flatNamed, newLabel, renderCell)
import MediaCopy.Gtk.Widgets.PluginsPage (PluginsPage (..), newPluginsPage)
import MediaCopy.Interface.Theme
import MediaCopy.Interface.Translation
import MediaCopy.Model (Model (..), PreferencesPage (..), UiMessage (..))

data Preferences = Preferences
  { render :: Model -> IO ()
  , showPage :: PreferencesPage -> IO ()
  }

newPreferences
  :: Adw.ApplicationWindow
  -> Wording
  -> Vector ThemeSection
  -> Vector ThemeSection
  -> (UiMessage -> IO ())
  -> IO Preferences
newPreferences window wording lightSections darkSections dispatch = do
  dialog <- new Adw.PreferencesDialog [#title := "Preferences"]
  page <- new Adw.PreferencesPage [#name := "general", #title := "General", #iconName := "preferences-system-symbolic"]
  group <- new Adw.PreferencesGroup [#title := "Appearance", #description := "Applies to this run only"]
  rows <- newAppearanceRows wording lightSections darkSections dispatch
  Adw.preferencesGroupAdd group rows.baseRow
  Adw.preferencesGroupAdd group rows.lightRow
  Adw.preferencesGroupAdd group rows.darkRow
  Adw.preferencesPageAdd page group
  Adw.preferencesDialogAdd dialog page
  plugins <- newPluginsPage dialog dispatch
  Adw.preferencesDialogAdd dialog plugins.page
  pure
    Preferences
      { render = \model -> do
          paintAppearance rows model.appearance
          renderCell plugins.cell model.plugins
      , showPage = \wanted -> do
          Adw.preferencesDialogSetVisiblePageName dialog (pageName wanted)
          Adw.dialogPresent dialog (Just window)
      }

pageName :: PreferencesPage -> Text
pageName = \case
  GeneralPreferences -> "general"
  PluginPreferences -> "plugins"

data AppearanceRows = AppearanceRows
  { lightRow :: Adw.ActionRow
  , lightThemes :: Vector Theme
  , darkRow :: Adw.ActionRow
  , darkThemes :: Vector Theme
  , baseRow :: Adw.ComboRow
  , paintBase :: Word32 -> IO ()
  , paintLight :: Word32 -> IO ()
  , paintDark :: Word32 -> IO ()
  }

newAppearanceRows :: Wording -> Vector ThemeSection -> Vector ThemeSection -> (UiMessage -> IO ()) -> IO AppearanceRows
newAppearanceRows wording lightSections darkSections dispatch = do
  baseNames <- Gtk.stringListNew (Just (V.toList (V.map (displayBase wording) baseValues)))
  baseRow <- new Adw.ComboRow [#title := "Base", #model := baseNames]
  (lightRow, lightDrop) <- newPaletteRow wording "Light palette" lightSections
  (darkRow, darkDrop) <- newPaletteRow wording "Dark palette" darkSections
  let lightThemes = themeRows lightSections
      darkThemes = themeRows darkSections
  paintBase <- bind (comboRow baseRow) (chooseFrom dispatch baseValues SetBase)
  paintLight <- bind (dropDown lightDrop) (chooseFrom dispatch lightThemes SetPalette)
  paintDark <- bind (dropDown darkDrop) (chooseFrom dispatch darkThemes SetPalette)
  pure AppearanceRows {lightRow, lightThemes, darkRow, darkThemes, baseRow, paintBase, paintLight, paintDark}

newPaletteRow :: Wording -> Text -> Vector ThemeSection -> IO (Adw.ActionRow, Gtk.DropDown)
newPaletteRow wording title sections = do
  names <- themeModel wording sections
  headers <- themeHeaderFactory sections
  picker <- new Gtk.DropDown [#model := names, #headerFactory := headers, #valign := Gtk.AlignCenter]
  row <- new Adw.ActionRow [#title := title, #activatableWidget := picker]
  Adw.actionRowAddSuffix row picker
  pure (row, picker)

chooseFrom :: (UiMessage -> IO ()) -> Vector a -> (a -> UiMessage) -> Word32 -> IO ()
chooseFrom dispatch values report index = forM_ (values V.!? fromIntegral index) (dispatch . report)

paintAppearance :: AppearanceRows -> Appearance -> IO ()
paintAppearance rows appearance = do
  forM_ (V.elemIndex appearance.base baseValues) (rows.paintBase . fromIntegral)
  set rows.lightRow [#sensitive := usesBase LightPalette appearance.base]
  set rows.darkRow [#sensitive := usesBase DarkPalette appearance.base]
  forM_ (V.elemIndex appearance.light rows.lightThemes) (rows.paintLight . fromIntegral)
  forM_ (V.elemIndex appearance.dark rows.darkThemes) (rows.paintDark . fromIntegral)

baseValues :: Vector Base
baseValues = V.fromList [minBound .. maxBound]

themeRows :: Vector ThemeSection -> Vector Theme
themeRows sections = V.concatMap (.themes) sections

themeModel :: Wording -> Vector ThemeSection -> IO Gtk.FlattenListModel
themeModel wording sections = do
  modelType <- glibType @Gio.ListModel
  store <- Gio.listStoreNew modelType
  forM_
    sections
    ( \section -> do
        labels <- Gtk.stringListNew (Just (V.toList (V.map (themeRowLabel wording) section.themes)))
        Gio.listStoreAppend store labels
    )
  Gtk.flattenListModelNew (Just store)

themeHeaderFactory :: Vector ThemeSection -> IO Gtk.SignalListItemFactory
themeHeaderFactory sections =
  new
    Gtk.SignalListItemFactory
    [ On #setup $ \object -> do
        header <- unsafeCastTo Gtk.ListHeader object
        box <- newHeadingBox
        Gtk.listHeaderSetChild header (Just box)
    , On #bind $ \object -> do
        header <- unsafeCastTo Gtk.ListHeader object
        start <- Gtk.listHeaderGetStart header
        child <- Gtk.listHeaderGetChild header
        case (child, rowSections sections V.!? fromIntegral start) of
          (Just widget, Just section) -> paintHeading widget section
          _ -> pure ()
    ]

newHeadingBox :: IO Gtk.Box
newHeadingBox = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  label <- newLabel "" [#xalign := 0] ["heading"]
  icon <- new Gtk.Image [#iconName := "adw-external-link-symbolic"]
  link <- new Gtk.LinkButton [#uri := "", #child := icon, #visible := False, #valign := Gtk.AlignCenter]
  flatNamed link "Open the theme's homepage"
  Gtk.boxAppend box label
  Gtk.boxAppend box link
  pure box

paintHeading :: Gtk.Widget -> ThemeSection -> IO ()
paintHeading box section = do
  firstChild <- Gtk.widgetGetFirstChild box
  lastChild <- Gtk.widgetGetLastChild box
  forM_ firstChild (\widget -> unsafeCastTo Gtk.Label widget >>= \label -> set label [#label := section.heading])
  forM_ lastChild (\widget -> unsafeCastTo Gtk.LinkButton widget >>= \link -> paintLink link section.homepage)

paintLink :: Gtk.LinkButton -> Maybe Text -> IO ()
paintLink link = \case
  Nothing -> set link [#visible := False]
  Just page -> set link [#uri := page, #tooltipText := page, #visible := True]

rowSections :: Vector ThemeSection -> Vector ThemeSection
rowSections sections =
  V.concatMap
    (\section -> V.replicate (V.length section.themes) section)
    sections
