module MediaCopy.Gtk.Widgets.Preferences
  ( newPreferences
  ) where

import Control.Monad (void)
import Data.GI.Base (AttrOp (On, (:=)), new, on, set, unsafeCastTo)
import Data.GI.Base.BasicTypes (glibType)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (flatNamed, newLabel, suppressing, unlessSuppressed)
import MediaCopy.Interface.Theme (Appearance (..), Base (..), PaletteMode (..), Theme, ThemeSection (..), themeRowLabel, usesBase)
import MediaCopy.Model (UiMessage (..))

newPreferences :: Adw.Application -> Adw.ApplicationWindow -> Vector ThemeSection -> Vector ThemeSection -> (UiMessage -> IO ()) -> IO (Appearance -> IO ())
newPreferences app window lightSections darkSections dispatch = do
  dialog <- new Adw.PreferencesDialog [#title := "Preferences"]
  page <- new Adw.PreferencesPage [#title := "General", #iconName := "preferences-system-symbolic"]
  group <- new Adw.PreferencesGroup [#title := "Appearance", #description := "Applies to this run only"]
  rows <- newAppearanceRows lightSections darkSections
  Adw.preferencesGroupAdd group rows.baseRow
  Adw.preferencesGroupAdd group rows.lightRow
  Adw.preferencesGroupAdd group rows.darkRow
  Adw.preferencesPageAdd page group
  Adw.preferencesDialogAdd dialog page
  asDialog <- Adw.toDialog dialog
  reportChoices rows dispatch
  installPreferencesAction app window asDialog
  pure (paintAppearance rows)

data AppearanceRows = AppearanceRows
  { baseRow :: Adw.ComboRow
  , lightRow :: Adw.ActionRow
  , lightDrop :: Gtk.DropDown
  , lightThemes :: Vector Theme
  , darkRow :: Adw.ActionRow
  , darkDrop :: Gtk.DropDown
  , darkThemes :: Vector Theme
  , suppress :: IORef Bool
  }

newAppearanceRows :: Vector ThemeSection -> Vector ThemeSection -> IO AppearanceRows
newAppearanceRows lightSections darkSections = do
  suppress <- newIORef False
  baseNames <- Gtk.stringListNew (Just (V.toList (V.map (\value -> display value) baseValues)))
  baseRow <- new Adw.ComboRow [#title := "Base", #model := baseNames]
  (lightRow, lightDrop) <- newPaletteRow "Light palette" lightSections
  (darkRow, darkDrop) <- newPaletteRow "Dark palette" darkSections
  pure
    AppearanceRows
      { baseRow
      , lightRow
      , lightDrop
      , lightThemes = themeRows lightSections
      , darkRow
      , darkDrop
      , darkThemes = themeRows darkSections
      , suppress
      }

newPaletteRow :: Text -> Vector ThemeSection -> IO (Adw.ActionRow, Gtk.DropDown)
newPaletteRow title sections = do
  names <- themeModel sections
  headers <- themeHeaderFactory sections
  dropDown <- new Gtk.DropDown [#model := names, #headerFactory := headers, #valign := Gtk.AlignCenter]
  row <- new Adw.ActionRow [#title := title, #activatableWidget := dropDown]
  Adw.actionRowAddSuffix row dropDown
  pure (row, dropDown)

reportChoices :: AppearanceRows -> (UiMessage -> IO ()) -> IO ()
reportChoices rows dispatch = do
  void (on rows.baseRow (Adw.PropertyNotify #selected) (\_ -> chosen rows dispatch (Adw.comboRowGetSelected rows.baseRow) baseValues SetBase))
  void (on rows.lightDrop (Gtk.PropertyNotify #selected) (\_ -> chosen rows dispatch (Gtk.dropDownGetSelected rows.lightDrop) rows.lightThemes SetPalette))
  void (on rows.darkDrop (Gtk.PropertyNotify #selected) (\_ -> chosen rows dispatch (Gtk.dropDownGetSelected rows.darkDrop) rows.darkThemes SetPalette))

chosen :: AppearanceRows -> (UiMessage -> IO ()) -> IO Word32 -> Vector a -> (a -> UiMessage) -> IO ()
chosen rows dispatch selected values report = unlessSuppressed rows.suppress $ do
  index <- selected
  mapM_ (\value -> dispatch (report value)) (values V.!? fromIntegral index)

paintAppearance :: AppearanceRows -> Appearance -> IO ()
paintAppearance rows appearance = do
  select rows.suppress (Adw.comboRowSetSelected rows.baseRow) baseValues appearance.base
  set rows.lightRow [#sensitive := usesBase LightPalette appearance.base]
  set rows.darkRow [#sensitive := usesBase DarkPalette appearance.base]
  select rows.suppress (Gtk.dropDownSetSelected rows.lightDrop) rows.lightThemes appearance.light
  select rows.suppress (Gtk.dropDownSetSelected rows.darkDrop) rows.darkThemes appearance.dark

select :: (Eq a) => IORef Bool -> (Word32 -> IO ()) -> Vector a -> a -> IO ()
select suppress choose values wanted =
  mapM_ (\index -> suppressing suppress (choose (fromIntegral index))) (V.elemIndex wanted values)

installPreferencesAction :: Adw.Application -> Adw.ApplicationWindow -> Adw.Dialog -> IO ()
installPreferencesAction app window dialog = do
  action <-
    new
      Gio.SimpleAction
      [ #name := "preferences"
      , On #activate (\_param -> Adw.dialogPresent dialog (Just window))
      ]
  Gio.actionMapAddAction app action

baseValues :: Vector Base
baseValues = V.fromList [minBound .. maxBound]

themeRows :: Vector ThemeSection -> Vector Theme
themeRows sections = V.concatMap (\section -> section.themes) sections

themeModel :: Vector ThemeSection -> IO Gtk.FlattenListModel
themeModel sections = do
  modelType <- glibType @Gio.ListModel
  store <- Gio.listStoreNew modelType
  mapM_
    ( \section -> do
        labels <- Gtk.stringListNew (Just (V.toList (V.map (\theme -> themeRowLabel theme) section.themes)))
        Gio.listStoreAppend store labels
    )
    sections
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
  mapM_
    (\widget -> unsafeCastTo Gtk.Label widget >>= \label -> set label [#label := section.heading])
    firstChild
  mapM_
    (\widget -> unsafeCastTo Gtk.LinkButton widget >>= \link -> paintLink link section.homepage)
    lastChild

paintLink :: Gtk.LinkButton -> Maybe Text -> IO ()
paintLink link = \case
  Nothing -> set link [#visible := False]
  Just page -> set link [#uri := page, #tooltipText := page, #visible := True]

rowSections :: Vector ThemeSection -> Vector ThemeSection
rowSections sections =
  V.concatMap
    (\section -> V.replicate (V.length section.themes) section)
    sections
