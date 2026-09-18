-- | The Preferences dialog: the one place the operator changes how the window looks.
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
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (flatNamed, newLabel, suppressing, unlessSuppressed)
import MediaCopy.Interface.Theme (Appearance (..), Base (..), PaletteMode (..), Theme, ThemeSection (..), themeRowLabel, usesBase)
import MediaCopy.Model (UiMessage (..))

-- | Builds the dialog, installs @app.preferences@ to present it, and answers with the render that
-- paints an appearance into its three rows. The dialog owns whether it is open, because nothing in
-- the model depends on it.
newPreferences :: Adw.Application -> Adw.ApplicationWindow -> Vector ThemeSection -> Vector ThemeSection -> (UiMessage -> IO ()) -> IO (Appearance -> IO ())
newPreferences app window lightSections darkSections dispatch = do
  dialog <- new Adw.PreferencesDialog [#title := "Preferences"]
  page <- new Adw.PreferencesPage [#title := "General", #iconName := "preferences-system-symbolic"]
  -- The three rows share one sentence, because no one of them survives the run.
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

-- | The three dropdowns, and the values each of them stands for.
data AppearanceRows = AppearanceRows
  { baseRow :: Adw.ComboRow
  , lightRow :: Adw.ComboRow
  , lightThemes :: Vector Theme
  , darkRow :: Adw.ComboRow
  , darkThemes :: Vector Theme
  , suppress :: IORef Bool
  -- ^ True while a render writes `selected`, so that write alone never dispatches.
  }

newAppearanceRows :: Vector ThemeSection -> Vector ThemeSection -> IO AppearanceRows
newAppearanceRows lightSections darkSections = do
  suppress <- newIORef False
  baseNames <- Gtk.stringListNew (Just (V.toList (V.map (\value -> display value) baseValues)))
  baseRow <- new Adw.ComboRow [#title := "Base", #model := baseNames]
  lightNames <- themeModel lightSections
  lightHeaders <- themeHeaderFactory lightSections
  lightRow <- new Adw.ComboRow [#title := "Light palette", #model := lightNames, #headerFactory := lightHeaders]
  darkNames <- themeModel darkSections
  darkHeaders <- themeHeaderFactory darkSections
  darkRow <- new Adw.ComboRow [#title := "Dark palette", #model := darkNames, #headerFactory := darkHeaders]
  pure
    AppearanceRows
      { baseRow
      , lightRow
      , lightThemes = themeRows lightSections
      , darkRow
      , darkThemes = themeRows darkSections
      , suppress
      }

-- | Every row reports the value the operator picked, and nothing a render wrote.
reportChoices :: AppearanceRows -> (UiMessage -> IO ()) -> IO ()
reportChoices rows dispatch = do
  void (on rows.baseRow (Adw.PropertyNotify #selected) (\_ -> chosen rows dispatch rows.baseRow baseValues SetBase))
  void (on rows.lightRow (Adw.PropertyNotify #selected) (\_ -> chosen rows dispatch rows.lightRow rows.lightThemes SetPalette))
  void (on rows.darkRow (Adw.PropertyNotify #selected) (\_ -> chosen rows dispatch rows.darkRow rows.darkThemes SetPalette))

chosen :: AppearanceRows -> (UiMessage -> IO ()) -> Adw.ComboRow -> Vector a -> (a -> UiMessage) -> IO ()
chosen rows dispatch row values report = unlessSuppressed rows.suppress $ do
  index <- Adw.comboRowGetSelected row
  mapM_ (\value -> dispatch (report value)) (values V.!? fromIntegral index)

paintAppearance :: AppearanceRows -> Appearance -> IO ()
paintAppearance rows appearance = do
  select rows.suppress rows.baseRow baseValues appearance.base
  set rows.lightRow [#sensitive := usesBase LightPalette appearance.base]
  set rows.darkRow [#sensitive := usesBase DarkPalette appearance.base]
  select rows.suppress rows.lightRow rows.lightThemes appearance.light
  select rows.suppress rows.darkRow rows.darkThemes appearance.dark

-- | A row's index is its value's index in the vector the row was built from.
select :: (Eq a) => IORef Bool -> Adw.ComboRow -> Vector a -> a -> IO ()
select suppress row values wanted =
  mapM_ (\index -> suppressing suppress (set row [#selected := fromIntegral index])) (V.elemIndex wanted values)

installPreferencesAction :: Adw.Application -> Adw.ApplicationWindow -> Adw.Dialog -> IO ()
installPreferencesAction app window dialog = do
  action <-
    new
      Gio.SimpleAction
      [ #name := "preferences"
      , On #activate (\_param -> Adw.dialogPresent dialog (Just window))
      ]
  Gio.actionMapAddAction app action

-- | The rows follow the declaration order of 'Base', which the dropdown shows.
baseValues :: Vector Base
baseValues = V.fromList [minBound .. maxBound]

-- | The rows follow the sections, so a row's place in this vector is its place in the list.
themeRows :: Vector ThemeSection -> Vector Theme
themeRows sections = V.concatMap (\section -> section.themes) sections

-- | A flatten model is a section model, so the list draws a heading over each section.
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

-- | A heading's start index is a row index, which 'rowSections' answers.
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

-- | The label comes first and the link second, which 'paintHeading' counts on.
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

-- | A family that names no page of its own shows no link.
paintLink :: Gtk.LinkButton -> Maybe Text -> IO ()
paintLink link = \case
  Nothing -> set link [#visible := False]
  Just page -> set link [#uri := page, #tooltipText := page, #visible := True]

rowSections :: Vector ThemeSection -> Vector ThemeSection
rowSections sections =
  V.concatMap
    (\section -> V.replicate (V.length section.themes) section)
    sections
