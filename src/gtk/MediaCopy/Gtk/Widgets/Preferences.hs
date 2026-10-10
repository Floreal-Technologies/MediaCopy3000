module MediaCopy.Gtk.Widgets.Preferences
  ( Preferences (..)
  , newPreferences
  ) where

import Control.Monad (forM, forM_)
import Data.GI.Base (AttrOp ((:=)), new)
import Data.List (List)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Bind (bind, comboRow, toggleRadio)
import MediaCopy.Gtk.Widgets.Common (nameAccessible, renderCell)
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
  -> (UiMessage -> IO ())
  -> IO Preferences
newPreferences window wording dispatch = do
  dialog <- new Adw.PreferencesDialog [#title := "Preferences"]
  page <- new Adw.PreferencesPage [#name := "general", #title := "General", #iconName := "preferences-system-symbolic"]
  group <- new Adw.PreferencesGroup [#title := "Appearance", #description := "Applies to this run only"]
  rows <- newAppearanceRows wording dispatch
  Adw.preferencesGroupAdd group rows.baseRow
  Adw.preferencesGroupAdd group rows.accentRow
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
  { baseRow :: Adw.ComboRow
  , accentRow :: Adw.ActionRow
  , paintBase :: Word32 -> IO ()
  , paintAccent :: Maybe (Maybe Accent) -> IO ()
  }

newAppearanceRows :: Wording -> (UiMessage -> IO ()) -> IO AppearanceRows
newAppearanceRows wording dispatch = do
  baseNames <- Gtk.stringListNew (Just (V.toList (V.map (displayBase wording) baseValues)))
  baseRow <- new Adw.ComboRow [#title := "Base", #model := baseNames]
  accentRow <- new Adw.ActionRow [#title := "Accent"]
  swatches <- newSwatches wording
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #valign := Gtk.AlignCenter]
  forM_ swatches (\(_, button) -> Gtk.boxAppend box button)
  Adw.actionRowAddSuffix accentRow box
  paintBase <- bind (comboRow baseRow) (chooseFrom dispatch baseValues SetBase)
  paintAccent <- bind (toggleRadio swatches) (mapM_ (dispatch . SetAccent))
  pure AppearanceRows {baseRow, accentRow, paintBase, paintAccent}

newSwatches :: Wording -> IO (List (Maybe Accent, Gtk.ToggleButton))
newSwatches wording = do
  desktop <- newSwatch wording Nothing
  icon <- new Gtk.Image [#iconName := "computer-symbolic"]
  Gtk.buttonSetChild desktop (Just icon)
  others <- forM [minBound .. maxBound] $ \accent -> do
    button <- newSwatch wording (Just accent)
    Gtk.toggleButtonSetGroup button (Just desktop)
    pure (Just accent, button)
  pure ((Nothing, desktop) : others)

newSwatch :: Wording -> Maybe Accent -> IO Gtk.ToggleButton
newSwatch wording accent = do
  let label = displayAccent wording accent
  button <-
    new
      Gtk.ToggleButton
      [#tooltipText := label, #cssClasses := ["accent-swatch", maybe "desktop" accentName accent], #valign := Gtk.AlignCenter]
  nameAccessible button label
  pure button

chooseFrom :: (UiMessage -> IO ()) -> Vector a -> (a -> UiMessage) -> Word32 -> IO ()
chooseFrom dispatch values report index = forM_ (values V.!? fromIntegral index) (dispatch . report)

paintAppearance :: AppearanceRows -> Appearance -> IO ()
paintAppearance rows appearance = do
  forM_ (V.elemIndex appearance.base baseValues) (rows.paintBase . fromIntegral)
  rows.paintAccent (Just appearance.accent)

baseValues :: Vector Base
baseValues = V.fromList [minBound .. maxBound]
