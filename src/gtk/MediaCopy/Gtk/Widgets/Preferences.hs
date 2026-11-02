module MediaCopy.Gtk.Widgets.Preferences
  ( Preferences (..)
  , newPreferences
  ) where

import Data.GI.Base (AttrOp ((:=)), new, set)
import Floreal.Commons.Gtk.Bind (bind, toggleRadio)
import Floreal.Commons.Gtk.Common (newCell, renderCell)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (nameAccessible)
import MediaCopy.Gtk.Widgets.Lists (choiceRow, relabelingChoiceRow)
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
  languageGroup <- new Adw.PreferencesGroup [#title := "Language", #description := "Applies to this run only"]
  (languageRow, paintLanguage) <- newLanguageRow dispatch
  Adw.preferencesGroupAdd languageGroup languageRow
  Adw.preferencesPageAdd page languageGroup
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
          paintLanguage model.wording.language
          paintAppearance rows model.wording model.appearance
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
  , paintBase :: (Wording, Base) -> IO ()
  , paintAccent :: Maybe (Maybe Accent) -> IO ()
  , labelSwatches :: Wording -> IO ()
  }

newLanguageRow :: (UiMessage -> IO ()) -> IO (Adw.ComboRow, SupportedLanguage -> IO ())
newLanguageRow dispatch = do
  row <- new Adw.ComboRow [#title := "Interface"]
  languages <- choiceRow endonym (minBound :| [succ minBound .. maxBound]) row
  paint <- bind languages (dispatch . SetLanguage)
  pure (row, paint)

newAppearanceRows :: Wording -> (UiMessage -> IO ()) -> IO AppearanceRows
newAppearanceRows wording dispatch = do
  baseRow <- new Adw.ComboRow [#title := "Base"]
  bases <- relabelingChoiceRow displayBase wording baseValues baseRow
  accentRow <- new Adw.ActionRow [#title := "Accent"]
  swatches <- newSwatches
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #valign := Gtk.AlignCenter]
  forM_ swatches (\(_, button) -> Gtk.boxAppend box button)
  Adw.actionRowAddSuffix accentRow box
  paintBase <- bind bases (dispatch . SetBase . snd)
  paintAccent <- bind (toggleRadio swatches) (mapM_ (dispatch . SetAccent))
  swatchLabels <- newCell (\current -> forM_ swatches (\(accent, button) -> labelSwatch current accent button))
  pure AppearanceRows {baseRow, accentRow, paintBase, paintAccent, labelSwatches = renderCell swatchLabels}

newSwatches :: IO (List (Maybe Accent, Gtk.ToggleButton))
newSwatches = do
  desktop <- newSwatch Nothing
  icon <- new Gtk.Image [#iconName := "computer-symbolic"]
  Gtk.buttonSetChild desktop (Just icon)
  others <- forM [minBound .. maxBound] $ \accent -> do
    button <- newSwatch (Just accent)
    Gtk.toggleButtonSetGroup button (Just desktop)
    pure (Just accent, button)
  pure ((Nothing, desktop) : others)

newSwatch :: Maybe Accent -> IO Gtk.ToggleButton
newSwatch accent =
  new Gtk.ToggleButton [#cssClasses := ["accent-swatch", maybe "desktop" accentName accent], #valign := Gtk.AlignCenter]

labelSwatch :: Wording -> Maybe Accent -> Gtk.ToggleButton -> IO ()
labelSwatch wording accent button = do
  let label = displayAccent wording accent
  set button [#tooltipText := label]
  nameAccessible button label

paintAppearance :: AppearanceRows -> Wording -> Appearance -> IO ()
paintAppearance rows wording appearance = do
  rows.paintBase (wording, appearance.base)
  rows.labelSwatches wording
  rows.paintAccent (Just appearance.accent)

baseValues :: NonEmpty Base
baseValues = minBound :| [succ minBound .. maxBound]
