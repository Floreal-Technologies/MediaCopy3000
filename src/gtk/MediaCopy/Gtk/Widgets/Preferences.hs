module MediaCopy.Gtk.Widgets.Preferences
  ( Preferences (..)
  , newPreferences
  ) where

import Data.GI.Base (AttrOp ((:=)), new)
import Floreal.Commons.Gtk.Bind (bind, toggleRadio)
import Floreal.Commons.Gtk.Common (renderCell)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (nameAccessible)
import MediaCopy.Gtk.Widgets.Lists (choiceRow)
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
  , paintBase :: Base -> IO ()
  , paintAccent :: Maybe (Maybe Accent) -> IO ()
  }

newAppearanceRows :: Wording -> (UiMessage -> IO ()) -> IO AppearanceRows
newAppearanceRows wording dispatch = do
  baseRow <- new Adw.ComboRow [#title := "Base"]
  bases <- choiceRow (displayBase wording) baseValues baseRow
  accentRow <- new Adw.ActionRow [#title := "Accent"]
  swatches <- newSwatches wording
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #valign := Gtk.AlignCenter]
  forM_ swatches (\(_, button) -> Gtk.boxAppend box button)
  Adw.actionRowAddSuffix accentRow box
  paintBase <- bind bases (dispatch . SetBase)
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

paintAppearance :: AppearanceRows -> Appearance -> IO ()
paintAppearance rows appearance = do
  rows.paintBase appearance.base
  rows.paintAccent (Just appearance.accent)

baseValues :: NonEmpty Base
baseValues = minBound :| drop 1 [minBound .. maxBound]
