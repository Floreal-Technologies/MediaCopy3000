module MediaCopy.Gtk.Widgets.FieldRows
  ( FieldActions (..)
  , FieldRow (..)
  , fieldRow
  , later
  ) where

import Control.Monad (void, when)
import Data.Functor ((<&>))
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.IORef (newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.PluginCatalog (FieldShape (..), FieldValue (..), FieldView (..))
import MediaCopy.Gtk.Widgets.Common (suppressing, unlessSuppressed)

data FieldActions = FieldActions
  { setText :: Text -> IO ()
  , setBool :: Bool -> IO ()
  , setSecret :: Text -> IO ()
  , clear :: IO ()
  , pickPath :: IO ()
  }

data FieldRow = FieldRow
  { row :: Adw.PreferencesRow
  , refresh :: FieldValue -> IO ()
  }

later :: IO () -> IO ()
later act = void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (act >> pure False))

fieldRow :: FieldActions -> Text -> FieldView -> IO FieldRow
fieldRow actions prefix field = do
  suppress <- newIORef False
  let handle chosen = unlessSuppressed suppress (chosen >>= later)
      quietly = suppressing suppress
  case field.shape of
    BoolShape -> do
      row <- new Adw.SwitchRow [#useMarkup := False, #active := isTrue field.value]
      set row [#title := title]
      void $ on row (Adw.PropertyNotify #active) $ \_ -> handle (Adw.switchRowGetActive row <&> actions.setBool)
      shown <- Adw.toPreferencesRow row
      pure FieldRow {row = shown, refresh = quietly . Adw.switchRowSetActive row . isTrue}
    ChoiceShape choices -> do
      let indexOf = \case
            Value text -> maybe 0 (fromIntegral . succ) (V.elemIndex text choices)
            _ -> 0
      names <- Gtk.stringListNew (Just ("Not set" : V.toList choices))
      row <- new Adw.ComboRow [#useMarkup := False, #model := names, #selected := indexOf field.value]
      set row [#title := title]
      void $ on row (Adw.PropertyNotify #selected) $ \_ ->
        handle (Adw.comboRowGetSelected row <&> \index -> maybe actions.clear actions.setText (choices V.!? (fromIntegral index - 1)))
      shown <- Adw.toPreferencesRow row
      pure FieldRow {row = shown, refresh = quietly . Adw.comboRowSetSelected row . indexOf}
    SecretShape -> do
      row <- new Adw.PasswordEntryRow [#useMarkup := False, #showApplyButton := True]
      set row [#title := title <> secretState field.value]
      void $ on row #apply $ do
        text <- Gtk.editableGetText row
        Gtk.editableSetText row ""
        later (if T.null text then actions.clear else actions.setSecret text)
      shown <- Adw.toPreferencesRow row
      pure FieldRow {row = shown, refresh = \value -> Adw.preferencesRowSetTitle row (title <> secretState value)}
    _ -> do
      row <- new Adw.EntryRow [#useMarkup := False, #text := textOf field.value, #showApplyButton := True]
      set row [#title := title]
      void $ on row #apply $ do
        text <- Gtk.editableGetText row
        later (if T.null text then actions.clear else actions.setText text)
      when (field.shape == PathShape) $ do
        choose <- new Gtk.Button [#label := "Choose…", #valign := Gtk.AlignCenter, On #clicked (later actions.pickPath)]
        Gtk.widgetAddCssClass choose "flat"
        Adw.entryRowAddSuffix row choose
      shown <- Adw.toPreferencesRow row
      let refresh value = do
            current <- Gtk.editableGetText row
            when (current /= textOf value) (Gtk.editableSetText row (textOf value))
      pure FieldRow {row = shown, refresh}
  where
    title = prefix <> field.label <> (if field.required then " (required)" else "")
    isTrue value = value == Value "true"
    textOf = \case
      Value text -> text
      _ -> ""
    secretState = \case
      SecretStored -> " – stored in the keyring"
      SecretInFile -> " – set in plugins.json"
      SecretUnreadable _ -> " – the keyring refused"
      Value _ -> " – set"
      NoValue -> ""
