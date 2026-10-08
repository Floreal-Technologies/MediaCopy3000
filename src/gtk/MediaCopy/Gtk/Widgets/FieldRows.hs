module MediaCopy.Gtk.Widgets.FieldRows
  ( FieldActions (..)
  , FieldRow (..)
  , authorsRow
  , fieldRow
  ) where

import Control.Monad (forM, forM_, void, when, zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Domain.Job (plural)
import MediaCopy.Domain.PluginCatalog (AuthorSlot (..), FieldShape (..), FieldValue (..), FieldView (..), authorSlots)
import MediaCopy.Gtk.Widgets.Bind (Control (..), bind, comboRow, entryApply, switchRow)
import MediaCopy.Gtk.Widgets.Common (paintEditable)

data FieldActions = FieldActions
  { setText :: Text -> IO ()
  , setBool :: Bool -> IO ()
  , clear :: IO ()
  , pickPath :: IO ()
  }

data FieldRow = FieldRow
  { rows :: [Adw.PreferencesRow]
  , refresh :: FieldValue -> IO ()
  }

fieldRow :: FieldActions -> Text -> FieldView -> IO FieldRow
fieldRow actions prefix field = do
  let title = fieldTitle prefix field
  case field.shape of
    BoolShape -> do
      row <- new Adw.SwitchRow [#useMarkup := False, #active := isTrue field.value]
      set row [#title := title]
      paintActive <- bind (switchRow row) actions.setBool
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintActive . isTrue}
    ChoiceShape choices -> do
      let indexOf = \case
            Value text -> maybe 0 (fromIntegral . succ) (V.elemIndex text choices)
            _ -> 0
      names <- Gtk.stringListNew (Just ("Not set" : V.toList choices))
      row <- new Adw.ComboRow [#useMarkup := False, #model := names, #selected := indexOf field.value]
      set row [#title := title]
      paintSelected <- bind (comboRow row) (\index -> maybe actions.clear actions.setText (choices V.!? (fromIntegral index - 1)))
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintSelected . indexOf}
    AuthorsShape -> do
      header <- authorsHeader title (authorSlots field.value)
      shown <- Adw.toPreferencesRow header
      pure FieldRow {rows = [shown], refresh = Adw.actionRowSetSubtitle header . countText . authorSlots}
    _ -> do
      row <- newEntryRow title (textOf field.value)
      paintText <- bind (entryApply row) (\text -> if T.null text then actions.clear else actions.setText text)
      when (field.shape == PathShape) $ do
        choose <- new Gtk.Button [#label := "Choose…", #valign := Gtk.AlignCenter, On #clicked actions.pickPath]
        Gtk.widgetAddCssClass choose "flat"
        Adw.entryRowAddSuffix row choose
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintText . textOf}
  where
    isTrue value = value == Value "true"
    textOf = \case
      Value text -> text
      _ -> ""

authorsRow :: (Vector AuthorSlot -> IO ()) -> Text -> FieldView -> IO FieldRow
authorsRow setAuthors prefix field = do
  let slots = authorSlots field.value
  header <- authorsHeader (fieldTitle prefix field) slots
  add <- new Gtk.Button [#label := "Add", #valign := Gtk.AlignCenter]
  Gtk.widgetAddCssClass add "flat"
  Adw.actionRowAddSuffix header add
  edited <- traverse slotRows (V.toList slots)
  let readAll = V.fromList <$> traverse (\slot -> slot.current) edited
      commitWith f = readAll >>= setAuthors . f
      control =
        Control
          { paint = \fresh -> do
              Adw.actionRowSetSubtitle header (countText fresh)
              zipWithM_ (\slot value -> slot.paint value) edited (V.toList fresh)
          , connect = \callback -> do
              quiets <- forM (concatMap (\slot -> slot.entries) edited) (\entry -> (entryApply entry).connect callback)
              pure (\act -> foldr (\quiet inner -> quiet inner) act quiets)
          , current = readAll
          }
  paintSlots <- bind control setAuthors
  void $ on add #clicked (commitWith (`V.snoc` AuthorSlot "" "" "" ""))
  forM_ (zip [0 ..] edited) $ \(index, slot) ->
    void $ on slot.remove #clicked (commitWith (V.ifilter (\i _ -> i /= index)))
  headerShown <- Adw.toPreferencesRow header
  slotsShown <- traverse (\slot -> Adw.toPreferencesRow slot.expander) edited
  pure FieldRow {rows = headerShown : slotsShown, refresh = paintSlots . authorSlots}

fieldTitle :: Text -> FieldView -> Text
fieldTitle prefix field = prefix <> field.label <> (if field.required then " (required)" else "")

authorsHeader :: Text -> Vector AuthorSlot -> IO Adw.ActionRow
authorsHeader title slots = do
  header <- new Adw.ActionRow [#useMarkup := False]
  set header [#title := title, #subtitle := countText slots]
  pure header

countText :: Vector AuthorSlot -> Text
countText slots = case V.length slots of
  0 -> "No author yet"
  n -> plural "author" n

newEntryRow :: Text -> Text -> IO Adw.EntryRow
newEntryRow title text = do
  row <- new Adw.EntryRow [#useMarkup := False, #text := text, #showApplyButton := True]
  set row [#title := title]
  pure row

data SlotRows = SlotRows
  { expander :: Adw.ExpanderRow
  , entries :: [Adw.EntryRow]
  , remove :: Gtk.Button
  , current :: IO AuthorSlot
  , paint :: AuthorSlot -> IO ()
  }

slotRows :: AuthorSlot -> IO SlotRows
slotRows slot = do
  expander <- new Adw.ExpanderRow [#useMarkup := False]
  set expander [#title := slotTitle slot, #subtitle := slot.name]
  role <- newEntryRow "Role (required)" slot.role
  name <- newEntryRow "Name" slot.name
  email <- newEntryRow "Email" slot.email
  phone <- newEntryRow "Phone" slot.phone
  let entries = [role, name, email, phone]
  forM_ entries (Adw.expanderRowAddRow expander)
  remove <- new Gtk.Button [#label := "Remove", #valign := Gtk.AlignCenter]
  Gtk.widgetAddCssClass remove "destructive-action"
  removeRow <- new Adw.ActionRow [#useMarkup := False]
  set removeRow [#title := "Remove this author"]
  Adw.actionRowAddSuffix removeRow remove
  Adw.expanderRowAddRow expander removeRow
  let current = AuthorSlot <$> Gtk.editableGetText role <*> Gtk.editableGetText name <*> Gtk.editableGetText email <*> Gtk.editableGetText phone
      paint fresh = do
        Adw.preferencesRowSetTitle expander (slotTitle fresh)
        Adw.expanderRowSetSubtitle expander fresh.name
        zipWithM_ paintEditable entries [fresh.role, fresh.name, fresh.email, fresh.phone]
  pure SlotRows {expander, entries, remove, current, paint}

slotTitle :: AuthorSlot -> Text
slotTitle slot = if T.null slot.role then "New author" else slot.role
