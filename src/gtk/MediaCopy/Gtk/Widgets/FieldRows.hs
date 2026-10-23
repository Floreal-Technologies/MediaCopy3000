{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Widgets.FieldRows
  ( FieldActions (..)
  , FieldRow (..)
  , authorsRow
  , fieldRow
  ) where

import Control.Monad (zipWithM_)
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.Text qualified as T
import Data.Vector qualified as V
import Floreal.Commons.Gtk.Bind (Control (..), accepting, bind, connectSelf, entryApply, switchRow)
import Floreal.Commons.Gtk.Common (paintEditable)
import Floreal.Commons.Gtk.Signal (onSelf)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Field qualified as Field

import MediaCopy.Domain.FieldView (FieldView)
import MediaCopy.Domain.FieldView qualified as FieldView
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Gtk.Widgets.Lists (choiceRow)

data FieldActions = FieldActions
  { setText :: Text -> IO ()
  , setBool :: Bool -> IO ()
  , clear :: IO ()
  , pickPath :: IO ()
  }

data FieldRow = FieldRow
  { rows :: [Adw.PreferencesRow]
  , refresh :: FieldView.FieldValue -> IO ()
  }

fieldRow :: FieldActions -> Text -> FieldView -> IO FieldRow
fieldRow actions prefix field = do
  let title = fieldTitle prefix field
  case field.kind of
    Field.BoolField -> do
      row <- new Adw.SwitchRow [#useMarkup := False, #active := isTrue field.value]
      set row [#title := title]
      paintActive <- bind (switchRow row) actions.setBool
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintActive . isTrue}
    Field.ChoiceField choices -> do
      let chosen = \case
            FieldView.Value text | V.elem text choices -> Just text
            _ -> Nothing
      row <- new Adw.ComboRow [#useMarkup := False]
      set row [#title := title]
      options <- choiceRow (fromMaybe "Not set") (Nothing :| map Just (V.toList choices)) row
      paintSelected <- bind options (maybe actions.clear actions.setText)
      paintSelected (chosen field.value)
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintSelected . chosen}
    Field.AuthorsField -> do
      header <- authorsHeader title (FieldView.authorSlots field.value)
      shown <- Adw.toPreferencesRow header
      pure FieldRow {rows = [shown], refresh = \value -> value & FieldView.authorSlots & countText & Adw.actionRowSetSubtitle header}
    _ -> do
      row <- (if field.kind == Field.SecretField then newPasswordRow else newEntryRow) title (textOf field.value)
      applied <- entryApply row
      let apply = if field.kind == Field.EmailField then accepting FieldView.emailAccepted applied else applied
      paintText <- bind apply (\text -> if T.null text then actions.clear else actions.setText text)
      when (field.kind == Field.EmailField) (emailRow row)
      when (field.kind == Field.PathField) $ do
        choose <- new Gtk.Button [#label := "Choose…", #valign := Gtk.AlignCenter, On #clicked actions.pickPath]
        Gtk.widgetAddCssClass choose "flat"
        Adw.entryRowAddSuffix row choose
      shown <- Adw.toPreferencesRow row
      pure FieldRow {rows = [shown], refresh = paintText . textOf}
  where
    isTrue value = value == FieldView.Value "true"
    textOf = \case
      FieldView.Value text -> text
      _ -> ""

authorsRow :: (Vector Plugin.AuthorSlot -> IO ()) -> FieldView -> IO FieldRow
authorsRow setAuthors field = do
  let slots = FieldView.authorSlots field.value
  header <- authorsHeader (fieldTitle "" field) slots
  add <- new Gtk.Button [#label := "Add", #valign := Gtk.AlignCenter]
  Gtk.widgetAddCssClass add "flat"
  Adw.actionRowAddSuffix header add
  edited <- traverse slotRows (V.toList slots)
  applied <- newIORef slots
  let commitWith f = readIORef applied >>= \shown -> let next = f shown in when (validSlots next) (setAuthors next)
      applyField report index setField text = do
        next <- readIORef applied <&> V.imap (\i value -> if i == index then setField text value else value)
        when (validSlots next) $ do
          writeIORef applied next
          report (const next)
      control =
        Control
          { paint = \fresh -> do
              writeIORef applied fresh
              Adw.actionRowSetSubtitle header (countText fresh)
              zipWithM_ (\slot value -> slot.paint value) edited (V.toList fresh)
          , connect = \report ->
              mconcat
                <$> sequence
                  [ connectSelf entry #apply (Gtk.editableGetText ?self >>= applyField report index setField)
                  | (index, slot) <- zip [0 ..] edited
                  , (entry, setField) <- zip slot.entries slotFields
                  ]
          , current = readIORef applied
          }
  paintSlots <- bind control setAuthors
  void $ onSelf add #clicked (commitWith (`V.snoc` AuthorSlot.AuthorSlot "" "" "" ""))
  forM_ (zip [0 ..] edited) $ \(index, slot) ->
    void $ onSelf slot.remove #clicked (commitWith (V.ifilter (\i _ -> i /= index)))
  headerShown <- Adw.toPreferencesRow header
  slotsShown <- traverse (\slot -> Adw.toPreferencesRow slot.expander) edited
  pure FieldRow {rows = headerShown : slotsShown, refresh = paintSlots . FieldView.authorSlots}

validSlots :: Vector Plugin.AuthorSlot -> Bool
validSlots = all (\slot -> FieldView.emailAccepted slot.email)

fieldTitle :: Text -> FieldView -> Text
fieldTitle prefix field = prefix <> field.label <> (if field.required then " (required)" else "")

authorsHeader :: Text -> Vector Plugin.AuthorSlot -> IO Adw.ActionRow
authorsHeader title slots = do
  header <- new Adw.ActionRow [#useMarkup := False]
  set header [#title := title, #subtitle := countText slots]
  pure header

countText :: Vector Plugin.AuthorSlot -> Text
countText slots = case V.length slots of
  0 -> "No author yet"
  n -> Prose.plural "author" n

newEntryRow :: Text -> Text -> IO Adw.EntryRow
newEntryRow title text = do
  row <- new Adw.EntryRow [#useMarkup := False, #text := text, #showApplyButton := True]
  set row [#title := title]
  pure row

newPasswordRow :: Text -> Text -> IO Adw.EntryRow
newPasswordRow title text = do
  row <- new Adw.PasswordEntryRow [#useMarkup := False, #text := text, #showApplyButton := True]
  set row [#title := title]
  Adw.toEntryRow row

emailRow :: Adw.EntryRow -> IO ()
emailRow row = do
  set row [#inputPurpose := Gtk.InputPurposeEmail]
  let flag entry = do
        text <- Gtk.editableGetText entry
        if FieldView.emailAccepted text then Gtk.widgetRemoveCssClass entry "error" else Gtk.widgetAddCssClass entry "error"
  flag row
  void $ onSelf row #changed (flag ?self)

data SlotRows = SlotRows
  { expander :: Adw.ExpanderRow
  , entries :: [Adw.EntryRow]
  , remove :: Gtk.Button
  , paint :: Plugin.AuthorSlot -> IO ()
  }

slotRows :: Plugin.AuthorSlot -> IO SlotRows
slotRows slot = do
  expander <- new Adw.ExpanderRow [#useMarkup := False]
  set expander [#title := slotTitle slot, #subtitle := slot.name]
  role <- newEntryRow "Role (required)" slot.role
  name <- newEntryRow "Name" slot.name
  email <- newEntryRow "Email" slot.email
  emailRow email
  phone <- newEntryRow "Phone" slot.phone
  let entries = [role, name, email, phone]
  forM_ entries (Adw.expanderRowAddRow expander)
  remove <- new Gtk.Button [#label := "Remove", #valign := Gtk.AlignCenter]
  Gtk.widgetAddCssClass remove "destructive-action"
  removeRow <- new Adw.ActionRow [#useMarkup := False]
  set removeRow [#title := "Remove this author"]
  Adw.actionRowAddSuffix removeRow remove
  Adw.expanderRowAddRow expander removeRow
  let paint fresh = do
        Adw.preferencesRowSetTitle expander (slotTitle fresh)
        Adw.expanderRowSetSubtitle expander fresh.name
        zipWithM_ paintEditable entries [fresh.role, fresh.name, fresh.email, fresh.phone]
  pure SlotRows {expander, entries, remove, paint}

slotFields :: List (Text -> Plugin.AuthorSlot -> Plugin.AuthorSlot)
slotFields =
  [ \text slot -> slot {AuthorSlot.role = text}
  , \text slot -> slot {AuthorSlot.name = text}
  , \text slot -> slot {AuthorSlot.email = text}
  , \text slot -> slot {AuthorSlot.phone = text}
  ]

slotTitle :: Plugin.AuthorSlot -> Text
slotTitle slot = if T.null slot.role then "New author" else slot.role
