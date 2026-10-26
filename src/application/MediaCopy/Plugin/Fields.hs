module MediaCopy.Plugin.Fields
  ( values
  , mergeAuthorSettings
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.Char (isControl, isSpace)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.Field qualified as Plugin (Field)

import MediaCopy.Domain.FieldView qualified as FieldView

values :: Vector Plugin.Field -> Map Text Value -> Either Text (Map Text Value)
values fields given = foldl' step (Right Map.empty) fields
  where
    step acc field = do
      collected <- acc
      case Map.lookup field.key given >>= accepted field.kind of
        Just value -> Right (Map.insert field.key value collected)
        Nothing
          | isJust (Map.lookup field.key given) -> Left field.key
          | Just value <- field.defaultValue >>= accepted field.kind -> Right (Map.insert field.key value collected)
          | field.required -> Left field.key
          | otherwise -> Right collected
    accepted kind value = case (kind, value) of
      (Field.TextField, String text) -> Just (String text)
      (Field.PathField, String text) -> Just (String text)
      (Field.SecretField, String text) -> Just (String text)
      (Field.BoolField, Bool flag) -> Just (Bool flag)
      (Field.BoolField, String "true") -> Just (Bool True)
      (Field.BoolField, String "false") -> Just (Bool False)
      (Field.ChoiceField choices, String text) | text `elem` choices -> Just (String text)
      (Field.AuthorsField, json) | Just slots <- parseMaybe parseJSON json :: Maybe (Vector Plugin.AuthorSlot), all (\slot -> isJust (nonBlank slot.role) && all FieldView.emailAccepted slot.email) slots -> Just (toJSON slots)
      _ -> Nothing

mergeAuthors :: Text -> Map Text Text -> Vector Plugin.AuthorSlot -> Vector Plugin.AuthorSlot
mergeAuthors key given slots = V.filter (\slot -> isJust (slot.name >>= nonBlank)) (V.imap merge slots)
  where
    merge index slot =
      AuthorSlot.AuthorSlot
        { role = slot.role
        , name = pick index "name" slot.name
        , email = pick index "email" slot.email
        , phone = pick index "phone" slot.phone
        }
    pick index part stored = (Map.lookup (FieldView.slotKey key index part) given >>= nonBlank) <|> stored

nonBlank :: Text -> Maybe Text
nonBlank text = if T.all (\c -> isSpace c || isControl c) text then Nothing else Just text

mergeAuthorSettings :: Vector Plugin.Field -> Map Text Text -> Map Text Value -> Either Text (Map Text Value)
mergeAuthorSettings fields given settings = foldl' step (Right settings) fields
  where
    step acc field
      | field.kind /= Field.AuthorsField = acc
      | otherwise = do
          collected <- acc
          let stored = fromMaybe V.empty (Map.lookup field.key collected >>= parseMaybe parseJSON)
              merged = mergeAuthors field.key given stored
          for_ [0 .. V.length stored - 1] $ \index ->
            let key = FieldView.slotKey field.key index "email"
            in for_ (Map.lookup key given >>= nonBlank) (\email -> unless (FieldView.validEmail email) (Left key))
          if V.null merged && field.required
            then Left field.key
            else Right (Map.insert field.key (toJSON merged) collected)
