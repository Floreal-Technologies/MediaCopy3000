module MediaCopy.Plugin.Fields
  ( fieldValues
  , mergeAuthorSettings
  ) where

import Control.Monad (unless)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.Char (isControl, isSpace)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (AuthorSlot (..), Field (..), FieldKind (..))

import MediaCopy.Domain.PluginCatalog (emailAccepted, slotKey, validEmail)

-- $setup
-- >>> import Data.Aeson (Value (..), object, toJSON, (.=))
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Text (Text)
-- >>> import Data.Vector qualified as V
-- >>> import MediaCopy.Plugin.Manifest (AuthorSlot (..), Field (..), FieldKind (..))
-- >>> let field key kind = Field {key, label = key, kind, required = True, defaultValue = Nothing}
-- >>> let slots = V.fromList [AuthorSlot "DIT" "Jane Doe" "" "", AuthorSlot "Camera operator" "" "" ""]

-- |
-- >>> fieldValues (V.fromList [field "camera" (ChoiceField (V.fromList ["A", "B"])), field "dit" TextField]) (Map.fromList [("camera", String "B"), ("dit", String "Jane")])
-- Right (fromList [("camera",String "B"),("dit",String "Jane")])
-- >>> fieldValues (V.singleton (field "camera" (ChoiceField (V.fromList ["A", "B"])))) (Map.singleton "camera" (String "C"))
-- Left "camera"
-- >>> fieldValues (V.singleton (field "offline" BoolField)) (Map.singleton "offline" (String "true"))
-- Right (fromList [("offline",Bool True)])
-- >>> fieldValues (V.singleton (field "token" SecretField)) (Map.singleton "token" (String "abc"))
-- Right (fromList [("token",String "abc")])
-- >>> fieldValues (V.singleton (field "token" SecretField)) Map.empty
-- Left "token"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.fromList [object ["role" .= ("DIT" :: Text), "name" .= ("Jane Doe" :: Text)], object ["role" .= ("Camera operator" :: Text)]])))
-- Right (fromList [("authors",Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")]),Object (fromList [("email",String ""),("name",String ""),("phone",String ""),("role",String "Camera operator")])])])
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (String "Jane"))
-- Left "authors"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.singleton (object ["name" .= ("Jane" :: Text)]))))
-- Left "authors"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.singleton (object ["role" .= (" " :: Text)]))))
-- Left "authors"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.singleton (object ["role" .= ("DIT" :: Text), "email" .= ("jane" :: Text)]))))
-- Left "authors"
fieldValues :: Vector Field -> Map Text Value -> Either Text (Map Text Value)
fieldValues fields given = foldl' step (Right Map.empty) fields
  where
    step acc field = do
      values <- acc
      case Map.lookup field.key given >>= accepted field.kind of
        Just value -> Right (Map.insert field.key value values)
        Nothing
          | isJust (Map.lookup field.key given) -> Left field.key
          | Just value <- field.defaultValue >>= accepted field.kind -> Right (Map.insert field.key value values)
          | field.required -> Left field.key
          | otherwise -> Right values
    accepted kind value = case (kind, value) of
      (TextField, String text) -> Just (String text)
      (PathField, String text) -> Just (String text)
      (SecretField, String text) -> Just (String text)
      (BoolField, Bool flag) -> Just (Bool flag)
      (BoolField, String "true") -> Just (Bool True)
      (BoolField, String "false") -> Just (Bool False)
      (ChoiceField choices, String text) | text `elem` choices -> Just (String text)
      (AuthorsField, json) | Just slots <- parseMaybe parseJSON json :: Maybe (Vector AuthorSlot), all (\slot -> isJust (nonBlank slot.role) && emailAccepted slot.email) slots -> Just (toJSON slots)
      _ -> Nothing

-- |
-- >>> mergeAuthors "authors" (Map.fromList [("authors.1.name", "Sam Roe"), ("authors.0.email", "jane@example.com"), ("authors.0.name", "  "), ("authors.7.name", "Nobody")]) slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "jane@example.com", phone = ""},AuthorSlot {role = "Camera operator", name = "Sam Roe", email = "", phone = ""}]
-- >>> mergeAuthors "authors" Map.empty slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""}]
-- >>> mergeAuthors "authors" (Map.singleton "authors.0.role" "Boss") slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""}]
mergeAuthors :: Text -> Map Text Text -> Vector AuthorSlot -> Vector AuthorSlot
mergeAuthors key given slots = V.filter (isJust . nonBlank . (.name)) (V.imap merge slots)
  where
    merge index slot =
      AuthorSlot
        { role = slot.role
        , name = pick index "name" slot.name
        , email = pick index "email" slot.email
        , phone = pick index "phone" slot.phone
        }
    pick index part stored = fromMaybe stored (Map.lookup (slotKey key index part) given >>= nonBlank)

nonBlank :: Text -> Maybe Text
nonBlank text = if T.all (\c -> isSpace c || isControl c) text then Nothing else Just text

-- |
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors.1.name" "Sam Roe") (Map.singleton "authors" (toJSON slots))
-- Right (fromList [("authors",Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")]),Object (fromList [("email",String ""),("name",String "Sam Roe"),("phone",String ""),("role",String "Camera operator")])])])
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField)) Map.empty (Map.singleton "authors" (toJSON (V.drop 1 slots)))
-- Left "authors"
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField) {required = False}) Map.empty (Map.singleton "authors" (toJSON (V.drop 1 slots)))
-- Right (fromList [("authors",Array [])])
-- >>> mergeAuthorSettings (V.singleton (field "dit" TextField)) Map.empty (Map.singleton "dit" (String "Jane"))
-- Right (fromList [("dit",String "Jane")])
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors.0.email" "jane") (Map.singleton "authors" (toJSON slots))
-- Left "authors.0.email"
mergeAuthorSettings :: Vector Field -> Map Text Text -> Map Text Value -> Either Text (Map Text Value)
mergeAuthorSettings fields given settings = foldl' step (Right settings) fields
  where
    step acc field
      | field.kind /= AuthorsField = acc
      | otherwise = do
          values <- acc
          let stored = fromMaybe V.empty (Map.lookup field.key values >>= parseMaybe parseJSON)
              merged = mergeAuthors field.key given stored
          for_ [0 .. V.length stored - 1] $ \index ->
            let key = slotKey field.key index "email"
            in for_ (Map.lookup key given >>= nonBlank) (\email -> unless (validEmail email) (Left key))
          if V.null merged && field.required
            then Left field.key
            else Right (Map.insert field.key (toJSON merged) values)
