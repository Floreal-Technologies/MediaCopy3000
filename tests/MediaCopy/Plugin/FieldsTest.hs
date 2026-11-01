module MediaCopy.Plugin.FieldsTest (tests) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.Field qualified as Plugin (Field)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Plugin.Fields qualified as Fields

tests :: TestTree
tests =
  testGroup
    "Plugin.Fields"
    [ testCase "Fields.values keeps values that fit their field kind and names the first field that does not" valuesKeepsValuesThatFit
    , testCase "mergeAuthorSettings fills author names and emails from the job form and names the first field that fails" mergeAuthorSettingsFillsAuthors
    ]

field :: Text -> Field.Kind -> Plugin.Field
field key kind = Field.Field {key, label = key, kind, required = True, defaultValue = Nothing}

slots :: Vector Plugin.AuthorSlot
slots = V.fromList [AuthorSlot.AuthorSlot "DIT" (Just "Jane Doe") Nothing Nothing, AuthorSlot.AuthorSlot "Camera operator" Nothing Nothing Nothing]

authorJson :: Text -> Maybe Text -> Value
authorJson role name = object (["role" .= role] <> maybe [] (\given -> ["name" .= given]) name)

valuesKeepsValuesThatFit :: Assertion
valuesKeepsValuesThatFit =
  map (\(fields, given, _) -> Fields.values fields given) table @?= map (\(_, _, expected) -> expected) table
  where
    table :: List (Vector Plugin.Field, Map Text Value, Either Text (Map Text Value))
    table =
      [
        ( V.fromList [field "camera" (Field.ChoiceField (V.fromList ["A", "B"])), field "dit" Field.TextField]
        , Map.fromList [("camera", String "B"), ("dit", String "Jane")]
        , Right (Map.fromList [("camera", String "B"), ("dit", String "Jane")])
        )
      ,
        ( V.singleton (field "camera" (Field.ChoiceField (V.fromList ["A", "B"])))
        , Map.singleton "camera" (String "C")
        , Left "camera"
        )
      ,
        ( V.singleton (field "offline" Field.BoolField)
        , Map.singleton "offline" (String "true")
        , Right (Map.singleton "offline" (Bool True))
        )
      ,
        ( V.singleton (field "token" Field.SecretField)
        , Map.singleton "token" (String "abc")
        , Right (Map.singleton "token" (String "abc"))
        )
      ,
        ( V.singleton (field "token" Field.SecretField)
        , Map.empty
        , Left "token"
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors" (Array (V.fromList [object ["role" .= ("DIT" :: Text), "name" .= ("Jane Doe" :: Text)], object ["role" .= ("Camera operator" :: Text)]]))
        , Right (Map.singleton "authors" (Array (V.fromList [authorJson "DIT" (Just "Jane Doe"), authorJson "Camera operator" Nothing])))
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors" (String "Jane")
        , Left "authors"
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors" (Array (V.singleton (object ["name" .= ("Jane" :: Text)])))
        , Left "authors"
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors" (Array (V.singleton (object ["role" .= (" " :: Text)])))
        , Left "authors"
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors" (Array (V.singleton (object ["role" .= ("DIT" :: Text), "email" .= ("jane" :: Text)])))
        , Left "authors"
        )
      ]

mergeAuthorSettingsFillsAuthors :: Assertion
mergeAuthorSettingsFillsAuthors =
  map (\(fields, given, settings, _) -> Fields.mergeAuthorSettings fields given settings) table @?= map (\(_, _, _, expected) -> expected) table
  where
    table :: List (Vector Plugin.Field, Map Text Text, Map Text Value, Either Text (Map Text Value))
    table =
      [
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors.1.name" "Sam Roe"
        , Map.singleton "authors" (toJSON slots)
        , Right (Map.singleton "authors" (Array (V.fromList [authorJson "DIT" (Just "Jane Doe"), authorJson "Camera operator" (Just "Sam Roe")])))
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.empty
        , Map.singleton "authors" (toJSON (V.drop 1 slots))
        , Left "authors"
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField) {Field.required = False}
        , Map.empty
        , Map.singleton "authors" (toJSON (V.drop 1 slots))
        , Right (Map.singleton "authors" (Array V.empty))
        )
      ,
        ( V.singleton (field "dit" Field.TextField)
        , Map.empty
        , Map.singleton "dit" (String "Jane")
        , Right (Map.singleton "dit" (String "Jane"))
        )
      ,
        ( V.singleton (field "authors" Field.AuthorsField)
        , Map.singleton "authors.0.email" "jane"
        , Map.singleton "authors" (toJSON slots)
        , Left "authors.0.email"
        )
      ]
