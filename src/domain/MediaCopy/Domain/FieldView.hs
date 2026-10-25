module MediaCopy.Domain.FieldView
  ( FieldView (..)
  , FieldValue (..)
  , authorFields
  , authorSlots
  , slotKey
  , validEmail
  , emailAccepted
  ) where

import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Field qualified as Field

data FieldValue = NoValue | Value Text | AuthorList (Vector Plugin.AuthorSlot)
  deriving stock (Eq, Show)

data FieldView = FieldView
  { key :: Text
  , label :: Text
  , kind :: Field.Kind
  , required :: Bool
  , value :: FieldValue
  }
  deriving stock (Eq, Show)

-- |
-- >>> map (\field -> (field.key, field.label, field.value)) (V.toList (authorFields "authors" (V.fromList [AuthorSlot.AuthorSlot "DIT" (Just "Jane Doe") Nothing Nothing, AuthorSlot.AuthorSlot "Camera operator" Nothing Nothing Nothing])))
-- [("authors.0.name","DIT: Name",Value "Jane Doe"),("authors.0.email","DIT: Email",NoValue),("authors.0.phone","DIT: Phone",NoValue),("authors.1.name","Camera operator: Name",NoValue),("authors.1.email","Camera operator: Email",NoValue),("authors.1.phone","Camera operator: Phone",NoValue)]
authorFields :: Text -> Vector Plugin.AuthorSlot -> Vector FieldView
authorFields key slots =
  V.fromList
    [ FieldView
        { key = slotKey key index part
        , label = slot.role <> ": " <> label
        , kind = if part == "email" then Field.EmailField else Field.TextField
        , required = False
        , value = maybe NoValue Value text
        }
    | (index, slot) <- zip [0 ..] (V.toList slots)
    , (part, label, text) <- [("name", "Name", slot.name), ("email", "Email", slot.email), ("phone", "Phone", slot.phone)]
    ]

-- |
-- >>> slotKey "authors" 2 "email"
-- "authors.2.email"
slotKey :: Text -> Int -> Text -> Text
slotKey key index part = key <> "." <> T.pack (show index) <> "." <> part

-- | @EmailAddressAttributeType@ of the ASC MHL schema.
--
-- >>> map validEmail ["jane@example.com", "jane@mail.example.com", "a@b@c.d", "jane@example", "jane@.com", "@example.com", "jane@example.", "jane@example.c\nom", ""]
-- [True,True,True,False,False,False,False,False,False]
validEmail :: Text -> Bool
validEmail text = not (T.null local || T.null rest || T.null host || T.null tld) && T.all (`notElem` ['\n', '\r']) tld
  where
    (local, rest) = T.breakOn "@" text
    (host, dotted) = T.breakOn "." (T.drop 1 rest)
    tld = T.drop 1 dotted

-- |
-- >>> map emailAccepted ["", "jane@example.com", "jane"]
-- [True,True,False]
emailAccepted :: Text -> Bool
emailAccepted text = T.null text || validEmail text

-- |
-- >>> V.length (authorSlots (AuthorList (V.singleton (AuthorSlot.AuthorSlot "DIT" Nothing Nothing Nothing))))
-- 1
-- >>> V.length (authorSlots (Value "x"))
-- 0
authorSlots :: FieldValue -> Vector Plugin.AuthorSlot
authorSlots = \case
  AuthorList slots -> slots
  _ -> V.empty
