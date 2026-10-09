module MediaCopy.Domain.PluginCatalog
  ( Answer (..)
  , AuthorSlot (..)
  , FieldShape (..)
  , FieldValue (..)
  , FieldView (..)
  , CapabilityView (..)
  , PluginEntry (..)
  , PluginCatalog (..)
  , emptyCatalog
  , Setting (..)
  , CatalogChange (..)
  , enabledJobFields
  , authorJobFields
  , authorSlots
  , slotKey
  , validEmail
  , emailAccepted
  , entryById
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (AuthorSlot (..), Capability)

import MediaCopy.Domain.Plugin (PluginRef (..))

data Answer = Granted | Declined | Unanswered
  deriving stock (Bounded, Enum, Eq, Show)

data FieldShape = TextShape | EmailShape | BoolShape | ChoiceShape (Vector Text) | PathShape | AuthorsShape
  deriving stock (Eq, Show)

data FieldValue = NoValue | Value Text | AuthorList (Vector AuthorSlot)
  deriving stock (Eq, Show)

data FieldView = FieldView
  { key :: Text
  , label :: Text
  , shape :: FieldShape
  , required :: Bool
  , value :: FieldValue
  }
  deriving stock (Eq, Show)

data CapabilityView = CapabilityView
  { capability :: Capability
  , answer :: Answer
  }
  deriving stock (Eq, Show)

data PluginEntry = PluginEntry
  { plugin :: PluginRef
  , version :: Text
  , description :: Text
  , folder :: Text
  , enabled :: Bool
  , trace :: Bool
  , capabilities :: Vector CapabilityView
  , settings :: Vector FieldView
  , jobFields :: Vector FieldView
  , active :: Bool
  , problem :: Maybe Text
  }
  deriving stock (Eq, Show)

data PluginCatalog = PluginCatalog
  { entries :: Vector PluginEntry
  , rejected :: Vector (Text, Text)
  , problem :: Maybe Text
  }
  deriving stock (Eq, Show)

emptyCatalog :: PluginCatalog
emptyCatalog = PluginCatalog {entries = V.empty, rejected = V.empty, problem = Nothing}

data Setting = SettingText Text | SettingBool Bool | SettingAuthors (Vector AuthorSlot)
  deriving stock (Eq, Show)

data CatalogChange
  = SetEnabled Text Bool
  | SetTrace Text Bool
  | SetAnswer Text Capability Answer
  | SetSetting Text Text Setting
  | ClearSetting Text Text
  deriving stock (Eq, Show)

-- |
-- >>> let field key = FieldView {key, label = key, shape = TextShape, required = True, value = NoValue}
-- >>> let entry ident on running = PluginEntry {plugin = PluginRef {id = ident, name = ident}, version = "1", description = "", folder = "", enabled = on, trace = False, capabilities = V.empty, settings = V.empty, jobFields = V.singleton (field "operator"), active = running, problem = Nothing}
-- >>> map (\(ref, fields) -> (ref.id, V.length fields)) (V.toList (enabledJobFields PluginCatalog {entries = V.fromList [entry "a" True True, entry "b" False True, entry "c" True False], rejected = V.empty, problem = Nothing}))
-- [("a",1)]
enabledJobFields :: PluginCatalog -> Vector (PluginRef, Vector FieldView)
enabledJobFields catalog =
  V.mapMaybe
    (\entry -> if entry.enabled && entry.active && not (V.null entry.jobFields) then Just (entry.plugin, entry.jobFields) else Nothing)
    catalog.entries

-- |
-- >>> map (\field -> (field.key, field.label, field.value)) (V.toList (authorJobFields "authors" (V.fromList [AuthorSlot "DIT" "Jane Doe" "" "", AuthorSlot "Camera operator" "" "" ""])))
-- [("authors.0.name","DIT: Name",Value "Jane Doe"),("authors.0.email","DIT: Email",NoValue),("authors.0.phone","DIT: Phone",NoValue),("authors.1.name","Camera operator: Name",NoValue),("authors.1.email","Camera operator: Email",NoValue),("authors.1.phone","Camera operator: Phone",NoValue)]
authorJobFields :: Text -> Vector AuthorSlot -> Vector FieldView
authorJobFields key slots =
  V.fromList
    [ FieldView
        { key = slotKey key index part
        , label = slot.role <> ": " <> label
        , shape = if part == "email" then EmailShape else TextShape
        , required = False
        , value = if T.null text then NoValue else Value text
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
-- >>> V.length (authorSlots (AuthorList (V.singleton (AuthorSlot "DIT" "" "" ""))))
-- 1
-- >>> V.length (authorSlots (Value "x"))
-- 0
authorSlots :: FieldValue -> Vector AuthorSlot
authorSlots = \case
  AuthorList slots -> slots
  _ -> V.empty

entryById :: Text -> PluginCatalog -> Maybe PluginEntry
entryById pluginId catalog = V.find (\entry -> entry.plugin.id == pluginId) catalog.entries
