module MediaCopy.Domain.PluginCatalog
  ( Answer (..)
  , FieldShape (..)
  , FieldValue (..)
  , FieldView (..)
  , CapabilityView (..)
  , PluginEntry (..)
  , PluginCatalog (..)
  , emptyCatalog
  , Setting (..)
  , SecretText (..)
  , CatalogChange (..)
  , enabledJobFields
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Domain.Plugin (PluginRef (..))

data Answer = Granted | Declined | Unanswered
  deriving stock (Bounded, Enum, Eq, Show)

data FieldShape = TextShape | SecretShape | BoolShape | ChoiceShape (Vector Text) | PathShape
  deriving stock (Eq, Show)

data FieldValue = NoValue | Value Text | SecretStored | SecretInFile | SecretUnreadable Text
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
  { name :: Text
  , answer :: Answer
  }
  deriving stock (Eq, Show)

data PluginEntry = PluginEntry
  { plugin :: PluginRef
  , version :: Text
  , folder :: Text
  , roles :: Vector Text
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

data Setting = SettingText Text | SettingBool Bool
  deriving stock (Eq, Show)

-- |
-- >>> SetSecret "tech.floreal.probe" "token" (SecretText "abc")
-- SetSecret "tech.floreal.probe" "token" <secret>
newtype SecretText = SecretText Text
  deriving stock (Eq)

instance Show SecretText where
  showsPrec _ _ = showString "<secret>"

data CatalogChange
  = SetEnabled Text Bool
  | SetTrace Text Bool
  | SetAnswer Text Text Answer
  | SetSetting Text Text Setting
  | ClearSetting Text Text
  | SetSecret Text Text SecretText
  | ClearSecret Text Text
  deriving stock (Eq, Show)

-- |
-- >>> let field key = FieldView {key, label = key, shape = TextShape, required = True, value = NoValue}
-- >>> let entry ident on running = PluginEntry {plugin = PluginRef {id = ident, name = ident}, version = "1", folder = "", roles = V.empty, enabled = on, trace = False, capabilities = V.empty, settings = V.empty, jobFields = V.singleton (field "operator"), active = running, problem = Nothing}
-- >>> map (\(ref, fields) -> (ref.id, V.length fields)) (V.toList (enabledJobFields PluginCatalog {entries = V.fromList [entry "a" True True, entry "b" False True, entry "c" True False], rejected = V.empty, problem = Nothing}))
-- [("a",1)]
enabledJobFields :: PluginCatalog -> Vector (PluginRef, Vector FieldView)
enabledJobFields catalog =
  V.mapMaybe
    (\entry -> if entry.enabled && entry.active && not (V.null entry.jobFields) then Just (entry.plugin, entry.jobFields) else Nothing)
    catalog.entries
