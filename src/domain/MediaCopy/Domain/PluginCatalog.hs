module MediaCopy.Domain.PluginCatalog
  ( PluginCatalog (..)
  , Entry (..)
  , CapabilityView (..)
  , Answer (..)
  , Setting (..)
  , Change (..)
  , Edit (..)
  , empty
  , enabledJobFields
  , entryById
  ) where

import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)

import MediaCopy.Domain.FieldView (FieldView)
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef

data Answer = Granted | Declined | Unanswered
  deriving stock (Bounded, Enum, Eq, Show)

data CapabilityView = CapabilityView
  { capability :: Plugin.Capability
  , answer :: Answer
  }
  deriving stock (Eq, Show)

data Entry = Entry
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
  { entries :: Vector Entry
  , rejected :: Vector (Text, Text)
  , problem :: Maybe Text
  }
  deriving stock (Eq, Show)

empty :: PluginCatalog
empty = PluginCatalog {entries = V.empty, rejected = V.empty, problem = Nothing}

data Setting = SettingText Text | SettingBool Bool | SettingAuthors (Vector Plugin.AuthorSlot)
  deriving stock (Eq, Show)

data Change = Change Text Edit
  deriving stock (Eq, Show)

data Edit = SetEnabled Bool | SetTrace Bool | SetAnswer Plugin.Capability Answer | SetSetting Text Setting | ClearSetting Text
  deriving stock (Eq, Show)

enabledJobFields :: PluginCatalog -> Vector (PluginRef, Vector FieldView)
enabledJobFields catalog =
  V.mapMaybe
    (\entry -> if entry.enabled && entry.active && not (V.null entry.jobFields) then Just (entry.plugin, entry.jobFields) else Nothing)
    catalog.entries

entryById :: Text -> PluginCatalog -> Maybe Entry
entryById pluginId catalog = V.find (\entry -> entry.plugin.id == pluginId) catalog.entries
