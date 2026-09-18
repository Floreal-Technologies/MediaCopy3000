module MediaCopy.Domain.PluginFinding
  ( PluginFinding (..)
  , About (..)
  , Says (..)
  , Fault (..)
  ) where

import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.Severity (Severity)

-- $setup
-- >>> import Data.Text.Display (display)
-- >>> import MediaCopy.Domain.Severity qualified as Severity
-- >>> let probe = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}

data Fault
  = Unavailable Text
  | FieldMissing Text
  | BadOutput Text
  deriving stock (Eq, Ord, Show)

-- |
-- >>> map display [Unavailable "timed out after 30 s", FieldMissing "camera", BadOutput "metadata: <a> is in no namespace, not urn:x"]
-- ["plug-in unavailable \8211 timed out after 30 s","the field camera has no valid value","plug-in sent a bad answer \8211 metadata: <a> is in no namespace, not urn:x"]
instance Display Fault where
  displayBuilder = \case
    Unavailable reason -> "plug-in unavailable – " <> displayBuilder reason
    FieldMissing key -> "the field " <> displayBuilder key <> " has no valid value"
    BadOutput reason -> "plug-in sent a bad answer – " <> displayBuilder reason

data Says = Says
  { key :: Text
  , title :: Text
  , detail :: Text
  }
  deriving stock (Eq, Ord, Show)

data About
  = Said Says
  | Faulted Fault
  deriving stock (Eq, Ord, Show)

data PluginFinding = PluginFinding
  { plugin :: PluginRef
  , severity :: Severity
  , about :: About
  }
  deriving stock (Eq, Ord, Show)

-- |
-- >>> display PluginFinding {plugin = probe, severity = Severity.Warning, about = Said Says {key = "c2pa", title = "2 clips carry Content Credentials", detail = "A001C001.MP4"}}
-- "Probe: 2 clips carry Content Credentials \8211 A001C001.MP4"
-- >>> display PluginFinding {plugin = probe, severity = Severity.Blocker, about = Faulted (FieldMissing "camera")}
-- "Probe: the field camera has no valid value"
instance Display PluginFinding where
  displayBuilder finding =
    displayBuilder finding.plugin.name <> ": " <> case finding.about of
      Said says -> displayBuilder says.title <> " – " <> displayBuilder says.detail
      Faulted fault -> displayBuilder fault
