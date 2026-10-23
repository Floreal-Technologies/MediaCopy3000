module MediaCopy.Interface.View.Plugins
  ( entrySubtitle
  , capabilitySubtitle
  , capabilityText
  , answers
  , answerText
  ) where

import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)

import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog

-- |
-- >>> fmap answerText answers
-- "Not answered" :| ["Granted","Declined"]
answers :: NonEmpty PluginCatalog.Answer
answers = PluginCatalog.Unanswered :| [PluginCatalog.Granted, PluginCatalog.Declined]

entrySubtitle :: PluginCatalog.Entry -> Text
entrySubtitle entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList (V.map (Capability.toText . (.capability)) entry.capabilities)) : maybe [] pure entry.problem)

capabilitySubtitle :: PluginCatalog.CapabilityView -> Text
capabilitySubtitle view = capabilityText view.capability <> (if view.answer == PluginCatalog.Unanswered then " – not answered yet" else "")

answerText :: PluginCatalog.Answer -> Text
answerText = \case
  PluginCatalog.Unanswered -> "Not answered"
  PluginCatalog.Granted -> "Granted"
  PluginCatalog.Declined -> "Declined"

capabilityText :: Plugin.Capability -> Text
capabilityText = \case
  Capability.FilesRead -> "Read the media files"
  Capability.PlanInspect -> "Add findings to the plan"
  Capability.FilesInspect -> "Add notes about each verified file to the report"
  Capability.Block -> "Stop a job with a blocker"
  Capability.ManifestWrite -> "Add data to the manifest"
