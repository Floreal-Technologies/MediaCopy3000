module MediaCopy.Interface.View.Plugins
  ( entrySubtitle
  , capabilitySubtitle
  , capabilityText
  , answerIndex
  , answerAt
  ) where

import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)

import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog

-- |
-- >>> map answerAt [0, 1, 2, 3]
-- [Just Unanswered,Just Granted,Just Declined,Nothing]
-- >>> map (answerAt . answerIndex) [PluginCatalog.Unanswered, PluginCatalog.Granted, PluginCatalog.Declined]
-- [Just Unanswered,Just Granted,Just Declined]
answerAt :: Word32 -> Maybe PluginCatalog.Answer
answerAt = \case
  0 -> Just PluginCatalog.Unanswered
  1 -> Just PluginCatalog.Granted
  2 -> Just PluginCatalog.Declined
  _ -> Nothing

entrySubtitle :: PluginCatalog.Entry -> Text
entrySubtitle entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList (V.map (Capability.toText . (.capability)) entry.capabilities)) : maybe [] pure entry.problem)

capabilitySubtitle :: PluginCatalog.CapabilityView -> Text
capabilitySubtitle view = capabilityText view.capability <> (if view.answer == PluginCatalog.Unanswered then " – not answered yet" else "")

answerIndex :: PluginCatalog.Answer -> Word32
answerIndex = \case
  PluginCatalog.Unanswered -> 0
  PluginCatalog.Granted -> 1
  PluginCatalog.Declined -> 2

capabilityText :: Plugin.Capability -> Text
capabilityText = \case
  Capability.FilesRead -> "Read the media files"
  Capability.PlanInspect -> "Add findings to the plan"
  Capability.FilesInspect -> "Add notes about each verified file to the report"
  Capability.Block -> "Stop a job with a blocker"
  Capability.ManifestWrite -> "Add data to the manifest"
