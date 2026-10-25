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

import MediaCopy.Domain.PluginCatalogue qualified as PluginCatalogue

-- |
-- >>> fmap answerText answers
-- "Not answered" :| ["Granted","Declined"]
answers :: NonEmpty PluginCatalogue.Answer
answers = PluginCatalogue.Unanswered :| [PluginCatalogue.Granted, PluginCatalogue.Declined]

entrySubtitle :: PluginCatalogue.Entry -> Text
entrySubtitle entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList (V.map (display . (.capability)) entry.capabilities)) : maybe [] pure entry.problem)

capabilitySubtitle :: PluginCatalogue.CapabilityView -> Text
capabilitySubtitle view = capabilityText view.capability <> (if view.answer == PluginCatalogue.Unanswered then " – not answered yet" else "")

answerText :: PluginCatalogue.Answer -> Text
answerText = \case
  PluginCatalogue.Unanswered -> "Not answered"
  PluginCatalogue.Granted -> "Granted"
  PluginCatalogue.Declined -> "Declined"

capabilityText :: Plugin.Capability -> Text
capabilityText = \case
  Capability.FilesRead -> "Read the media files"
  Capability.PlanInspect -> "Add findings to the plan"
  Capability.FilesInspect -> "Add notes about each verified file to the report"
  Capability.Block -> "Stop a job with a blocker"
  Capability.ManifestWrite -> "Add data to the manifest"
  Capability.JobFinished -> "Act when a job ends"
