module MediaCopy.Interface.View.Plugins
  ( entrySubtitle
  , capabilitySubtitle
  , capabilityText
  , answerIndex
  , answerAt
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)

import MediaCopy.Domain.PluginCatalog

-- |
-- >>> map answerAt [0, 1, 2, 3]
-- [Just Unanswered,Just Granted,Just Declined,Nothing]
-- >>> map (answerAt . answerIndex) [Unanswered, Granted, Declined]
-- [Just Unanswered,Just Granted,Just Declined]
answerAt :: Word32 -> Maybe Answer
answerAt = \case
  0 -> Just Unanswered
  1 -> Just Granted
  2 -> Just Declined
  _ -> Nothing

entrySubtitle :: PluginEntry -> Text
entrySubtitle entry = T.intercalate " · " (entry.version : T.intercalate ", " (V.toList (V.map (.name) entry.capabilities)) : maybe [] pure entry.problem)

capabilitySubtitle :: CapabilityView -> Text
capabilitySubtitle capability = capabilityText capability.name <> (if capability.answer == Unanswered then " – not answered yet" else "")

answerIndex :: Answer -> Word32
answerIndex = \case
  Unanswered -> 0
  Granted -> 1
  Declined -> 2

capabilityText :: Text -> Text
capabilityText = \case
  "files.read" -> "Read the media files"
  "plan.inspect" -> "Add findings to the plan"
  "files.inspect" -> "Add notes about each verified file to the report"
  "block" -> "Stop a job with a blocker"
  "manifest.write" -> "Add data to the manifest"
  other -> other
