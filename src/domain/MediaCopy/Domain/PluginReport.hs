module MediaCopy.Domain.PluginReport
  ( PluginReport (..)
  , Annotation (..)
  ) where

import Ascmhl.RelPath (RelPath)
import Data.Text qualified as T
import Data.Vector qualified as V

import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef

data Annotation = Annotation
  { plugin :: PluginRef
  , key :: Text
  , label :: Text
  , value :: Text
  }
  deriving stock (Eq, Show)

data PluginReport
  = Annotated RelPath (Vector Annotation)
  | Warned PluginFinding
  | InspectionsLeft Int
  | NotInspected PluginRef Int
  | Logged PluginRef Text
  deriving stock (Eq, Show)

instance Display PluginReport where
  displayBuilder = \case
    Annotated path notes ->
      "plug-ins: " <> displayBuilder path <> " " <> displayBuilder (T.intercalate ", " (V.toList (V.map (\note -> note.label <> "=" <> note.value) notes)))
    Warned finding -> "plug-in warning " <> displayBuilder finding
    InspectionsLeft left -> "plug-ins: " <> displayBuilder (files left) <> " left to inspect"
    NotInspected plugin left -> "plug-in " <> displayBuilder plugin.name <> ": " <> displayBuilder (files left) <> " not inspected"
    Logged plugin line -> "plug-in " <> displayBuilder plugin.name <> ": " <> displayBuilder line
    where
      files n = if n == 1 then "1 file" else T.show n <> " files"
