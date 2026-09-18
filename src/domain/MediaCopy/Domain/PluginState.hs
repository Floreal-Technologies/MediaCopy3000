module MediaCopy.Domain.PluginState
  ( PluginState (..)
  , empty
  , apply
  ) where

import Ascmhl.RelPath (RelPath)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginReport (PluginReport)
import MediaCopy.Domain.PluginReport qualified as PluginReport

data PluginState = PluginState
  { annotations :: Map RelPath (Vector PluginReport.Annotation)
  , warnings :: Vector PluginFinding
  , inspectionsLeft :: Int
  , notInspected :: Map PluginRef Int
  }
  deriving stock (Eq, Show)

empty :: PluginState
empty =
  PluginState
    { annotations = Map.empty
    , warnings = V.empty
    , inspectionsLeft = 0
    , notInspected = Map.empty
    }

apply :: PluginReport -> PluginState -> PluginState
apply report st = case report of
  PluginReport.Annotated path notes -> st {annotations = Map.insertWith (flip (<>)) path notes st.annotations}
  PluginReport.Warned finding -> st {warnings = V.snoc st.warnings finding}
  PluginReport.InspectionsLeft left -> st {inspectionsLeft = left}
  PluginReport.NotInspected plugin left -> st {notInspected = Map.insert plugin left st.notInspected}
  PluginReport.Logged _ _ -> st
