module MediaCopy.Domain.Plugin
  ( PluginRef (..)
  , PluginFault (..)
  , PluginSays (..)
  , About (..)
  , PluginFinding (..)
  , Contributions (..)
  , noContributions
  , mergeContributions
  , joinFragments
  , PluginPlan (..)
  , noPluginPlan
  , pluginBlockers
  , VerifiedFile (..)
  , Annotation (..)
  , PluginReport (..)
  , PluginState (..)
  , noPluginState
  , foldPluginReport
  , oneLine
  ) where

import Ascmhl.Hash (Hash)
import Ascmhl.Path (RelPath)
import Ascmhl.Types (Author, Fragment (..))
import Control.Applicative ((<|>))
import Data.Char (GeneralCategory (..), generalCategory, isControl)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath)

import MediaCopy.Domain.Severity

-- $setup
-- >>> import Data.Text.Display (display)
-- >>> let probe = PluginRef {id = "tech.floreal.probe", name = "Probe"}

data PluginRef = PluginRef
  { id :: Text
  , name :: Text
  }
  deriving stock (Eq, Ord, Show)

data PluginFault
  = Unavailable Text
  | FieldMissing Text
  | BadOutput Text
  deriving stock (Eq, Ord, Show)

-- |
-- >>> map display [Unavailable "timed out after 30 s", FieldMissing "camera", BadOutput "metadata: <a> is in no namespace, not urn:x"]
-- ["plug-in unavailable \8211 timed out after 30 s","the field camera has no valid value","plug-in sent a bad answer \8211 metadata: <a> is in no namespace, not urn:x"]
instance Display PluginFault where
  displayBuilder = \case
    Unavailable reason -> "plug-in unavailable – " <> displayBuilder reason
    FieldMissing key -> "the field " <> displayBuilder key <> " has no valid value"
    BadOutput reason -> "plug-in sent a bad answer – " <> displayBuilder reason

data PluginSays = PluginSays
  { key :: Text
  , title :: Text
  , detail :: Text
  }
  deriving stock (Eq, Ord, Show)

data About
  = Said PluginSays
  | Faulted PluginFault
  deriving stock (Eq, Ord, Show)

data PluginFinding = PluginFinding
  { plugin :: PluginRef
  , severity :: Severity
  , about :: About
  }
  deriving stock (Eq, Ord, Show)

-- |
-- >>> display PluginFinding {plugin = probe, severity = Warning, about = Said PluginSays {key = "c2pa", title = "2 clips carry Content Credentials", detail = "A001C001.MP4"}}
-- "Probe: 2 clips carry Content Credentials \8211 A001C001.MP4"
-- >>> display PluginFinding {plugin = probe, severity = Blocker, about = Faulted (FieldMissing "camera")}
-- "Probe: the field camera has no valid value"
instance Display PluginFinding where
  displayBuilder finding =
    displayBuilder finding.plugin.name <> ": " <> case finding.about of
      Said says -> displayBuilder says.title <> " – " <> displayBuilder says.detail
      Faulted fault -> displayBuilder fault

data Contributions = Contributions
  { authors :: Vector Author
  , fileMetadata :: Map RelPath Fragment
  , manifestMetadata :: Maybe Fragment
  , contributors :: Vector PluginRef
  }
  deriving stock (Eq, Show)

noContributions :: Contributions
noContributions = Contributions {authors = V.empty, fileMetadata = Map.empty, manifestMetadata = Nothing, contributors = V.empty}

mergeContributions :: Vector Contributions -> Contributions
mergeContributions = foldl' merge noContributions
  where
    merge acc next =
      Contributions
        { authors = acc.authors <> next.authors
        , fileMetadata = Map.unionWith joinFragments acc.fileMetadata next.fileMetadata
        , manifestMetadata = case (acc.manifestMetadata, next.manifestMetadata) of
            (Just earlier, Just later) -> Just (joinFragments earlier later)
            (earlier, later) -> earlier <|> later
        , contributors = acc.contributors <> next.contributors
        }

joinFragments :: Fragment -> Fragment -> Fragment
joinFragments (Fragment earlier) (Fragment later) = Fragment (earlier <> later)

data PluginPlan = PluginPlan
  { active :: Vector PluginRef
  , findings :: Vector PluginFinding
  , contributions :: Contributions
  }
  deriving stock (Eq, Show)

noPluginPlan :: PluginPlan
noPluginPlan = PluginPlan {active = V.empty, findings = V.empty, contributions = noContributions}

pluginBlockers :: PluginPlan -> Vector PluginFinding
pluginBlockers pluginPlan = V.filter (\finding -> finding.severity == Blocker) pluginPlan.findings

data VerifiedFile = VerifiedFile
  { path :: RelPath
  , location :: OsPath
  , hashes :: Vector Hash
  , size :: Int64
  }
  deriving stock (Eq, Show)

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

-- |
-- >>> display (InspectionsLeft 12)
-- "plug-ins: 12 files left to inspect"
-- >>> display (NotInspected probe 3)
-- "plug-in Probe: 3 files not inspected"
-- >>> display (InspectionsLeft 1)
-- "plug-ins: 1 file left to inspect"
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

data PluginState = PluginState
  { annotations :: Map RelPath (Vector Annotation)
  , warnings :: Vector PluginFinding
  , inspectionsLeft :: Int
  , notInspected :: Map PluginRef Int
  }
  deriving stock (Eq, Show)

noPluginState :: PluginState
noPluginState =
  PluginState
    { annotations = Map.empty
    , warnings = V.empty
    , inspectionsLeft = 0
    , notInspected = Map.empty
    }

foldPluginReport :: PluginReport -> PluginState -> PluginState
foldPluginReport report st = case report of
  Annotated path notes -> st {annotations = Map.insertWith (flip (<>)) path notes st.annotations}
  Warned finding -> st {warnings = V.snoc st.warnings finding}
  InspectionsLeft left -> st {inspectionsLeft = left}
  NotInspected plugin left -> st {notInspected = Map.insert plugin left st.notInspected}
  Logged _ _ -> st

-- |
-- >>> oneLine "two\nlines\tand\ESCa\8232separator"
-- "two lines and a separator"
oneLine :: Text -> Text
oneLine = T.map (\c -> if isControl c || generalCategory c `elem` [LineSeparator, ParagraphSeparator] then ' ' else c)
