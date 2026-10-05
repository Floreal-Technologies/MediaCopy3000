module MediaCopy.Plugin.Wire
  ( pluginRef
  , jobInfo
  , fileInfos
  , fieldValues
  , pluginSaid
  , contributionOf
  , mergeContributions
  , inspectFileParams
  , annotationsOf
  , snapshotOf
  , producedFile
  ) where

import Ascmhl.Hash (Hash (..))
import Ascmhl.Path (RelPath, mkRelPath, pathText)
import Ascmhl.Read (isXmlChar, parseFragment)
import Ascmhl.Types (Fragment (..))
import Ascmhl.Types qualified as Mhl
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (Field (..), FieldKind (..), PluginId (..), PluginManifest (..))
import MediaCopy.Plugin.Protocol qualified as P

import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (formatAlgo)
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Plugin.Discovery (Installed (..))

-- $setup
-- >>> import Data.Aeson (Value (..))
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Vector qualified as V
-- >>> import MediaCopy.Plugin.Manifest (Field (..), FieldKind (..))
-- >>> let field key kind = Field {key, label = key, kind, required = True, defaultValue = Nothing}

pluginRef :: Installed -> PluginRef
pluginRef installed = let PluginId raw = installed.manifest.id in PluginRef {id = raw, name = oneLine installed.manifest.name}

jobInfo :: JobPlan -> P.JobInfo
jobInfo plan =
  P.JobInfo
    { kind = display (jobKind plan.spec.job)
    , source = pathText (jobRoot plan.spec.job)
    , destinations = case plan.spec.job of
        Offload _ -> V.map (\target -> pathText target.root) plan.targets
        _ -> V.empty
    , hashFormat = fmap (display . formatAlgo) plan.format
    }

fileInfos :: JobPlan -> Vector P.FileInfo
fileInfos plan = V.map (\step -> P.FileInfo {path = display step.path, size = step.size}) plan.steps

-- |
-- >>> fieldValues (V.fromList [field "camera" (ChoiceField (V.fromList ["A", "B"])), field "dit" TextField]) (Map.fromList [("camera", String "B"), ("dit", String "Jane")])
-- Right (fromList [("camera",String "B"),("dit",String "Jane")])
-- >>> fieldValues (V.singleton (field "camera" (ChoiceField (V.fromList ["A", "B"])))) (Map.singleton "camera" (String "C"))
-- Left "camera"
-- >>> fieldValues (V.singleton (field "offline" BoolField)) (Map.singleton "offline" (String "true"))
-- Right (fromList [("offline",Bool True)])
-- >>> fieldValues (V.singleton (field "token" SecretField)) (Map.singleton "token" (String "abc"))
-- Right (fromList [("token",String "abc")])
-- >>> fieldValues (V.singleton (field "token" SecretField)) Map.empty
-- Left "token"
fieldValues :: Vector Field -> Map Text Value -> Either Text (Map Text Value)
fieldValues fields given = foldl' step (Right Map.empty) fields
  where
    step acc field = do
      values <- acc
      case Map.lookup field.key given >>= accepted field.kind of
        Just value -> Right (Map.insert field.key value values)
        Nothing
          | isJust (Map.lookup field.key given) -> Left field.key
          | Just value <- field.defaultValue >>= accepted field.kind -> Right (Map.insert field.key value values)
          | field.required -> Left field.key
          | otherwise -> Right values
    accepted kind value = case (kind, value) of
      (TextField, String text) -> Just (String text)
      (PathField, String text) -> Just (String text)
      (SecretField, String text) -> Just (String text)
      (BoolField, Bool flag) -> Just (Bool flag)
      (BoolField, String "true") -> Just (Bool True)
      (BoolField, String "false") -> Just (Bool False)
      (ChoiceField choices, String text) | text `elem` choices -> Just (String text)
      _ -> Nothing

pluginSaid :: PluginRef -> Bool -> P.Finding -> PluginFinding
pluginSaid plugin mayBlock finding =
  PluginFinding
    { plugin
    , severity = case finding.severity of
        P.Blocker | mayBlock -> Blocker
        _ -> Warning
    , about = Said PluginSays {key = oneLine finding.key, title = oneLine finding.title, detail = oneLine finding.detail}
    }

-- |
-- >>> let probe = PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> let author name = P.Author {name, email = Nothing, phone = Nothing, role = Just "DIT"}
-- >>> either id (const "accepted") (contributionOf probe "urn:x" mempty P.ContributeResult {authors = V.singleton (author "Jane\SOHDoe"), fileMetadata = V.empty, manifestMetadata = Nothing})
-- "an author holds a character that XML does not allow: \"Jane\\SOHDoe\""
-- >>> either id (const "accepted") (contributionOf probe "urn:x" mempty P.ContributeResult {authors = V.singleton (author "Jane Doe"), fileMetadata = V.empty, manifestMetadata = Nothing})
-- "accepted"
contributionOf :: PluginRef -> Text -> Set RelPath -> P.ContributeResult -> Either Text Contributions
contributionOf plugin namespace jobFiles result = do
  traverse_ xmlText (V.toList result.authors)
  pairs <- traverse metadataOf (V.toList result.fileMetadata)
  manifestMetadata <- traverse (parseFragment namespace) result.manifestMetadata
  Right
    Contributions
      { authors = V.map authorOf result.authors
      , fileMetadata = Map.fromListWith (flip joinFragments) pairs
      , manifestMetadata
      , contributors = V.singleton plugin
      }
  where
    metadataOf entry = case mkRelPath entry.path of
      Just path | Set.member path jobFiles -> (\fragment -> (path, fragment)) <$> parseFragment namespace entry.xml
      _ -> Left ("metadata for a file that is not in the job: " <> oneLine entry.path)
    authorOf author = Mhl.Author {name = oneLine author.name, email = oneLine <$> author.email, phone = oneLine <$> author.phone, role = oneLine <$> author.role}
    xmlText author =
      for_ (author.name : catMaybes [author.email, author.phone, author.role]) $ \text ->
        unless (T.all isXmlChar text) (Left ("an author holds a character that XML does not allow: " <> T.show text))

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

inspectFileParams :: VerifiedFile -> P.InspectFileParams
inspectFileParams file =
  P.InspectFileParams
    { path = display file.path
    , location = pathText file.location
    , hashes = V.map hashValue file.hashes
    , size = file.size
    }

hashValue :: Hash -> P.HashValue
hashValue hash = P.HashValue {algo = display hash.algo, value = hash.value}

annotationsOf :: PluginRef -> P.InspectFileResult -> Vector Annotation
annotationsOf plugin result = V.map (\note -> Annotation {plugin, key = oneLine note.key, label = oneLine note.label, value = oneLine note.value}) result.annotations

snapshotOf :: JobPlan -> JobState -> Map RelPath (Vector Hash) -> JobEvent -> P.Snapshot
snapshotOf plan st hashes terminal =
  P.Snapshot
    { job = jobInfo plan
    , result = case terminal of
        JobFinished _ -> "finished"
        _ -> "failed"
    , failures = case terminal of
        JobFinished (WithFailures n) -> n
        _ -> 0
    , detail = case terminal of
        JobFailed message -> Just message
        _ -> Nothing
    , files = V.fromList (map outcomeOf (Map.toList st.files))
    , manifests = V.map pathText st.mhlPaths
    , findings = V.map coreFinding plan.findings <> V.map reportedPluginFinding (plan.plugins.findings <> st.plugins.warnings)
    , annotations = V.concat [V.map (wireAnnotation path) notes | (path, notes) <- Map.toList st.plugins.annotations]
    , log = fmap pathText st.logPath
    }
  where
    outcomeOf (path, entry) =
      let (status, detail) = statusOf entry.status
      in P.FileOutcome {path = display path, status, detail, hashes = V.map hashValue (Map.findWithDefault V.empty path hashes)}
    statusOf = \case
      Done Ok -> ("verified", Nothing)
      Done (HashMismatch _) -> ("hash-mismatch", Nothing)
      Done Missing -> ("missing", Nothing)
      Done New -> ("new", Nothing)
      Done (IoError message) -> ("io-error", Just message)
      Done (Replaced _) -> ("replaced", Nothing)
      _ -> ("pending", Nothing)
    coreFinding finding =
      P.ReportedFinding {origin = "core", severity = wireSeverity finding.severity, key = T.show finding.code, title = display finding.code, detail = finding.detail}
    wireAnnotation path note =
      P.PluginAnnotation {plugin = PluginId note.plugin.id, path = display path, key = note.key, label = note.label, value = note.value}

reportedPluginFinding :: PluginFinding -> P.ReportedFinding
reportedPluginFinding finding = case finding.about of
  Said says -> P.ReportedFinding {origin = finding.plugin.id, severity = wireSeverity finding.severity, key = says.key, title = says.title, detail = says.detail}
  Faulted fault -> P.ReportedFinding {origin = finding.plugin.id, severity = wireSeverity finding.severity, key = faultKey fault, title = display fault, detail = ""}
  where
    faultKey = \case
      Unavailable _ -> "plugin-unavailable"
      FieldMissing _ -> "plugin-field-missing"
      BadOutput _ -> "plugin-bad-output"

wireSeverity :: Severity -> P.Severity
wireSeverity = \case
  Blocker -> P.Blocker
  Warning -> P.Warning

producedFile :: Artifact -> P.ProducedFile
producedFile artifact =
  P.ProducedFile {plugin = PluginId artifact.plugin.id, path = pathText artifact.path, label = artifact.label, mediaType = artifact.mediaType}
