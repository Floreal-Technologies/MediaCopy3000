module MediaCopy.Plugin.Wire
  ( pluginRef
  , jobInfo
  , fileInfos
  , pluginSaid
  , contributionOf
  , inspectFileParams
  , annotationsOf
  ) where

import Ascmhl.Hash (Hash (..))
import Ascmhl.Path (RelPath, mkRelPath, pathText)
import Ascmhl.Read (isXmlChar, parseFragment)
import Ascmhl.Types qualified as Mhl
import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (PluginId (..), PluginManifest (..))
import MediaCopy.Plugin.Protocol qualified as P

import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Plugin.Discovery (Installed (..))

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
    , hashFormat = fmap display plan.format
    }

fileInfos :: JobPlan -> Vector P.FileInfo
fileInfos plan = V.map (\step -> P.FileInfo {path = display step.path, size = step.size}) plan.steps

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
