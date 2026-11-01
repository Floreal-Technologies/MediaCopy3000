module MediaCopy.Plugin.Wire
  ( pluginRef
  , jobInfo
  , fileInfos
  , pluginSaid
  , contributionOf
  , inspectFileParams
  , annotationsOf
  , jobFinishedParams
  ) where

import Ascmhl.Author qualified as Author
import Ascmhl.Fragment qualified as Fragment
import Ascmhl.Hash (Hash)
import Ascmhl.Hash qualified as Hash
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Manifest qualified as Manifest
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.FileInfo qualified as FileInfo
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.Finding qualified as Finding
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.HashValue qualified as HashValue
import MediaCopy.Plugin.Protocol.HashValue qualified as Protocol (HashValue)
import MediaCopy.Plugin.Protocol.InspectFile qualified as InspectFile
import MediaCopy.Plugin.Protocol.JobFinished qualified as JobFinished
import MediaCopy.Plugin.Protocol.JobInfo qualified as JobInfo
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)

import MediaCopy.Domain.Contributions (Contributions)
import MediaCopy.Domain.Contributions qualified as Contributions
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobEnd (JobEnd)
import MediaCopy.Domain.JobEnd qualified as JobEnd
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Domain.Plan qualified as Plan
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PluginFinding (PluginFinding)
import MediaCopy.Domain.PluginFinding qualified as PluginFinding
import MediaCopy.Domain.PluginRef (PluginRef)
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Domain.PluginReport qualified as PluginReport
import MediaCopy.Domain.Prose qualified as Prose
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Domain.VerifiedFile (VerifiedFile)
import MediaCopy.Domain.VerifiedFile qualified as VerifiedFile
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Installed qualified as Plugin (Installed)

pluginRef :: Plugin.Installed -> PluginRef
pluginRef installed = let Id.Id raw = installed.manifest.id in PluginRef.PluginRef {id = raw, name = Prose.oneLine installed.manifest.name}

jobInfo :: Plan -> Protocol.JobInfo
jobInfo plan =
  JobInfo.JobInfo
    { kind = display (Job.kind plan.spec.job)
    , source = OsPath.toText (Job.root plan.spec.job)
    , destinations = case plan.spec.job of
        Job.Offload _ -> V.map (\target -> OsPath.toText target.root) plan.targets
        _ -> V.empty
    , hashFormat = fmap display plan.format
    }

jobFinishedParams :: Plan -> JobEnd -> JobFinished.Params
jobFinishedParams plan jobEnd =
  JobFinished.Params
    { job = jobInfo plan
    , outcome = case jobEnd.result of
        Left message -> JobFinished.Failed message
        Right JobResult.AllOk -> JobFinished.AllOk
        Right (JobResult.WithFailures count) -> JobFinished.WithFailures count
    , manifests = V.map (\written -> JobFinished.ManifestInfo {destination = OsPath.toText written.folder, path = OsPath.toText written.path}) jobEnd.manifests
    }

fileInfos :: Plan -> Vector Protocol.FileInfo
fileInfos plan = V.map (\step -> FileInfo.FileInfo {path = display step.path, size = step.size}) plan.steps

pluginSaid :: PluginRef -> Bool -> Protocol.Finding -> PluginFinding
pluginSaid plugin mayBlock finding =
  PluginFinding.PluginFinding
    { plugin
    , severity = case finding.severity of
        Finding.Blocker | mayBlock -> Severity.Blocker
        _ -> Severity.Warning
    , about = PluginFinding.Said PluginFinding.Says {key = Prose.oneLine finding.key, title = Prose.oneLine finding.title, detail = Prose.oneLine finding.detail}
    }

contributionOf :: PluginRef -> Text -> Set RelPath -> Contribute.Result -> Either Text Contributions
contributionOf plugin namespace jobFiles result = do
  traverse_ xmlText (V.toList result.authors)
  pairs <- traverse metadataOf (V.toList result.fileMetadata)
  manifestMetadata <- traverse (Fragment.parse namespace) result.manifestMetadata
  Right
    Contributions.Contributions
      { authors = V.map authorOf result.authors
      , fileMetadata = Map.fromListWith (flip Contributions.joinFragments) pairs
      , manifestMetadata
      , contributors = V.singleton plugin
      }
  where
    metadataOf entry = case RelPath.parse entry.path of
      Just path | Set.member path jobFiles -> (\fragment -> (path, fragment)) <$> Fragment.parse namespace entry.xml
      _ -> Left ("metadata for a file that is not in the job: " <> Prose.oneLine entry.path)
    authorOf author = Author.Author {name = Prose.oneLine author.name, email = Prose.oneLine <$> author.email, phone = Prose.oneLine <$> author.phone, role = Prose.oneLine <$> author.role}
    xmlText author =
      for_ (author.name : catMaybes [author.email, author.phone, author.role]) $ \text ->
        unless (T.all Fragment.isXmlChar text) (Left ("an author holds a character that XML does not allow: " <> T.show text))

inspectFileParams :: VerifiedFile -> InspectFile.Params
inspectFileParams file =
  InspectFile.Params
    { path = display file.path
    , location = OsPath.toText file.location
    , hashes = V.map hashValue file.hashes
    , size = file.size
    }

hashValue :: Hash -> Protocol.HashValue
hashValue hash = HashValue.HashValue {algo = display hash.algo, value = hash.value}

annotationsOf :: PluginRef -> InspectFile.Result -> Vector PluginReport.Annotation
annotationsOf plugin result = V.map (\note -> PluginReport.Annotation {plugin, key = Prose.oneLine note.key, label = Prose.oneLine note.label, value = Prose.oneLine note.value}) result.annotations
