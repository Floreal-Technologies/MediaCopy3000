module MediaCopy.Demo
  ( Scene (..)
  , scenes
  , sceneNames
  , lookupScene
  ) where

import Ascmhl.Algo qualified as Algo
import Ascmhl.History qualified as History
import Data.Time (addUTCTime)
import Data.Vector qualified as V
import Prelude hiding (first)

import MediaCopy.Demo.Fixtures
import MediaCopy.Domain.FileOutcome qualified as FileOutcome
import MediaCopy.Domain.FileStatus (FileStatus)
import MediaCopy.Domain.FileStatus qualified as FileStatus
import MediaCopy.Domain.Job (Job)
import MediaCopy.Domain.JobEvent qualified as JobEvent
import MediaCopy.Domain.JobId (JobId)
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobResult qualified as JobResult
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Plan (Plan)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Scene (Frame (..))
import MediaCopy.Interface.Theme (Accent (..), Base (..), PaletteMode (..))
import MediaCopy.Model (FileFilter (..), Message (..), Model (..), UiMessage (..), initialModel, update)

data Scene = Scene
  { name :: Text
  , frame :: Frame
  }

scenes :: List Scene
scenes =
  [ still "empty" []
  , still "queue" queueMessages
  , still "offload-dialog" offloadDialogMessages
  , still "plan-planning" [RequestPlan (offloadJob Offload.UseHistory)]
  , still "plan-ready" (offered (offloadJob (Offload.SealBeforeCopy Offload.StopBeforeCopy)) readyPlan)
  , still "plan-partial" (offered (offloadJob Offload.UseHistory) partialPlan)
  , still "plan-blocked" (offered (offloadJob Offload.UseHistory) blockedPlan)
  , tweak (\frame -> frame {scroll = True}) (still "plan-findings" (offered (offloadJob Offload.UseHistory) blockedPlan))
  , still "plan-error" (RequestPlan (offloadJob Offload.UseHistory) : [PlanComputed (specFor first (offloadJob Offload.UseHistory)) (Left "/media/CARD_A001/ascmhl/ascmhl_chain.xml: unexpected end of input")])
  , still "job-running" (runningMessages <> [EngineEvent first (JobEvent.Progress 6_100_000_000), Tick (addUTCTime 2 at), EngineEvent first (JobEvent.Progress 8_640_000_000)])
  , still "job-finished" finishedMessages
  , still "job-failures" failuresMessages
  , still "job-failed-only" (failuresMessages <> [Ui (SetFileFilter FailedOnly)])
  , tweak (\frame -> frame {expand = True}) (still "verify-history" verifyMessages)
  , still "seal-finished" sealMessages
  , tweak (\frame -> frame {action = Just Command.Preferences}) (still "preferences" finishedMessages)
  , tweak (\frame -> frame {action = Just Command.Plugins}) (still "plugins" [CatalogueLoaded pluginCatalogue])
  , tweak (\frame -> frame {scroll = True}) (still "plan-plugins" pluginPlanMessages)
  , still "job-plugins" pluginJobMessages
  , still "close-confirm" (runningMessages <> [EngineEvent first (JobEvent.Progress 8_640_000_000), Ui RequestClose])
  , still "command-palette" (finishedMessages <> [Ui (RunCommand Command.CommandPalette), Ui (SetPaletteQuery "job")])
  , tweak (\frame -> frame {action = Just Command.About}) (still "about" [])
  , still "themes/queue-light" (queueMessages <> [Ui (SetBase AlwaysLight)])
  , still "themes/queue-dark" (queueMessages <> [Ui (SetBase AlwaysDark)])
  ]

sceneNames :: List Text
sceneNames = map (.name) scenes

lookupScene :: Text -> Maybe Scene
lookupScene wanted = find (\scene -> scene.name == wanted) scenes

still :: Text -> List Message -> Scene
still name msgs = Scene {name, frame = Frame {model = play msgs, action = Nothing, expand = False, scroll = False}}

tweak :: (Frame -> Frame) -> Scene -> Scene
tweak change scene = scene {frame = change scene.frame}

play :: List Message -> Model
play msgs = foldl (\model msg -> fst (update msg model)) (initialModel at LightPalette Blue) msgs

offered :: Job -> (JobSpec -> Plan) -> List Message
offered job toPlan = [RequestPlan job, PlanComputed spec (Right (toPlan spec))]
  where
    spec = specFor first job

approved :: JobId -> Job -> (JobSpec -> Plan) -> List Message
approved jid job toPlan =
  [RequestPlan job, PlanComputed spec (Right (toPlan spec)), Ui ConfirmPlan]
  where
    spec = specFor jid job

pluginPlanMessages :: List Message
pluginPlanMessages =
  [ CatalogueLoaded pluginCatalogue
  , RequestPlan (offloadJob Offload.UseHistory)
  , Ui (SetJobField "tech.floreal.credits" "authors.1.name" "Sam Roe")
  , PlanComputed spec (Right (pluginPlan spec))
  ]
  where
    spec = pluginSpec first (offloadJob Offload.UseHistory)

pluginJobMessages :: List Message
pluginJobMessages =
  [ CatalogueLoaded pluginCatalogue
  , RequestPlan (offloadJob Offload.UseHistory)
  , Ui (SetJobField "tech.floreal.credits" "authors.1.name" "Sam Roe")
  , PlanComputed spec (Right (pluginPlan spec))
  , Ui ConfirmPlan
  , EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
  , EngineEvent first (JobEvent.Progress totalBytes)
  ]
    <> map (\index -> statusOf index (FileStatus.Done FileOutcome.Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> map (EngineEvent first . JobEvent.PluginReported) pluginReports
    <> [EngineEvent first (JobEvent.Finished JobResult.AllOk), Ui DismissToast]
  where
    spec = pluginSpec first (offloadJob Offload.UseHistory)

offloadDialogMessages :: List Message
offloadDialogMessages =
  [ Ui (RunCommand Command.NewOffload)
  , SourcePicked mediaSource
  , DestinationPicked shuttle
  , DestinationPicked archive
  ]

runningMessages :: List Message
runningMessages =
  approved first (offloadJob Offload.UseHistory) readyPlan
    <> [ EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (JobEvent.OriginalsResolved "the media source's own history, 7 files" Algo.XXH64)
       , statusOf 0 (FileStatus.Done FileOutcome.Ok)
       , statusOf 1 (FileStatus.Done FileOutcome.Ok)
       , statusOf 2 (FileStatus.Done FileOutcome.Ok)
       , statusOf 3 FileStatus.Copying
       , statusOf 4 FileStatus.Verifying
       ]

finishedMessages :: List Message
finishedMessages =
  approved first (offloadJob Offload.UseHistory) readyPlan
    <> [ EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (JobEvent.OriginalsResolved "the media source's own history, 7 files" Algo.XXH64)
       , EngineEvent first (JobEvent.Progress totalBytes)
       ]
    <> map (\index -> statusOf index (FileStatus.Done FileOutcome.Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first JobEvent.ManifestWriting
       , EngineEvent first (JobEvent.MhlWritten (shuttle `child` "CARD_A001/ascmhl/0003_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first JobEvent.ManifestWriting
       , EngineEvent first (JobEvent.MhlWritten (archive `child` "CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first (JobEvent.Finished JobResult.AllOk)
       , Ui DismissToast
       ]

failuresMessages :: List Message
failuresMessages =
  approved first (offloadJob Offload.UseHistory) readyPlan
    <> [ EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (JobEvent.OriginalsResolved "the media source's own history, 7 files" Algo.XXH64)
       , EngineEvent first (JobEvent.Progress totalBytes)
       , statusOf 0 (FileStatus.Done FileOutcome.Ok)
       , statusOf 1 (FileStatus.Done (FileOutcome.HashMismatch FileOutcome.Mismatch {expected = hashOf "4f9a1c3b2d7e8051", actual = hashOf "8c21b40fa9e3d517"}))
       , statusOf 2 (FileStatus.Done FileOutcome.Ok)
       , statusOf 3 (FileStatus.Done (FileOutcome.IoError "input/output error"))
       , statusOf 4 (FileStatus.Done FileOutcome.Ok)
       , statusOf 5 (FileStatus.Done FileOutcome.Missing)
       , statusOf 6 (FileStatus.Done FileOutcome.New)
       , EngineEvent first (JobEvent.Finished (JobResult.WithFailures 3))
       , Ui DismissToast
       ]

verifyMessages :: List Message
verifyMessages =
  approved first (verifyJob (shuttle `child` "CARD_A001")) verifyPlan
    <> [EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (FileStatus.Done FileOutcome.Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first (JobEvent.Progress totalBytes)
       , EngineEvent first (JobEvent.Finished JobResult.AllOk)
       , HistoryLoaded first history
       , Ui DismissToast
       ]

sealMessages :: List Message
sealMessages =
  approved first (sealJob mediaSource) sealPlan
    <> [EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (FileStatus.Done FileOutcome.Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first (JobEvent.Progress totalBytes)
       , EngineEvent first JobEvent.ManifestWriting
       , EngineEvent first (JobEvent.MhlWritten (mediaSource `child` "ascmhl/0001_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first (JobEvent.Finished JobResult.AllOk)
       , HistoryLoaded first (History.History (V.take 1 history.generations))
       , Ui DismissToast
       ]

queueMessages :: List Message
queueMessages =
  approved first (sealJob mediaSource) sealPlan
    <> [EngineEvent first (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (FileStatus.Done FileOutcome.Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [EngineEvent first (JobEvent.Finished JobResult.AllOk)]
    <> approved (JobId.JobId 2) (offloadFrom (osp "/media/CARD_B002") Offload.UseHistory) readyPlan
    <> [ EngineEvent (JobId.JobId 2) (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent (JobId.JobId 2) (JobEvent.Failed "not enough space: /Volumes/Archive-A/2026-09-12")
       ]
    <> approved (JobId.JobId 3) (offloadFrom (osp "/media/CARD_C003") Offload.UseHistory) readyPlan
    <> [ EngineEvent (JobId.JobId 3) (JobEvent.Planned (JobEvent.PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent (JobId.JobId 3) (JobEvent.Progress 8_640_000_000)
       , EngineEvent (JobId.JobId 3) (JobEvent.FileStatusChanged (fst (mediaSourceFiles V.! 0)) (FileStatus.Done FileOutcome.Ok))
       , EngineEvent (JobId.JobId 3) (JobEvent.FileStatusChanged (fst (mediaSourceFiles V.! 1)) FileStatus.Copying)
       ]
    <> approved (JobId.JobId 4) (verifyJob (archive `child` "CARD_D004")) verifyPlan
    <> [Ui (SelectJob (Just (JobId.JobId 3))), Ui DismissToast]

statusOf :: Int -> FileStatus -> Message
statusOf index status = EngineEvent first (JobEvent.FileStatusChanged (fst (mediaSourceFiles V.! index)) status)

first :: JobId
first = JobId.JobId 1
