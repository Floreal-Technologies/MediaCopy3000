module MediaCopy.Demo
  ( Scene (..)
  , scenes
  , sceneNames
  , lookupScene
  ) where

import Ascmhl.Hash (HashAlgo (..))
import Ascmhl.Types (MhlHistory (..))
import Data.Function ((&))
import Data.List (List)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Demo.Fixtures
import MediaCopy.Domain.Job
import MediaCopy.Domain.Plan (JobPlan)
import MediaCopy.Interface.Theme (Base (..), Palette (..), PaletteMode (..), Theme (..), modeDirectory, palettesFrom)
import MediaCopy.Model (FileFilter (..), Message (..), Model (..), UiMessage (..), initialModel, update)

data Scene = Scene
  { name :: Text
  , frames :: NonEmpty Model
  , action :: Maybe Text
  , expand :: Bool
  , scroll :: Bool
  }

scenes :: List Scene
scenes =
  [ still "empty" []
  , still "queue" queueMessages
  , still "offload-dialog" offloadDialogMessages
  , still "plan-planning" [RequestPlan (offloadJob UseHistory)]
  , still "plan-ready" (offered (offloadJob (SealBeforeCopy StopBeforeCopy)) readyPlan)
  , still "plan-partial" (offered (offloadJob UseHistory) partialPlan)
  , still "plan-blocked" (offered (offloadJob UseHistory) blockedPlan)
  , (still "plan-findings" (offered (offloadJob UseHistory) blockedPlan)) {scroll = True}
  , still "plan-error" (RequestPlan (offloadJob UseHistory) : [PlanComputed (specFor first (offloadJob UseHistory)) (Left planErrorText)])
  , running "job-running" runningMessages
  , still "job-finished" finishedMessages
  , still "job-failures" failuresMessages
  , still "job-failed-only" (failuresMessages <> [Ui (SetFileFilter FailedOnly)])
  , (still "verify-history" verifyMessages) {expand = True}
  , still "seal-finished" sealMessages
  , (still "preferences" finishedMessages) {action = Just "app.preferences"}
  , still "close-confirm" (runningMessages <> [EngineEvent first (Progress 8_640_000_000), Ui RequestClose])
  , (still "about" []) {action = Just "app.about"}
  ]
    <> map queueUnder (V.toList demoPalettes)

queueUnder :: Palette -> Scene
queueUnder palette =
  still
    ("themes/queue-" <> palette.family <> "-" <> modeDirectory palette.mode <> "-" <> palette.variant)
    (queueMessages <> [Ui (SetBase (baseFor palette.mode)), Ui (SetPalette (PaletteTheme palette))])

baseFor :: PaletteMode -> Base
baseFor = \case
  DarkPalette -> AlwaysDark
  LightPalette -> AlwaysLight

demoPalettes :: Vector Palette
demoPalettes = palettesFrom themeRoot paletteListing

themeRoot :: FilePath
themeRoot = "assets/themes"

sceneNames :: List Text
sceneNames = map (\scene -> scene.name) scenes

lookupScene :: Text -> Maybe Scene
lookupScene wanted = scenes & filter (\scene -> scene.name == wanted) & headOrNothing
  where
    headOrNothing = \case
      [] -> Nothing
      x : _ -> Just x

still :: Text -> List Message -> Scene
still name msgs = Scene {name, frames = NE.singleton (play msgs), action = Nothing, expand = False, scroll = False}

running :: Text -> List Message -> Scene
running name msgs =
  Scene
    { name
    , frames = play (msgs <> earlier) :| [play (msgs <> earlier), play (msgs <> later)]
    , action = Nothing
    , expand = False
    , scroll = False
    }
  where
    earlier = [EngineEvent first (Progress 6_100_000_000)]
    later = [Tick (addUTCTime 2 at), EngineEvent first (Progress 8_640_000_000)]

play :: List Message -> Model
play msgs = foldl (\model msg -> fst (update msg model)) (initialModel at LightPalette) msgs

offered :: Job -> (JobSpec -> JobPlan) -> List Message
offered job toPlan = [RequestPlan job, PlanComputed spec (Right (toPlan spec))]
  where
    spec = specFor first job

approved :: JobId -> Job -> (JobSpec -> JobPlan) -> List Message
approved jid job toPlan =
  [RequestPlan job, PlanComputed spec (Right (toPlan spec)), Ui ConfirmPlan]
  where
    spec = specFor jid job

offloadDialogMessages :: List Message
offloadDialogMessages =
  [ Ui OpenOffloadDialog
  , SourcePicked mediaSource
  , DestinationPicked shuttle
  , DestinationPicked archive
  ]

runningMessages :: List Message
runningMessages =
  approved first (offloadJob UseHistory) readyPlan
    <> [ EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (OriginalsResolved "the media source's own history, 7 files" XXH64)
       , statusOf 0 (Done Ok)
       , statusOf 1 (Done Ok)
       , statusOf 2 (Done Ok)
       , statusOf 3 Copying
       , statusOf 4 Verifying
       ]

finishedMessages :: List Message
finishedMessages =
  approved first (offloadJob UseHistory) readyPlan
    <> [ EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (OriginalsResolved "the media source's own history, 7 files" XXH64)
       , EngineEvent first (Progress totalBytes)
       ]
    <> map (\index -> statusOf index (Done Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first ManifestWriting
       , EngineEvent first (MhlWritten (shuttle `child` "CARD_A001/ascmhl/0003_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first ManifestWriting
       , EngineEvent first (MhlWritten (archive `child` "CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first (JobFinished AllOk)
       , Ui DismissToast
       ]

failuresMessages :: List Message
failuresMessages =
  approved first (offloadJob UseHistory) readyPlan
    <> [ EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent first (OriginalsResolved "the media source's own history, 7 files" XXH64)
       , EngineEvent first (Progress totalBytes)
       , statusOf 0 (Done Ok)
       , statusOf 1 (Done (HashMismatch Mismatch {expected = hashOf "4f9a1c3b2d7e8051", actual = hashOf "8c21b40fa9e3d517"}))
       , statusOf 2 (Done Ok)
       , statusOf 3 (Done (IoError "input/output error"))
       , statusOf 4 (Done Ok)
       , statusOf 5 (Done Missing)
       , statusOf 6 (Done New)
       , EngineEvent first (JobFinished (WithFailures 3))
       , Ui DismissToast
       ]

verifyMessages :: List Message
verifyMessages =
  approved first (verifyJob (shuttle `child` "CARD_A001")) verifyPlan
    <> [EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (Done Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first (Progress totalBytes)
       , EngineEvent first (JobFinished AllOk)
       , HistoryLoaded first history
       , Ui DismissToast
       ]

sealMessages :: List Message
sealMessages =
  approved first (sealJob mediaSource) sealPlan
    <> [EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (Done Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [ EngineEvent first (Progress totalBytes)
       , EngineEvent first ManifestWriting
       , EngineEvent first (MhlWritten (mediaSource `child` "ascmhl/0001_CARD_A001_2026-09-12_140300.mhl"))
       , EngineEvent first (JobFinished AllOk)
       , HistoryLoaded first (MhlHistory (V.take 1 history.generations))
       , Ui DismissToast
       ]

queueMessages :: List Message
queueMessages =
  approved first (sealJob mediaSource) sealPlan
    <> [EngineEvent first (Planned (PlannedWork mediaSourceFiles totalBytes))]
    <> map (\index -> statusOf index (Done Ok)) [0 .. V.length mediaSourceFiles - 1]
    <> [EngineEvent first (JobFinished AllOk)]
    <> approved (JobId 2) (offloadFrom (osp "/media/CARD_B002") UseHistory) readyPlan
    <> [ EngineEvent (JobId 2) (Planned (PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent (JobId 2) (JobFailed "not enough space: /Volumes/Archive-A/2026-09-12")
       ]
    <> approved (JobId 3) (offloadFrom (osp "/media/CARD_C003") UseHistory) readyPlan
    <> [ EngineEvent (JobId 3) (Planned (PlannedWork mediaSourceFiles totalBytes))
       , EngineEvent (JobId 3) (Progress 8_640_000_000)
       , EngineEvent (JobId 3) (FileStatusChanged (fst (mediaSourceFiles V.! 0)) (Done Ok))
       , EngineEvent (JobId 3) (FileStatusChanged (fst (mediaSourceFiles V.! 1)) Copying)
       ]
    <> approved (JobId 4) (verifyJob (archive `child` "CARD_D004")) verifyPlan
    <> [Ui (SelectJob (Just (JobId 3))), Ui DismissToast]

statusOf :: Int -> FileStatus -> Message
statusOf index status = EngineEvent first (FileStatusChanged (fst (mediaSourceFiles V.! index)) status)

first :: JobId
first = JobId 1

planErrorText :: Text
planErrorText = "/media/CARD_A001/ascmhl/ascmhl_chain.xml: unexpected end of input"
