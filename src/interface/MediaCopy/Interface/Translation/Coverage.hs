module MediaCopy.Interface.Translation.Coverage
  ( wordingFaults
  ) where

import Ascmhl.Hash
import Control.Exception (evaluate, try)
import Data.Containers.ListUtils (nubOrd)
import Data.List (List)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.OsPath (unsafeEncodeUtf)

import MediaCopy.Domain.Job
import MediaCopy.Interface.Theme
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Wording

wordingFaults :: Wording -> IO (List Text)
wordingFaults wording = nubOrd . catMaybes <$> traverse attempt (expectedWording wording {strict = True})
  where
    attempt text =
      try (evaluate (T.length text)) >>= \case
        Left (WordingFault fault) -> pure (Just fault)
        Right _ -> pure Nothing

expectedWording :: Wording -> List Text
expectedWording wording =
  concatMap (\kind -> List.map (resultText wording kind) (AllOk : map WithFailures counts)) [minBound @JobKind ..]
    <> [noHistoryText wording (unsafeEncodeUtf "card")]
    <> map (humanBytes wording) [0, 12_288, 999_500, 95_600_000_000, 2_500_000_000_000]
    <> map (humanRate wording) [0, 1.1e9]
    <> map (humanEta wording) [34, 125, 3_720]
    <> map (runningVerbText wording) [minBound ..]
    <> map (\doing -> quietLine wording doing 25) (WritingManifest : map OnFile fileStatuses)
    <> map (jobKindText wording) [minBound ..]
    <> map (writeModeText wording) [minBound ..]
    <> map (processKindText wording) [minBound ..]
    <> map (targetStateText wording) [minBound ..]
    <> map (findingText wording) [minBound ..]
    <> map (displayBase wording) [minBound ..]
    <> map (\mode -> themeRowLabel wording (SystemTheme mode)) [minBound ..]
    <> map (\section -> section.heading) (V.toList (themeSections wording minBound V.empty))

fileStatuses :: List FileStatus
fileStatuses =
  [Pending, Hashing, Copying, Flushing, Publishing, Verifying]
    <> List.map Done [Ok, HashMismatch mismatch, Missing, New, IoError "disk full", Replaced mismatch]
  where
    mismatch = Mismatch {expected = hash, actual = hash}
    hash = Hash {algo = XXH64, value = "0"}

counts :: List Int
counts = [0, 1, 3, 1_000_000]
