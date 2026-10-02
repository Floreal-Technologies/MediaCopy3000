module MediaCopy.Interface.Translation.Coverage
  ( wordingFaults
  ) where

import Control.Exception (evaluate, try)
import Data.Containers.ListUtils (nubOrd)
import Data.List (List)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import System.OsPath (unsafeEncodeUtf)

import MediaCopy.Domain.Job
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

counts :: List Int
counts = [0, 1, 3, 1_000_000]
