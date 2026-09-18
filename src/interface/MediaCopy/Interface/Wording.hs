-- | The words the operator's side puts on a number or a job kind.
--
-- A phrase that both a widget and the report say lives here, so the two cannot drift. A phrase that
-- reads differently in the two places is not one phrase, and stays with its own renderer.
module MediaCopy.Interface.Wording
  ( count
  , humanBytes
  , humanRate
  , humanEta
  , generationsText
  , noHistoryText
  , quietText
  , KindUi (..)
  , kindUi
  ) where

import Ascmhl.Path (pathText)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Numeric (showFFloat)
import System.OsPath (OsPath)

import MediaCopy.Domain.Job (Doing (..), JobKind (..), JobState (..), isDone)

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

-- | A number in a sentence. The one spelling of 'show' for the operator's side.
--
-- >>> count (3 :: Int)
-- "3"
count :: (Show a) => a -> Text
count n = T.pack (show n)

-- | SI units, one decimal from MB up.
--
-- >>> humanBytes 0
-- "0 B"
-- >>> humanBytes 999
-- "999 B"
-- >>> humanBytes 1_000
-- "1 KB"
-- >>> humanBytes 12_288
-- "12 KB"
-- >>> humanBytes 999_499
-- "999 KB"
-- >>> humanBytes 999_500
-- "1.0 MB"
-- >>> humanBytes 95_600_000_000
-- "95.6 GB"
-- >>> humanBytes 2_500_000_000_000
-- "2.5 TB"
humanBytes :: Int64 -> Text
humanBytes n
  | n < 1_000 = count n <> " B"
  | n < 999_500 = whole (bytes / 1e3) <> " KB"
  | n < 999_950_000 = decimal (bytes / 1e6) <> " MB"
  | n < 999_950_000_000 = decimal (bytes / 1e9) <> " GB"
  | otherwise = decimal (bytes / 1e12) <> " TB"
  where
    -- Each threshold is the value where the rounded figure reads 1000 of its unit.
    bytes = fromIntegral n :: Double

-- | Bytes per second. A rate that is not a positive number reads as nothing moving.
--
-- >>> humanRate 1.1e9
-- "1.1 GB/s"
-- >>> humanRate 0
-- "0 B/s"
-- >>> humanRate (0 / 0)
-- "0 B/s"
humanRate :: Double -> Text
humanRate bytesPerSecond
  | not (isNormalPositive bytesPerSecond) = "0 B/s"
  | otherwise = humanBytes (round bytesPerSecond) <> "/s"

-- | Seconds.
--
-- >>> humanEta 34
-- "34 s"
-- >>> humanEta 125
-- "2 min 05 s"
-- >>> humanEta 3_720
-- "1 h 02 min"
-- >>> humanEta (-1)
-- "0 s"
humanEta :: Double -> Text
humanEta seconds
  | not (isNormalPositive seconds) = "0 s"
  | total < 60 = count total <> " s"
  | total < 3_600 = count (total `div` 60) <> " min " <> pad2 (total `mod` 60) <> " s"
  | otherwise = count (total `div` 3_600) <> " h " <> pad2 (total `mod` 3_600 `div` 60) <> " min"
  where
    total = round seconds :: Int

isNormalPositive :: Double -> Bool
isNormalPositive x = not (isNaN x) && not (isInfinite x) && x > 0

-- | How long a job may give no sign of movement before the interface says so. The clock ticks once a
-- second, so one missed tick is jitter and two is a fact.
quietFor :: NominalDiffTime
quietFor = 2

-- | What a job says in place of a rate when nothing moved for 'quietFor': the state of the file it
-- holds, and how long it has been in it. 'Nothing' while the job moves, which is the ordinary case.
-- A phase that reads and writes no byte has no rate to measure, and a spinner cannot tell a stopped
-- disk from a busy one, so the state and its age are the only honest things to say. A job with no
-- file in hand also says nothing, whatever its clock reads, because the work between two files has
-- no name this line can give.
quietText :: UTCTime -> JobState -> Maybe Text
quietText now state
  | Just doing <- state.doing
  , unfinished doing
  , quiet >= quietFor =
      Just (display doing <> " · " <> humanEta (realToFrac quiet))
  | otherwise = Nothing
  where
    quiet = diffUTCTime now state.lastMovedAt
    -- A file that reached its outcome is work the job has let go of. The manifest phase has no such
    -- outcome: 'foldEvent' clears it when the job ends, so while it stands it is in hand.
    unfinished = \case
      OnFile status -> not (isDone status)
      WritingManifest -> True

whole :: Double -> Text
whole x = count (round x :: Int64)

decimal :: Double -> Text
decimal x = T.pack (showFFloat (Just 1) x "")

pad2 :: Int -> Text
pad2 n = T.justifyRight 2 '0' (count n)

-- | Pluralised generation count. The history expander and the detail pane's path line both use it.
--
-- >>> generationsText 1
-- "1 generation"
-- >>> generationsText 4
-- "4 generations"
generationsText :: Int -> Text
generationsText 1 = "1 generation"
generationsText n = count n <> " generations"

-- | What the operator reads about a folder that holds no ASC MHL history yet. This is not a fault,
-- so it names no remedy.
--
-- >>> noHistoryText (unsafeEncodeUtf "card")
-- "no ASC MHL history (ascmhl/) in card"
noHistoryText :: OsPath -> Text
noHistoryText folder = "no ASC MHL history (ascmhl/) in " <> pathText folder

-- | How a job kind looks and what it has to say. A new kind is a new row here, not a new
-- branch in the sidebar, the detail pane and the header bar.
data KindUi = KindUi
  { icon :: Text
  -- ^ A named icon of the desktop's theme.
  , runningVerb :: Text
  , showsProgress :: Bool
  -- ^ A running offload reads out a percentage and a rate. A job that only hashes has neither.
  , showsOriginals :: Bool
  -- ^ Only an offload consults originals, so only an offload has a line about them.
  }

-- | >>> (kindUi OffloadKind).runningVerb
-- "Copying"
-- >>> (kindUi VerifyKind).showsProgress
-- False
kindUi :: JobKind -> KindUi
kindUi = \case
  OffloadKind ->
    KindUi {icon = "folder-download-symbolic", runningVerb = "Copying", showsProgress = True, showsOriginals = True}
  VerifyKind ->
    KindUi {icon = "emblem-ok-symbolic", runningVerb = "Verifying", showsProgress = False, showsOriginals = False}
  SealKind ->
    KindUi {icon = "channel-secure-symbolic", runningVerb = "Sealing", showsProgress = False, showsOriginals = False}
