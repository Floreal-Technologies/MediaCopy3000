module MediaCopy.Interface.Wording
  ( count
  , humanBytes
  , humanRate
  , humanEta
  , resultText
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

import MediaCopy.Domain.Job (Doing (..), JobKind (..), JobResult (..), JobState (..), isDone, plural)

-- $setup
-- >>> import System.OsPath (unsafeEncodeUtf)

-- |
-- >>> count (3 :: Int)
-- "3"
count :: (Show a) => a -> Text
count n = T.pack (show n)

-- |
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
  | n < 999_500 = count (round (bytes / 1e3) :: Int64) <> " KB"
  | n < 999_950_000 = decimal (bytes / 1e6) <> " MB"
  | n < 999_950_000_000 = decimal (bytes / 1e9) <> " GB"
  | otherwise = decimal (bytes / 1e12) <> " TB"
  where
    bytes = fromIntegral n :: Double

-- |
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

-- |
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

quietFor :: NominalDiffTime
quietFor = 2

quietText :: UTCTime -> JobState -> Maybe Text
quietText now state
  | Just doing <- state.doing
  , unfinished doing
  , quiet >= quietFor =
      Just (display doing <> " · " <> humanEta (realToFrac quiet))
  | otherwise = Nothing
  where
    quiet = diffUTCTime now state.lastMovedAt
    unfinished = \case
      OnFile status -> not (isDone status)
      WritingManifest -> True

decimal :: Double -> Text
decimal x = T.pack (showFFloat (Just 1) x "")

pad2 :: Int -> Text
pad2 n = T.justifyRight 2 '0' (count n)

-- |
-- >>> resultText SealKind AllOk
-- "finished, all files sealed"
-- >>> resultText OffloadKind (WithFailures 1)
-- "finished with 1 failure"
resultText :: JobKind -> JobResult -> Text
resultText kind = \case
  AllOk -> case kind of
    (OffloadKind; VerifyKind) -> "finished, all files verified"
    SealKind -> "finished, all files sealed"
  WithFailures n -> "finished with " <> plural "failure" n

-- |
-- >>> noHistoryText (unsafeEncodeUtf "card")
-- "no ASC MHL history (ascmhl/) in card"
noHistoryText :: OsPath -> Text
noHistoryText folder = "no ASC MHL history (ascmhl/) in " <> pathText folder

data KindUi = KindUi
  { icon :: Text
  , runningVerb :: Text
  , showsProgress :: Bool
  , showsOriginals :: Bool
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
