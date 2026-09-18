module Ascmhl.Time
  ( parse
  , render
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM, zonedTimeToUTC)

parse :: Text -> Maybe UTCTime
parse t =
  zonedTimeToUTC <$> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" (T.unpack (fixZ t))
  where
    fixZ s = if "Z" `T.isSuffixOf` s then T.dropEnd 1 s <> "+00:00" else s

render :: UTCTime -> Text
render t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" t)
