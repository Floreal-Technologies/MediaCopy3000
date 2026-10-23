{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Interface.Translation.French
  ( languageCode
  , languageName
  , localeFile
  , capitalise
  , pluralCategory
  , formatNumber
  , formatTime
  , decimal
  , subunit
  ) where

import Data.Char qualified as Char
import Data.Scientific
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Language.Fluent.Plural
import Numeric
import System.OsPath

languageCode :: Text
languageCode = "fr"

languageName :: Text
languageName = "Français"

localeFile :: OsPath
localeFile = [osp|locales/fr.ftl|]

capitalise :: Text -> Text
capitalise text = case T.uncons text of
  Just (firstChar, rest) -> T.cons (Char.toUpper firstChar) rest
  Nothing -> text

pluralCategory :: Scientific -> Category
pluralCategory n
  | abs n < 2 = One
  | Right whole <- floatingOrInteger @Double n, whole `rem` 1_000_000 == (0 :: Integer) = Many
  | otherwise = Other

-- |
-- >>> formatNumber 1024
-- "1024"
-- >>> formatNumber 12345
-- "12\8239\&345"
-- >>> formatNumber 1234567.5
-- "1\8239\&234\8239\&567,5"
formatNumber :: Scientific -> Text
formatNumber n = case floatingOrInteger @Double n of
  Right whole -> digits (T.show @Integer whole)
  Left _ -> digits (T.pack (formatScientific Fixed Nothing n))

formatTime :: UTCTime -> Text
formatTime t = T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M" t)

-- |
-- >>> decimal 95.6
-- "95,6"
decimal :: Double -> Text
decimal x = digits (T.pack (showFFloat (Just 1) x ""))

subunit :: Int -> Text
subunit n = T.show n

digits :: Text -> Text
digits text = sign <> grouped whole <> T.replace "." "," fraction
  where
    (sign, unsigned) = T.span (== '-') text
    (whole, fraction) = T.breakOn "." unsigned
    grouped part
      | T.length part < 5 = part
      | otherwise = part & T.reverse & T.chunksOf 3 & T.intercalate "\x202F" & T.reverse
