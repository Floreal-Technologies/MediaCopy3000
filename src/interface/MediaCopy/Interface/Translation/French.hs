{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Interface.Translation.French
  ( languageCode
  , languageName
  , localeFile
  , capitalise
  , displayLanguage
  , pluralCategory
  , formatNumber
  , formatTime
  , decimal
  ) where

import Data.Char qualified as Char
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Language.Fluent.Plural
import System.OsPath
import Numeric

languageCode :: Text
languageCode = "fr"

languageName :: Text
languageName = "Français"

localeFile :: OsPath
localeFile = [osp|locales/fr.ftl|]

displayLanguage :: Maybe Text
displayLanguage = Nothing

capitalise :: Text -> Text
capitalise text = case T.uncons text of
  Just (firstChar, rest) -> T.cons (Char.toUpper firstChar) rest
  Nothing -> text

pluralCategory :: Scientific -> Category
pluralCategory n
  | abs n < 2 = One
  | Right whole <- floatingOrInteger @Double n, whole `rem` 1_000_000 == (0 :: Integer) = Many
  | otherwise = Other

formatNumber :: Scientific -> Text
formatNumber n = case floatingOrInteger @Double n of
  Right whole -> T.show @Integer whole
  Left _ -> T.pack (formatScientific Fixed Nothing n)

formatTime :: UTCTime -> Text
formatTime t = T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M" t)

decimal :: Double -> Text
decimal x = T.replace "." "," (T.pack (showFFloat (Just 1) x ""))
