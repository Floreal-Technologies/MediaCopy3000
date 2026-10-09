{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Interface.Translation.English
  ( languageCode
  , languageName
  , localeFile
  , capitalise
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
import Numeric
import System.OsPath

languageCode :: Text
languageCode = "en"

languageName :: Text
languageName = "English"

localeFile :: OsPath
localeFile = [osp|locales/en.ftl|]

capitalise :: Text -> Text
capitalise text = case T.uncons text of
  Just (firstChar, rest) -> T.cons (Char.toUpper firstChar) rest
  Nothing -> text

pluralCategory :: Scientific -> Category
pluralCategory n
  | n == 1 = One
  | otherwise = Other

formatNumber :: Scientific -> Text
formatNumber n = case floatingOrInteger @Double n of
  Right whole -> T.show @Integer whole
  Left _ -> T.pack (formatScientific Fixed Nothing n)

formatTime :: UTCTime -> Text
formatTime t = T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M" t)

decimal :: Double -> Text
decimal x = T.pack (showFFloat (Just 1) x "")
