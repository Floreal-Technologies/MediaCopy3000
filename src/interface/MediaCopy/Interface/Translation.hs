module MediaCopy.Interface.Translation
  ( SupportedLanguage (..)
  , parseFtl
  , Wording (..)
  , mkWording
  , WordingFault (..)
  , getTranslation
  , localeFile
  , languageCode
  , str
  ) where

import Control.Exception (Exception, displayException, throw)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Language.Fluent
import Language.Fluent.AST (Identifier (..))
import Language.Fluent.Bundle qualified as Bundle
import Language.Fluent.Parser (rejectJunk)
import Language.Fluent.Value qualified as Value

import MediaCopy.Interface.Translation.English qualified as English
import MediaCopy.Interface.Translation.French qualified as French

data SupportedLanguage
  = English
  | French
  deriving stock (Eq, Show, Ord, Bounded, Enum)

instance Locale SupportedLanguage where
  fromCode code = Map.lookup code supportedLanguages
  toCode = languageCode
  displayLanguage namedLanguage _ =
    case namedLanguage of
      English -> Just English.languageName
      French -> Just French.languageName
  capitalise = \case
    English -> English.capitalise
    French -> French.capitalise
  pluralCategory lang n _ = case lang of
    English -> English.pluralCategory n
    French -> French.pluralCategory n
  formatNumber ls n _ = case NonEmpty.head ls of
    English -> Right (English.formatNumber n)
    French -> Right (French.formatNumber n)
  formatTime ls t _ = case NonEmpty.head ls of
    English -> Right (English.formatTime t)
    French -> Right (French.formatTime t)

data Wording = Wording
  { language :: SupportedLanguage
  , bundle :: Bundle SupportedLanguage
  , revision :: Word
  , strict :: Bool
  }
  deriving stock (Show)

instance Eq Wording where
  left == right = (left.language, left.revision) == (right.language, right.revision)

newtype WordingFault = WordingFault Text
  deriving stock (Show)

instance Exception WordingFault

parseFtl :: Text -> Either (NonEmpty Text) Resource
parseFtl source =
  parseResourceLenient source
    & rejectJunk
    & first (fmap (T.pack . displayException))

mkWording :: SupportedLanguage -> Word -> Resource -> Wording
mkWording language revision resource =
  let localeBundle = (Bundle.bundle (NonEmpty.singleton language) [resource]) {useIsolating = False}
  in Wording
       { language
       , bundle = localeBundle
       , revision
       , strict = False
       }

getTranslation
  :: Wording
  -> Reference
  -> List (Text, Value)
  -> Text
getTranslation wording reference arguments =
  case translate reference arguments wording.bundle of
    Right translation -> translation
    Left message
      | wording.strict -> throw (WordingFault (referenceName reference <> ": " <> T.pack message))
      | otherwise -> "{" <> referenceName reference <> "}"

referenceName :: Reference -> Text
referenceName reference =
  case reference.name of
    Left message -> T.pack message
    Right (Identifier name) -> name

str :: Text -> Value
str = Value.String

localeFile :: SupportedLanguage -> OsPath
localeFile = \case
  English -> English.localeFile
  French -> French.localeFile

languageCode :: SupportedLanguage -> Text
languageCode = \case
  English -> English.languageCode
  French -> French.languageCode

supportedLanguages :: Map Text SupportedLanguage
supportedLanguages =
  Map.fromList $
    List.zip (List.map toCode [minBound @SupportedLanguage .. maxBound]) [minBound .. maxBound]
