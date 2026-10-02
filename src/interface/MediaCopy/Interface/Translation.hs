module MediaCopy.Interface.Translation
  ( getTranslation
  , SupportedLanguage (..)
  , parseFtl
  , Wording (..)
  , mkWording
  , parseWording
  , WordingFault (..)
  , getTranslation'
  , localeFile
  , int
  , str
  ) where

import Control.Exception (Exception, throw)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (List)
import Data.List qualified as List
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Language.Fluent
import Language.Fluent.AST (Entry (..), Identifier (..), Resource (..))
import Language.Fluent.Bundle qualified as Bundle
import System.OsPath (OsPath)

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
parseFtl source = do
  resource <- first (\message -> pure (T.pack message)) (parseResource source)
  let potentialJunk =
        resource.entries
          & List.map
            ( \case
                JunkEntry junk -> Just junk
                _ -> Nothing
            )
          & Maybe.catMaybes
          & NonEmpty.nonEmpty
  case potentialJunk of
    Just junkEntries -> Left junkEntries
    Nothing -> Right resource

mkWording :: SupportedLanguage -> Word -> Resource -> Wording
mkWording language revision resource =
  let localeBundle = (Bundle.bundle (NonEmpty.singleton language) [resource]) {useIsolating = False}
  in Wording
       { language
       , bundle = localeBundle
       , revision
       , strict = False
       }

parseWording :: SupportedLanguage -> Word -> Text -> Either (NonEmpty Text) Wording
parseWording language revision source =
  mkWording language revision <$> parseFtl source

getTranslation
  :: Wording
  -> Reference
  -> List (Text, SomeValue)
  -> Either Text Text
getTranslation wording reference arguments =
  case translate reference arguments wording.bundle of
    Left message -> Left $ referenceName reference <> ": " <> T.pack message
    Right translation -> Right translation

getTranslation'
  :: Wording
  -> Reference
  -> List (Text, SomeValue)
  -> Text
getTranslation' wording reference arguments =
  case getTranslation wording reference arguments of
    Right translation -> translation
    Left fault
      | wording.strict -> throw (WordingFault fault)
      | otherwise -> "{" <> referenceName reference <> "}"

referenceName :: Reference -> Text
referenceName reference =
  case reference.name of
    Left message -> T.pack message
    Right (Identifier name) -> name

str :: Text -> SomeValue
str = StringValue

int :: Int -> SomeValue
int = value

-- * Helpers

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
