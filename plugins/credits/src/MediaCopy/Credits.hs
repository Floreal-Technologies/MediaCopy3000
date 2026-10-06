module MediaCopy.Credits
  ( Crew (..)
  , namespace
  , crewOf
  , contribution
  , creditsXml
  ) where

import Data.Aeson (Value (..))
import Data.Char (isControl)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import MediaCopy.Plugin.Protocol (Author (..), ContributeResult (..))

-- $setup
-- >>> import Data.Aeson (Value (..))
-- >>> import Data.Map.Strict qualified as Map
-- >>> let crew = crewOf (Map.fromList [("production", String "Night Shift"), ("dit", String "Jane Doe"), ("ditEmail", String "jane@example.com")]) (Map.fromList [("operator", String "Sam Roe"), ("secondOperator", String " ")])

data Crew = Crew
  { production :: Maybe Text
  , dit :: Maybe Text
  , ditEmail :: Maybe Text
  , ditPhone :: Maybe Text
  , operators :: [Text]
  }
  deriving stock (Eq, Show)

namespace :: Text
namespace = "https://floreal.tech/ns/credits/1"

-- |
-- >>> crew
-- Crew {production = Just "Night Shift", dit = Just "Jane Doe", ditEmail = Just "jane@example.com", ditPhone = Nothing, operators = ["Sam Roe"]}
-- >>> (crewOf (Map.singleton "dit" (String "Jane\nDoe\SOH")) Map.empty).dit
-- Just "Jane Doe"
crewOf :: Map Text Value -> Map Text Value -> Crew
crewOf settings jobFields =
  Crew
    { production = text settings "production"
    , dit = text settings "dit"
    , ditEmail = text settings "ditEmail"
    , ditPhone = text settings "ditPhone"
    , operators = catMaybes [text jobFields "operator", text jobFields "secondOperator"]
    }
  where
    text fields key = case Map.lookup key fields of
      Just (String raw) | cleaned <- clean raw, not (T.null cleaned) -> Just cleaned
      _ -> Nothing

clean :: Text -> Text
clean = T.unwords . T.words . T.map (\c -> if isControl c then ' ' else c) . T.filter xmlChar
  where
    xmlChar c = c `notElem` ['\xFFFE', '\xFFFF']

-- |
-- >>> map (\author -> (author.name, author.role)) (V.toList (contribution crew).authors)
-- [("Jane Doe",Just "DIT"),("Sam Roe",Just "camera operator")]
contribution :: Crew -> ContributeResult
contribution crew =
  ContributeResult
    { authors = V.fromList (ditAuthor <> map operator crew.operators)
    , fileMetadata = V.empty
    , manifestMetadata = creditsXml crew
    }
  where
    ditAuthor = [Author {name, email = crew.ditEmail, phone = crew.ditPhone, role = Just "DIT"} | Just name <- [crew.dit]]
    operator name = Author {name, email = Nothing, phone = Nothing, role = Just "camera operator"}

-- |
-- >>> creditsXml crew
-- Just "<credits xmlns=\"https://floreal.tech/ns/credits/1\"><production>Night Shift</production><dit>Jane Doe</dit><operator>Sam Roe</operator></credits>"
-- >>> creditsXml (crewOf (Map.singleton "production" (String "Tom & Jerry <2>")) Map.empty)
-- Just "<credits xmlns=\"https://floreal.tech/ns/credits/1\"><production>Tom &amp; Jerry &lt;2&gt;</production></credits>"
-- >>> creditsXml (crewOf Map.empty Map.empty)
-- Nothing
creditsXml :: Crew -> Maybe Text
creditsXml crew = case children of
  [] -> Nothing
  _ -> Just ("<credits xmlns=\"" <> namespace <> "\">" <> T.concat children <> "</credits>")
  where
    children =
      maybe [] (pure . element "production") crew.production
        <> maybe [] (pure . element "dit") crew.dit
        <> map (element "operator") crew.operators
    element name value = "<" <> name <> ">" <> escape value <> "</" <> name <> ">"

escape :: Text -> Text
escape = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  c -> T.singleton c
