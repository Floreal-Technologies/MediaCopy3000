module MediaCopy.Credits
  ( slotsOf
  , contribution
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.Char (isControl)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (AuthorSlot (..))
import MediaCopy.Plugin.Protocol (Author (..), ContributeResult (..))

-- $setup
-- >>> import Data.Aeson (Value (..), object, (.=))
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Text (Text)
-- >>> import Data.Vector qualified as V
-- >>> let slot role name email = object ["role" .= (role :: Text), "name" .= (name :: Text), "email" .= (email :: Text), "phone" .= ("" :: Text)]
-- >>> let slots = slotsOf (Map.singleton "authors" (Array (V.fromList [slot "DIT" "Jane Doe" "jane@example.com", slot "Camera operator" " " "", slot "Camera operator" "Sam\nRoe\SOH" ""])))

-- |
-- >>> slots
-- [Author {name = "Jane Doe", email = Just "jane@example.com", phone = Nothing, role = Just "DIT"},Author {name = "Sam Roe", email = Nothing, phone = Nothing, role = Just "Camera operator"}]
-- >>> slotsOf Map.empty
-- []
-- >>> slotsOf (Map.singleton "authors" (String "Jane"))
-- []
slotsOf :: Map Text Value -> Vector Author
slotsOf settings = case Map.lookup "authors" settings of
  Just (Array found) -> V.mapMaybe (\value -> parseMaybe parseJSON value >>= authorOf) found
  _ -> V.empty
  where
    authorOf :: AuthorSlot -> Maybe Author
    authorOf slot = do
      name <- present slot.name
      Just Author {name, email = present slot.email, phone = present slot.phone, role = present slot.role}
    present raw = let cleaned = clean raw in if T.null cleaned then Nothing else Just cleaned

clean :: Text -> Text
clean text = text & T.filter xmlChar & T.map (\c -> if isControl c then ' ' else c) & T.words & T.unwords
  where
    xmlChar c = c `notElem` ['\xFFFE', '\xFFFF']

-- |
-- >>> map (\author -> (author.name, author.role, author.email, author.phone)) (V.toList (contribution slots).authors)
-- [("Jane Doe",Just "DIT",Just "jane@example.com",Nothing),("Sam Roe",Just "Camera operator",Nothing,Nothing)]
-- >>> (contribution slots).manifestMetadata
-- Nothing
contribution :: Vector Author -> ContributeResult
contribution authors = ContributeResult {authors, fileMetadata = V.empty, manifestMetadata = Nothing}
