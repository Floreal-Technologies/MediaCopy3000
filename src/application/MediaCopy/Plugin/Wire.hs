module MediaCopy.Plugin.Wire
  ( pluginRef
  , jobInfo
  , fileInfos
  , fieldValues
  , slotsOf
  , slotsJson
  , mergeAuthorSettings
  , pluginSaid
  , contributionOf
  , mergeContributions
  , inspectFileParams
  , annotationsOf
  ) where

import Ascmhl.Hash (Hash (..))
import Ascmhl.Path (RelPath, mkRelPath, pathText)
import Ascmhl.Read (isXmlChar, parseFragment)
import Ascmhl.Types (Fragment (..))
import Ascmhl.Types qualified as Mhl
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isControl, isSpace)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest (Field (..), FieldKind (..), PluginId (..), PluginManifest (..))
import MediaCopy.Plugin.Protocol qualified as P

import MediaCopy.Domain.Job
import MediaCopy.Domain.JobFormat (formatAlgo)
import MediaCopy.Domain.Plan
import MediaCopy.Domain.Plugin
import MediaCopy.Domain.PluginCatalog (AuthorSlot (..), slotKey)
import MediaCopy.Plugin.Discovery (Installed (..))

-- $setup
-- >>> import Data.Aeson (Value (..), object, (.=))
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Text (Text)
-- >>> import Data.Vector qualified as V
-- >>> import MediaCopy.Plugin.Manifest (Field (..), FieldKind (..))
-- >>> import MediaCopy.Domain.PluginCatalog (AuthorSlot (..))
-- >>> let field key kind = Field {key, label = key, kind, required = True, defaultValue = Nothing}
-- >>> let slots = V.fromList [AuthorSlot "DIT" "Jane Doe" "" "", AuthorSlot "Camera operator" "" "" ""]

pluginRef :: Installed -> PluginRef
pluginRef installed = let PluginId raw = installed.manifest.id in PluginRef {id = raw, name = oneLine installed.manifest.name}

jobInfo :: JobPlan -> P.JobInfo
jobInfo plan =
  P.JobInfo
    { kind = display (jobKind plan.spec.job)
    , source = pathText (jobRoot plan.spec.job)
    , destinations = case plan.spec.job of
        Offload _ -> V.map (\target -> pathText target.root) plan.targets
        _ -> V.empty
    , hashFormat = fmap (display . formatAlgo) plan.format
    }

fileInfos :: JobPlan -> Vector P.FileInfo
fileInfos plan = V.map (\step -> P.FileInfo {path = display step.path, size = step.size}) plan.steps

-- |
-- >>> fieldValues (V.fromList [field "camera" (ChoiceField (V.fromList ["A", "B"])), field "dit" TextField]) (Map.fromList [("camera", String "B"), ("dit", String "Jane")])
-- Right (fromList [("camera",String "B"),("dit",String "Jane")])
-- >>> fieldValues (V.singleton (field "camera" (ChoiceField (V.fromList ["A", "B"])))) (Map.singleton "camera" (String "C"))
-- Left "camera"
-- >>> fieldValues (V.singleton (field "offline" BoolField)) (Map.singleton "offline" (String "true"))
-- Right (fromList [("offline",Bool True)])
-- >>> fieldValues (V.singleton (field "token" SecretField)) (Map.singleton "token" (String "abc"))
-- Right (fromList [("token",String "abc")])
-- >>> fieldValues (V.singleton (field "token" SecretField)) Map.empty
-- Left "token"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.fromList [object ["role" .= ("DIT" :: Text), "name" .= ("Jane Doe" :: Text)], object ["role" .= ("Camera operator" :: Text)]])))
-- Right (fromList [("authors",Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")]),Object (fromList [("email",String ""),("name",String ""),("phone",String ""),("role",String "Camera operator")])])])
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (String "Jane"))
-- Left "authors"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.singleton (object ["name" .= ("Jane" :: Text)]))))
-- Left "authors"
-- >>> fieldValues (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors" (Array (V.singleton (object ["role" .= (" " :: Text)]))))
-- Left "authors"
fieldValues :: Vector Field -> Map Text Value -> Either Text (Map Text Value)
fieldValues fields given = foldl' step (Right Map.empty) fields
  where
    step acc field = do
      values <- acc
      case Map.lookup field.key given >>= accepted field.kind of
        Just value -> Right (Map.insert field.key value values)
        Nothing
          | isJust (Map.lookup field.key given) -> Left field.key
          | Just value <- field.defaultValue >>= accepted field.kind -> Right (Map.insert field.key value values)
          | field.required -> Left field.key
          | otherwise -> Right values
    accepted kind value = case (kind, value) of
      (TextField, String text) -> Just (String text)
      (PathField, String text) -> Just (String text)
      (SecretField, String text) -> Just (String text)
      (BoolField, Bool flag) -> Just (Bool flag)
      (BoolField, String "true") -> Just (Bool True)
      (BoolField, String "false") -> Just (Bool False)
      (ChoiceField choices, String text) | text `elem` choices -> Just (String text)
      (AuthorsField, json) | Just slots <- slotsOf json, all (isJust . nonBlank . (.role)) slots -> Just (slotsJson slots)
      _ -> Nothing

-- |
-- >>> slotsOf (Array (V.fromList [object ["role" .= ("DIT" :: Text), "name" .= ("Jane Doe" :: Text)], object ["role" .= ("Camera operator" :: Text)]]))
-- Just [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""},AuthorSlot {role = "Camera operator", name = "", email = "", phone = ""}]
-- >>> slotsOf (Array (V.singleton (object ["name" .= ("Jane" :: Text)])))
-- Nothing
-- >>> slotsOf (Array (V.singleton (object ["role" .= ("DIT" :: Text), "email" .= Null])))
-- Nothing
-- >>> slotsOf (String "Jane")
-- Nothing
slotsOf :: Value -> Maybe (Vector AuthorSlot)
slotsOf = \case
  Array slots -> traverse slotOf slots
  _ -> Nothing
  where
    slotOf = \case
      Object slot -> do
        role <- KeyMap.lookup "role" slot >>= text
        name <- part "name" slot
        email <- part "email" slot
        phone <- part "phone" slot
        Just AuthorSlot {role, name, email, phone}
      _ -> Nothing
    part key slot = maybe (Just "") text (KeyMap.lookup key slot)
    text = \case
      String value -> Just value
      _ -> Nothing

-- |
-- >>> slotsJson (V.singleton (AuthorSlot "DIT" "Jane Doe" "" ""))
-- Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")])]
slotsJson :: Vector AuthorSlot -> Value
slotsJson = Array . V.map (\slot -> object ["role" .= slot.role, "name" .= slot.name, "email" .= slot.email, "phone" .= slot.phone])

-- |
-- >>> mergeAuthors "authors" (Map.fromList [("authors.1.name", "Sam Roe"), ("authors.0.email", "jane@example.com"), ("authors.0.name", "  "), ("authors.7.name", "Nobody")]) slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "jane@example.com", phone = ""},AuthorSlot {role = "Camera operator", name = "Sam Roe", email = "", phone = ""}]
-- >>> mergeAuthors "authors" Map.empty slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""}]
-- >>> mergeAuthors "authors" (Map.singleton "authors.0.role" "Boss") slots
-- [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""}]
mergeAuthors :: Text -> Map Text Text -> Vector AuthorSlot -> Vector AuthorSlot
mergeAuthors key given slots = V.filter (isJust . nonBlank . (.name)) (V.imap merge slots)
  where
    merge index slot =
      AuthorSlot
        { role = slot.role
        , name = pick index "name" slot.name
        , email = pick index "email" slot.email
        , phone = pick index "phone" slot.phone
        }
    pick index part stored = fromMaybe stored (Map.lookup (slotKey key index part) given >>= nonBlank)

nonBlank :: Text -> Maybe Text
nonBlank text = if T.all (\c -> isSpace c || isControl c) text then Nothing else Just text

-- |
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField)) (Map.singleton "authors.1.name" "Sam Roe") (Map.singleton "authors" (slotsJson slots))
-- Right (fromList [("authors",Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")]),Object (fromList [("email",String ""),("name",String "Sam Roe"),("phone",String ""),("role",String "Camera operator")])])])
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField)) Map.empty (Map.singleton "authors" (slotsJson (V.drop 1 slots)))
-- Left "authors"
-- >>> mergeAuthorSettings (V.singleton (field "authors" AuthorsField) {required = False}) Map.empty (Map.singleton "authors" (slotsJson (V.drop 1 slots)))
-- Right (fromList [("authors",Array [])])
-- >>> mergeAuthorSettings (V.singleton (field "dit" TextField)) Map.empty (Map.singleton "dit" (String "Jane"))
-- Right (fromList [("dit",String "Jane")])
mergeAuthorSettings :: Vector Field -> Map Text Text -> Map Text Value -> Either Text (Map Text Value)
mergeAuthorSettings fields given settings = foldl' step (Right settings) fields
  where
    step acc field
      | field.kind /= AuthorsField = acc
      | otherwise = do
          values <- acc
          let stored = fromMaybe V.empty (Map.lookup field.key values >>= slotsOf)
              merged = mergeAuthors field.key given stored
          if V.null merged && field.required
            then Left field.key
            else Right (Map.insert field.key (slotsJson merged) values)

pluginSaid :: PluginRef -> Bool -> P.Finding -> PluginFinding
pluginSaid plugin mayBlock finding =
  PluginFinding
    { plugin
    , severity = case finding.severity of
        P.Blocker | mayBlock -> Blocker
        _ -> Warning
    , about = Said PluginSays {key = oneLine finding.key, title = oneLine finding.title, detail = oneLine finding.detail}
    }

-- |
-- >>> let probe = PluginRef {id = "tech.floreal.probe", name = "Probe"}
-- >>> let author name = P.Author {name, email = Nothing, phone = Nothing, role = Just "DIT"}
-- >>> either id (const "accepted") (contributionOf probe "urn:x" mempty P.ContributeResult {authors = V.singleton (author "Jane\SOHDoe"), fileMetadata = V.empty, manifestMetadata = Nothing})
-- "an author holds a character that XML does not allow: \"Jane\\SOHDoe\""
-- >>> either id (const "accepted") (contributionOf probe "urn:x" mempty P.ContributeResult {authors = V.singleton (author "Jane Doe"), fileMetadata = V.empty, manifestMetadata = Nothing})
-- "accepted"
contributionOf :: PluginRef -> Text -> Set RelPath -> P.ContributeResult -> Either Text Contributions
contributionOf plugin namespace jobFiles result = do
  traverse_ xmlText (V.toList result.authors)
  pairs <- traverse metadataOf (V.toList result.fileMetadata)
  manifestMetadata <- traverse (parseFragment namespace) result.manifestMetadata
  Right
    Contributions
      { authors = V.map authorOf result.authors
      , fileMetadata = Map.fromListWith (flip joinFragments) pairs
      , manifestMetadata
      , contributors = V.singleton plugin
      }
  where
    metadataOf entry = case mkRelPath entry.path of
      Just path | Set.member path jobFiles -> (\fragment -> (path, fragment)) <$> parseFragment namespace entry.xml
      _ -> Left ("metadata for a file that is not in the job: " <> oneLine entry.path)
    authorOf author = Mhl.Author {name = oneLine author.name, email = oneLine <$> author.email, phone = oneLine <$> author.phone, role = oneLine <$> author.role}
    xmlText author =
      for_ (author.name : catMaybes [author.email, author.phone, author.role]) $ \text ->
        unless (T.all isXmlChar text) (Left ("an author holds a character that XML does not allow: " <> T.show text))

mergeContributions :: Vector Contributions -> Contributions
mergeContributions = foldl' merge noContributions
  where
    merge acc next =
      Contributions
        { authors = acc.authors <> next.authors
        , fileMetadata = Map.unionWith joinFragments acc.fileMetadata next.fileMetadata
        , manifestMetadata = case (acc.manifestMetadata, next.manifestMetadata) of
            (Just earlier, Just later) -> Just (joinFragments earlier later)
            (earlier, later) -> earlier <|> later
        , contributors = acc.contributors <> next.contributors
        }

joinFragments :: Fragment -> Fragment -> Fragment
joinFragments (Fragment earlier) (Fragment later) = Fragment (earlier <> later)

inspectFileParams :: VerifiedFile -> P.InspectFileParams
inspectFileParams file =
  P.InspectFileParams
    { path = display file.path
    , location = pathText file.location
    , hashes = V.map hashValue file.hashes
    , size = file.size
    }

hashValue :: Hash -> P.HashValue
hashValue hash = P.HashValue {algo = display hash.algo, value = hash.value}

annotationsOf :: PluginRef -> P.InspectFileResult -> Vector Annotation
annotationsOf plugin result = V.map (\note -> Annotation {plugin, key = oneLine note.key, label = oneLine note.label, value = oneLine note.value}) result.annotations
