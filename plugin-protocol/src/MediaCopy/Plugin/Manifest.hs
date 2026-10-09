module MediaCopy.Plugin.Manifest
  ( -- * Manifest
    PluginManifest (..)
  , PluginId (..)
  , validateManifest

    -- * Capabilities
  , Capability (..)
  , capabilityName
  , Hook (..)
  , hookOf

    -- * Settings and job fields
  , Field (..)
  , FieldKind (..)
  , AuthorSlot (..)

    -- * API version
  , apiMajor
  , apiMinor

    -- * Platform
  , platformKey
  ) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isAsciiLower, isDigit)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (List, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Info (arch, os)

import MediaCopy.Plugin.JsonSchema

newtype PluginId = PluginId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Capability = FilesRead | PlanInspect | FilesInspect | Block | ManifestWrite
  deriving stock (Bounded, Enum, Eq, Ord, Show)

capabilityName :: Capability -> Text
capabilityName = \case
  FilesRead -> "files.read"
  PlanInspect -> "plan.inspect"
  FilesInspect -> "files.inspect"
  Block -> "block"
  ManifestWrite -> "manifest.write"

instance ToJSON Capability where
  toJSON capability = String (capabilityName capability)

data Hook = InspectPlan | InspectFile | Contribute
  deriving stock (Eq, Show)

hookOf :: Capability -> Maybe Hook
hookOf = \case
  PlanInspect -> Just InspectPlan
  FilesInspect -> Just InspectFile
  ManifestWrite -> Just Contribute
  FilesRead -> Nothing
  Block -> Nothing

instance FromJSON Capability where
  parseJSON = withText "capability" (named "capability" capabilityName)

named :: (Bounded a, Enum a) => String -> (a -> Text) -> Text -> Parser a
named what nameOf found = case lookup found [(nameOf value, value) | value <- [minBound ..]] of
  Just value -> pure value
  Nothing -> fail ("unknown " <> what <> " " <> show found)

data FieldKind = TextField | SecretField | BoolField | ChoiceField (Vector Text) | PathField | AuthorsField
  deriving stock (Eq, Show)

data Field = Field
  { key :: Text
  , label :: Text
  , kind :: FieldKind
  , required :: Bool
  , defaultValue :: Maybe Value
  }
  deriving stock (Eq, Show)

kindName :: FieldKind -> Text
kindName = \case
  TextField -> "text"
  SecretField -> "secret"
  BoolField -> "bool"
  ChoiceField _ -> "choice"
  PathField -> "path"
  AuthorsField -> "authors"

fieldKinds :: [(Text, Object -> Parser FieldKind)]
fieldKinds =
  [ (kindName TextField, \_ -> pure TextField)
  , (kindName SecretField, \_ -> pure SecretField)
  , (kindName BoolField, \_ -> pure BoolField)
  , (kindName (ChoiceField V.empty), \o -> ChoiceField <$> o .: "options")
  , (kindName PathField, \_ -> pure PathField)
  , (kindName AuthorsField, \_ -> pure AuthorsField)
  ]

instance FromJSON Field where
  parseJSON = withObject "field" $ \o -> do
    key <- o .: "key"
    label <- o .: "label"
    name <- o .: "kind"
    kind <- case lookup name fieldKinds of
      Just parse -> parse o
      Nothing -> fail ("unknown field kind " <> show name)
    required <- fromMaybe False <$> o .:? "required"
    defaultValue <- o .:? "default"
    pure Field {key, label, kind, required, defaultValue}

data PluginManifest = PluginManifest
  { id :: PluginId
  , name :: Text
  , description :: Text
  , version :: Text
  , api :: Int
  , namespace :: Maybe Text
  , executable :: Map Text FilePath
  , capabilities :: Vector Capability
  , settings :: Vector Field
  , jobFields :: Vector Field
  }
  deriving stock (Eq, Show)

instance FromJSON PluginManifest where
  parseJSON = withObject "plugin.json" $ \o -> do
    pluginId <- o .: "id"
    name <- o .: "name"
    description <- o .: "description"
    version <- o .: "version"
    api <- o .: "api"
    namespace <- o .:? "namespace"
    executable <- o .: "executable"
    capabilities <- fromMaybe V.empty <$> o .:? "capabilities"
    settings <- fromMaybe V.empty <$> o .:? "settings"
    jobFields <- fromMaybe V.empty <$> o .:? "jobFields"
    pure PluginManifest {id = pluginId, name, description, version, api, namespace, executable, capabilities, settings, jobFields}

apiMajor :: Int
apiMajor = 1

apiMinor :: Int
apiMinor = 0

validateManifest :: PluginManifest -> Either Text PluginManifest
validateManifest m = do
  let PluginId raw = m.id
      declares capability = V.elem capability m.capabilities
  unless (validId raw) (Left ("has the id " <> T.show raw <> ", which is not a reverse domain name"))
  when (T.null (T.strip m.description)) (Left "has a blank description")
  when (m.api /= apiMajor) $
    Left ("needs plug-in API " <> T.show m.api <> ", and this MediaCopy 3000 speaks API " <> T.show apiMajor)
  unless (V.any (isJust . hookOf) m.capabilities) (Left "declares no capability that MediaCopy 3000 calls")
  for_ (Map.toList m.executable) $ \(platform, path) ->
    unless (insideFolder (T.pack path)) $
      Left ("names the executable " <> T.show path <> " for " <> T.show platform <> ", which is not inside the plug-in folder")
  when (declares Block && not (declares PlanInspect)) (Left "declares block, but not plan.inspect")
  when (declares ManifestWrite && isNothing m.namespace) (Left "declares manifest.write, but no namespace")
  for_ m.namespace $ \ns ->
    when (ns `elem` reservedNamespaces) $
      Left ("declares the namespace " <> T.show ns <> ", which is reserved")
  for_ [("settings", m.settings), ("jobFields", m.jobFields)] $ \(group, fields) -> do
    let keys = V.toList (V.map (.key) fields)
    when (length (nub keys) /= length keys) (Left ("repeats a key in " <> group))
  let authorsIn fields = V.filter (\field -> field.kind == AuthorsField) fields
  for_ (authorsIn m.jobFields) $ \field ->
    Left ("declares the job field " <> T.show field.key <> " of kind authors, which only a setting can be")
  when (V.length (authorsIn m.settings) > 1) (Left "declares more than one setting of kind authors")
  for_ (authorsIn m.settings) $ \field -> do
    unless (declares ManifestWrite) $
      Left ("declares the setting " <> T.show field.key <> " of kind authors, which needs manifest.write")
    unless (isNothing field.defaultValue) $
      Left ("declares a default for the setting " <> T.show field.key <> " of kind authors")
  Right m

reservedNamespaces :: List Text
reservedNamespaces =
  [ ""
  , "urn:ASC:MHL:v2.0"
  , "urn:ASC:MHL:DIRECTORY:v2.0"
  , "http://www.w3.org/XML/1998/namespace"
  , "http://www.w3.org/2000/xmlns/"
  ]

validId :: Text -> Bool
validId raw =
  T.all (\c -> isAsciiLower c || isDigit c || c `elem` ("._-" :: List Char)) raw
    && T.isInfixOf "." raw
    && not ("." `T.isPrefixOf` raw)
    && not ("." `T.isSuffixOf` raw)
    && not (".." `T.isInfixOf` raw)

insideFolder :: Text -> Bool
insideFolder path =
  not (T.null path)
    && T.all (/= ':') path
    && T.take 1 path `notElem` ["/", "\\"]
    && ".." `notElem` T.split (\c -> c == '/' || c == '\\') path

platformKey :: Text
platformKey = T.pack (system <> "-" <> arch)
  where
    system = case os of
      "darwin" -> "macos"
      "mingw32" -> "windows"
      other -> other

instance JsonSchema PluginId where
  defName = Just "pluginId"
  schema =
    object ["type" .= String "string", "pattern" .= String "^(?!.*\\.\\.)[a-z0-9_-][a-z0-9._-]*\\.[a-z0-9._-]*[a-z0-9_-]$"]
      & describe "A reverse domain name in lowercase, for example tech.floreal.c2pa-reader. It holds at least one '.', it does not start or end with '.', and it has no '..'. It is also the name of the folder of the plug-in."

instance JsonSchema Capability where
  defName = Just "capability"
  schema = enumOf @Capability

-- |
-- >>> decode "[{\"role\": \"DIT\", \"name\": \"Jane Doe\"}, {\"role\": \"Camera operator\"}]" :: Maybe (V.Vector AuthorSlot)
-- Just [AuthorSlot {role = "DIT", name = "Jane Doe", email = "", phone = ""},AuthorSlot {role = "Camera operator", name = "", email = "", phone = ""}]
-- >>> decode "[{\"name\": \"Jane\"}]" :: Maybe (V.Vector AuthorSlot)
-- Nothing
-- >>> decode "[{\"role\": \"DIT\", \"email\": null}]" :: Maybe (V.Vector AuthorSlot)
-- Nothing
-- >>> toJSON (AuthorSlot "DIT" "Jane Doe" "" "")
-- Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")])
data AuthorSlot = AuthorSlot
  { role :: Text
  , name :: Text
  , email :: Text
  , phone :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON AuthorSlot where
  parseJSON = withObject "author slot" $ \o -> do
    role <- o .: "role"
    name <- o .:! "name" .!= ""
    email <- o .:! "email" .!= ""
    phone <- o .:! "phone" .!= ""
    pure AuthorSlot {role, name, email, phone}

instance ToJSON AuthorSlot where
  toJSON slot = object ["role" .= slot.role, "name" .= slot.name, "email" .= slot.email, "phone" .= slot.phone]

instance JsonSchema AuthorSlot where
  defName = Just "authorSlot"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= ["role" :: Text]
      , "properties"
          .= object
            [ "role" .= object ["type" .= String "string", "minLength" .= (1 :: Int)]
            , "name" .= string
            , "email" .= string
            , "phone" .= string
            ]
      ]
      & describe "One author slot of a setting of kind authors. In plugins.json, name, email and phone are the defaults. In initialize.settings, they are the merged values for the job, and slots with no name are removed."

instance JsonSchema Field where
  defName = Just "field"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= (["key", "label", "kind"] :: [Text])
      , "properties"
          .= object
            [ "key" .= string
            , "label" .= string
            , "kind" .= object ["enum" .= map fst fieldKinds]
            , "options" .= embed @(Vector Text)
            , "required" .= object ["type" .= String "boolean", "default" .= False]
            , "default" .= object []
            ]
      , "allOf"
          .= [ object ["if" .= kindIs "choice", "then" .= object ["required" .= ["options" :: Text]]]
             , object ["if" .= kindIs "authors", "then" .= object ["not" .= object ["required" .= ["default" :: Text]]]]
             ]
      ]
      & describe "A setting or a job field. The kind authors is a closed shape: it is allowed only in settings, only once, only with manifest.write, and without default. Its stored value and its value in initialize are arrays of authorSlot."
    where
      kindIs :: Text -> Value
      kindIs name = object ["properties" .= object ["kind" .= object ["const" .= name]]]

instance JsonSchema PluginManifest where
  defName = Just "manifest"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= (["id", "name", "description", "version", "api", "executable", "capabilities"] :: [Text])
      , "properties"
          .= object
            [ "id" .= embed @PluginId
            , "name" .= string
            , "description"
                .= (object ["type" .= String "string", "pattern" .= String "\\S"] & describe "One or two short sentences that tell what the plug-in does. The preferences page shows it under the name.")
            , "version" .= string
            , "api" .= object ["const" .= apiMajor]
            , "namespace"
                .= ( object ["type" .= String "string", "not" .= object ["enum" .= reservedNamespaces]]
                       & describe "The only XML namespace that a plug-in with manifest.write can write. It cannot be empty, a namespace of ASC MHL, or a namespace that XML reserves."
                   )
            , "executable"
                .= ( object ["type" .= String "object", "additionalProperties" .= object ["type" .= String "string", "pattern" .= String "^(?![/\\\\])(?!.*:)(?!(?:.*[/\\\\])?\\.\\.(?:[/\\\\]|$)).+$"]]
                       & describe "Keys: linux-x86_64, linux-aarch64, macos-aarch64, macos-x86_64, windows-x86_64. Values: paths relative to the plug-in folder. A path does not start with '/' or '\\', holds no ':', and has no '..' part. One bad path, for any platform, rejects the plug-in on every platform."
                   )
            , "capabilities"
                .= ( object ["type" .= String "array", "items" .= embed @Capability, "contains" .= object ["enum" .= hooks]]
                       & describe "At least one of plan.inspect, files.inspect and manifest.write."
                   )
            , "settings"
                .= ( object ["type" .= String "array", "items" .= embed @Field, "contains" .= authors, "minContains" .= (0 :: Int), "maxContains" .= (1 :: Int)]
                       & describe "Keys are unique. At most one setting has the kind authors."
                   )
            , "jobFields"
                .= ( object ["type" .= String "array", "items" .= (embed @Field & property "kind" (object ["not" .= object ["const" .= String "authors"]]))]
                       & describe "Keys are unique. No job field has the kind authors."
                   )
            ]
      , "allOf"
          .= [ object ["if" .= declares Block, "then" .= declares PlanInspect]
             , object ["if" .= declares ManifestWrite, "then" .= object ["required" .= ["namespace" :: Text]]]
             , object ["if" .= object ["properties" .= object ["settings" .= object ["contains" .= authors]], "required" .= ["settings" :: Text]], "then" .= declares ManifestWrite]
             ]
      ]
    where
      hooks = [capability | capability <- [minBound .. maxBound], isJust (hookOf capability)]
      authors = object ["properties" .= object ["kind" .= object ["const" .= String "authors"]], "required" .= ["kind" :: Text]]
      declares capability = object ["properties" .= object ["capabilities" .= object ["contains" .= object ["const" .= capability]]]]
