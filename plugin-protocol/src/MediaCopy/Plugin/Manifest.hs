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
import Data.List (List, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Info (arch, os)

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

instance FromJSON Field where
  parseJSON = withObject "field" $ \o -> do
    key <- o .: "key"
    label <- o .: "label"
    kindName <- o .: "kind"
    kind <- case kindName :: Text of
      "text" -> pure TextField
      "secret" -> pure SecretField
      "bool" -> pure BoolField
      "choice" -> ChoiceField <$> o .: "options"
      "path" -> pure PathField
      "authors" -> pure AuthorsField
      other -> fail ("unknown field kind " <> show other)
    required <- fromMaybe False <$> o .:? "required"
    defaultValue <- o .:? "default"
    pure Field {key, label, kind, required, defaultValue}

instance ToJSON Field where
  toJSON field =
    object $
      [ "key" .= field.key
      , "label" .= field.label
      , "kind" .= kindName
      , "required" .= field.required
      ]
        <> options
        <> maybe [] (\value -> ["default" .= value]) field.defaultValue
    where
      (kindName, options) = case field.kind of
        TextField -> ("text" :: Text, [])
        SecretField -> ("secret", [])
        BoolField -> ("bool", [])
        ChoiceField choices -> ("choice", ["options" .= choices])
        PathField -> ("path", [])
        AuthorsField -> ("authors", [])

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

instance ToJSON PluginManifest where
  toJSON m =
    object $
      [ "id" .= m.id
      , "name" .= m.name
      , "description" .= m.description
      , "version" .= m.version
      , "api" .= m.api
      , "executable" .= m.executable
      , "capabilities" .= m.capabilities
      , "settings" .= m.settings
      , "jobFields" .= m.jobFields
      ]
        <> maybe [] (\ns -> ["namespace" .= ns]) m.namespace

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
