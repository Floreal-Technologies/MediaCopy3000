module MediaCopy.Plugin.Manifest
  ( -- * Manifest
    PluginManifest (..)
  , PluginId (..)
  , validateManifest

    -- * Roles and capabilities
  , Role (..)
  , roleName
  , Capability (..)
  , capabilityName

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
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Info (arch, os)

newtype PluginId = PluginId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Role = Inspector | Contributor | Producer | Deliverer
  deriving stock (Bounded, Enum, Eq, Ord, Show)

roleName :: Role -> Text
roleName = \case
  Inspector -> "inspector"
  Contributor -> "contributor"
  Producer -> "producer"
  Deliverer -> "deliverer"

instance ToJSON Role where
  toJSON role = String (roleName role)

instance FromJSON Role where
  parseJSON = withText "role" (named "role" roleName)

data Capability = FilesRead | Block | Network | ManifestWrite
  deriving stock (Bounded, Enum, Eq, Ord, Show)

capabilityName :: Capability -> Text
capabilityName = \case
  FilesRead -> "files.read"
  Block -> "block"
  Network -> "network"
  ManifestWrite -> "manifest.write"

instance ToJSON Capability where
  toJSON capability = String (capabilityName capability)

instance FromJSON Capability where
  parseJSON = withText "capability" (named "capability" capabilityName)

named :: (Bounded a, Enum a) => String -> (a -> Text) -> Text -> Parser a
named what nameOf found = case lookup found [(nameOf value, value) | value <- [minBound ..]] of
  Just value -> pure value
  Nothing -> fail ("unknown " <> what <> " " <> show found)

data FieldKind = TextField | SecretField | BoolField | ChoiceField (Vector Text) | PathField
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

data PluginManifest = PluginManifest
  { id :: PluginId
  , name :: Text
  , version :: Text
  , api :: Int
  , namespace :: Maybe Text
  , executable :: Map Text FilePath
  , roles :: Vector Role
  , capabilities :: Vector Capability
  , settings :: Vector Field
  , jobFields :: Vector Field
  }
  deriving stock (Eq, Show)

instance FromJSON PluginManifest where
  parseJSON = withObject "plugin.json" $ \o -> do
    pluginId <- o .: "id"
    name <- o .: "name"
    version <- o .: "version"
    api <- o .: "api"
    namespace <- o .:? "namespace"
    executable <- o .: "executable"
    roles <- o .: "roles"
    capabilities <- fromMaybe V.empty <$> o .:? "capabilities"
    settings <- fromMaybe V.empty <$> o .:? "settings"
    jobFields <- fromMaybe V.empty <$> o .:? "jobFields"
    pure PluginManifest {id = pluginId, name, version, api, namespace, executable, roles, capabilities, settings, jobFields}

instance ToJSON PluginManifest where
  toJSON m =
    object $
      [ "id" .= m.id
      , "name" .= m.name
      , "version" .= m.version
      , "api" .= m.api
      , "executable" .= m.executable
      , "roles" .= m.roles
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
  unless (validId raw) (Left ("has the id " <> T.show raw <> ", which is not a reverse domain name"))
  when (m.api /= apiMajor) $
    Left ("needs plug-in API " <> T.show m.api <> ", and this MediaCopy 3000 speaks API " <> T.show apiMajor)
  when (V.null m.roles) (Left "declares no role")
  for_ (Map.toList m.executable) $ \(platform, path) ->
    unless (insideFolder (T.pack path)) $
      Left ("names the executable " <> T.show path <> " for " <> T.show platform <> ", which is not inside the plug-in folder")
  for_ m.capabilities $ \capability -> case allowedFor capability of
    Just role
      | role `notElem` m.roles ->
          Left ("declares " <> capabilityName capability <> ", which only " <> article role <> " can have")
    _ -> Right ()
  when (Contributor `elem` m.roles && ManifestWrite `notElem` m.capabilities) $
    Left "is a contributor, but does not declare manifest.write"
  when (Contributor `elem` m.roles && isNothing m.namespace) $
    Left "is a contributor, but declares no namespace"
  for_ m.namespace $ \ns ->
    when (ns `elem` reservedNamespaces) $
      Left ("declares the namespace " <> T.show ns <> ", which is reserved")
  for_ [("settings", m.settings), ("jobFields", m.jobFields)] $ \(group, fields) -> do
    let keys = V.toList (V.map (.key) fields)
    when (length (nub keys) /= length keys) (Left ("repeats a key in " <> group))
  Right m

reservedNamespaces :: List Text
reservedNamespaces =
  [ ""
  , "urn:ASC:MHL:v2.0"
  , "urn:ASC:MHL:DIRECTORY:v2.0"
  , "http://www.w3.org/XML/1998/namespace"
  , "http://www.w3.org/2000/xmlns/"
  ]

allowedFor :: Capability -> Maybe Role
allowedFor = \case
  FilesRead -> Nothing
  Block -> Just Inspector
  Network -> Just Deliverer
  ManifestWrite -> Just Contributor

article :: Role -> Text
article role = case role of
  Inspector -> "an inspector"
  _ -> "a " <> roleName role

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
