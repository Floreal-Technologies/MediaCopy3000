module MediaCopy.Plugin.Manifest
  ( Manifest (..)
  , validate
  , platformKey
  ) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Char (isAsciiLower, isDigit)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (List, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Info (arch, os)

import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.Field qualified as Plugin (Field)
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)

data Manifest = Manifest
  { id :: Plugin.Id
  , name :: Text
  , description :: Text
  , version :: Text
  , api :: Int
  , namespace :: Maybe Text
  , executable :: Map Text FilePath
  , capabilities :: Vector Plugin.Capability
  , settings :: Vector Plugin.Field
  , jobFields :: Vector Plugin.Field
  }
  deriving stock (Eq, Show)

instance FromJSON Manifest where
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
    pure Manifest {id = pluginId, name, description, version, api, namespace, executable, capabilities, settings, jobFields}

validate :: Manifest -> Either Text Manifest
validate m = do
  let Id.Id raw = m.id
      declares capability = V.elem capability m.capabilities
  unless (validId raw) (Left ("has the id " <> T.show raw <> ", which is not a reverse domain name"))
  when (T.null (T.strip m.description)) (Left "has a blank description")
  when (m.api /= Api.major) $
    Left ("needs plug-in API " <> T.show m.api <> ", and this MediaCopy 3000 speaks API " <> T.show Api.major)
  unless (V.any (`elem` Capability.hooks) m.capabilities) (Left "declares no capability that MediaCopy 3000 calls")
  for_ (Map.toList m.executable) $ \(platform, path) ->
    unless (insideFolder (T.pack path)) $
      Left ("names the executable " <> T.show path <> " for " <> T.show platform <> ", which is not inside the plug-in folder")
  when (declares Capability.Block && not (declares Capability.PlanInspect)) (Left "declares block, but not plan.inspect")
  when (declares Capability.ManifestWrite && isNothing m.namespace) (Left "declares manifest.write, but no namespace")
  for_ m.namespace $ \ns ->
    when (ns `elem` reservedNamespaces) $
      Left ("declares the namespace " <> T.show ns <> ", which is reserved")
  for_ [("settings", m.settings), ("jobFields", m.jobFields)] $ \(group, fields) -> do
    let keys = V.toList (V.map (.key) fields)
    when (length (nub keys) /= length keys) (Left ("repeats a key in " <> group))
  let authorsIn fields = V.filter (\field -> field.kind == Field.AuthorsField) fields
  for_ (authorsIn m.jobFields) $ \field ->
    Left ("declares the job field " <> T.show field.key <> " of kind authors, which only a setting can be")
  when (V.length (authorsIn m.settings) > 1) (Left "declares more than one setting of kind authors")
  for_ (authorsIn m.settings) $ \field -> do
    unless (declares Capability.ManifestWrite) $
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

instance Plugin.JsonSchema Manifest where
  defName = Just "manifest"
  schema =
    object
      [ "type" .= String "object"
      , "required" .= (["id", "name", "description", "version", "api", "executable", "capabilities"] :: [Text])
      , "properties"
          .= object
            [ "id" .= JsonSchema.embed @Plugin.Id
            , "name" .= JsonSchema.string
            , "description"
                .= (object ["type" .= String "string", "pattern" .= String "\\S"] & JsonSchema.describe "One or two short sentences that tell what the plug-in does. The preferences page shows it under the name.")
            , "version" .= JsonSchema.string
            , "api" .= object ["const" .= Api.major]
            , "namespace"
                .= ( object ["type" .= String "string", "not" .= object ["enum" .= reservedNamespaces]]
                       & JsonSchema.describe "The only XML namespace that a plug-in with manifest.write can write. It cannot be empty, a namespace of ASC MHL, or a namespace that XML reserves."
                   )
            , "executable"
                .= ( object ["type" .= String "object", "additionalProperties" .= object ["type" .= String "string", "pattern" .= String "^(?![/\\\\])(?!.*:)(?!(?:.*[/\\\\])?\\.\\.(?:[/\\\\]|$)).+$"]]
                       & JsonSchema.describe "Keys: linux-x86_64, linux-aarch64, macos-aarch64, macos-x86_64, windows-x86_64. Values: paths relative to the plug-in folder. A path does not start with '/' or '\\', holds no ':', and has no '..' part. One bad path, for any platform, rejects the plug-in on every platform."
                   )
            , "capabilities"
                .= ( object ["type" .= String "array", "items" .= JsonSchema.embed @Plugin.Capability, "contains" .= object ["enum" .= Capability.hooks]]
                       & JsonSchema.describe "At least one of plan.inspect, files.inspect and manifest.write."
                   )
            , "settings"
                .= ( object ["type" .= String "array", "items" .= JsonSchema.embed @Plugin.Field, "contains" .= authors, "minContains" .= (0 :: Int), "maxContains" .= (1 :: Int)]
                       & JsonSchema.describe "Keys are unique. At most one setting has the kind authors."
                   )
            , "jobFields"
                .= ( object ["type" .= String "array", "items" .= (JsonSchema.embed @Plugin.Field & JsonSchema.property "kind" (object ["not" .= object ["const" .= String "authors"]]))]
                       & JsonSchema.describe "Keys are unique. No job field has the kind authors."
                   )
            ]
      , "allOf"
          .= [ object ["if" .= declares Capability.Block, "then" .= declares Capability.PlanInspect]
             , object ["if" .= declares Capability.ManifestWrite, "then" .= object ["required" .= ["namespace" :: Text]]]
             , object ["if" .= object ["properties" .= object ["settings" .= object ["contains" .= authors]], "required" .= ["settings" :: Text]], "then" .= declares Capability.ManifestWrite]
             ]
      ]
    where
      authors = object ["properties" .= object ["kind" .= object ["const" .= String "authors"]], "required" .= ["kind" :: Text]]
      declares capability = object ["properties" .= object ["capabilities" .= object ["contains" .= object ["const" .= capability]]]]
