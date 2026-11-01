module MediaCopy.Plugin.Manifest
  ( Manifest (..)
  , validate
  , platformKey
  , reservedNamespaces
  ) where

import Autodocodec
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Foldable (for_)
import Data.List (List, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
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
  deriving (FromJSON, ToJSON) via Autodocodec Manifest

instance HasCodec Manifest where
  codec =
    JsonSchema.namedObject "manifest" $
      Manifest
        <$> requiredField' "id" .= (.id)
        <*> requiredField' "name" .= (.name)
        <*> requiredField "description" "One or two short sentences that tell what the plug-in does. The preferences page shows it under the name." .= (.description)
        <*> requiredField' "version" .= (.version)
        <*> requiredField' "api" .= (.api)
        <*> optionalField "namespace" "The only XML namespace that a plug-in with manifest.write can write. It cannot be empty, a namespace of ASC MHL, or a namespace that XML reserves." .= (.namespace)
        <*> requiredFieldWith "executable" (mapCodec stringCodec) "Keys: linux-x86_64, linux-aarch64, macos-aarch64, macos-x86_64, windows-x86_64. Values: paths relative to the plug-in folder. A path does not start with '/' or '\\', holds no ':', and has no '..' part. One bad path, for any platform, rejects the plug-in on every platform." .= (.executable)
        <*> optionalFieldWithOmittedDefault "capabilities" V.empty "At least one of plan.inspect, files.inspect, manifest.write and job.finished." .= (.capabilities)
        <*> optionalFieldWithOmittedDefault "settings" V.empty "Keys are unique. At most one setting has the kind authors." .= (.settings)
        <*> optionalFieldWithOmittedDefault "jobFields" V.empty "Keys are unique. No job field has the kind authors." .= (.jobFields)

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
