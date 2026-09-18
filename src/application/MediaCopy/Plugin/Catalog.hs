module MediaCopy.Plugin.Catalog
  ( load
  , apply
  , editGrants
  ) where

import Ascmhl.OsPath qualified as OsPath
import Control.Applicative ((<|>))
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as V
import Effectful.Exception qualified as Exception
import Floreal.Commons.Environment (Environment)
import Floreal.Commons.Xdg (Xdg)
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.Field qualified as Plugin (Field)
import MediaCopy.Plugin.Id qualified as Id
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import MediaCopy.Plugin.Manifest qualified as Manifest
import System.OsPath (takeDirectory)

import MediaCopy.Domain.FieldView (FieldView)
import MediaCopy.Domain.FieldView qualified as FieldView
import MediaCopy.Domain.PluginCatalog (PluginCatalog)
import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog
import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Effects.FileSystem (FileSystem)
import MediaCopy.Effects.FileSystem qualified as FileSystem
import MediaCopy.Plugin.Discovery qualified as Discovery
import MediaCopy.Plugin.Fields qualified as Fields
import MediaCopy.Plugin.Grant qualified as Grant
import MediaCopy.Plugin.Grant qualified as Plugin (Grant)
import MediaCopy.Plugin.Installed qualified as Installed
import MediaCopy.Plugin.Installed qualified as Plugin (Installed)
import MediaCopy.Plugin.Setup qualified as Setup

load :: (FileSystem :> es, Environment :> es, Xdg :> es) => Eff es PluginCatalog
load = do
  setup <- Setup.load
  pure
    PluginCatalog.PluginCatalog
      { entries = V.map (entryOf setup.grants) setup.installed
      , rejected = V.map (\entry -> (OsPath.toText entry.folder, entry.reason)) setup.rejected
      , problem = setup.grantsProblem
      }

entryOf :: Map.Map Plugin.Id Plugin.Grant -> Plugin.Installed -> PluginCatalog.Entry
entryOf grantMap installed =
  PluginCatalog.Entry
    { plugin = PluginRef.PluginRef {id = pluginId, name = manifest.name}
    , version = manifest.version
    , description = manifest.description
    , folder = OsPath.toText installed.folder
    , enabled
    , trace = maybe False (.trace) grant
    , capabilities = V.map (\capability -> PluginCatalog.CapabilityView {capability, answer = answerOf capability}) manifest.capabilities
    , settings
    , jobFields = V.map (settingView Map.empty) manifest.jobFields <> V.concatMap (\view -> FieldView.authorFields view.key (FieldView.authorSlots view.value)) settings
    , active = isRight activated
    , problem = if enabled then inactive <|> unset else Nothing
    }
  where
    manifest = installed.manifest
    Id.Id pluginId = manifest.id
    grant = Map.lookup manifest.id grantMap
    enabled = maybe False (.enabled) grant
    answerOf capability = case grant of
      Just g
        | Set.member capability g.grants -> PluginCatalog.Granted
        | Set.member capability g.declined -> PluginCatalog.Declined
      _ -> PluginCatalog.Unanswered
    stored = maybe Map.empty (.settings) grant
    settings = V.map (settingView stored) manifest.settings
    activated = Setup.activate grantMap installed
    inactive = either (\entry -> Just entry.reason) (const Nothing) activated
    labelOf key = maybe key (.label) (V.find (\field -> field.key == key) manifest.settings)
    unset = either (\key -> Just ("needs a valid value for " <> labelOf key)) (const Nothing) (Fields.values manifest.settings stored)

settingView :: Map.Map Text Value -> Plugin.Field -> FieldView
settingView stored field = viewOf field $ case field.kind of
  Field.AuthorsField -> maybe FieldView.NoValue FieldView.AuthorList (Map.lookup field.key stored >>= parseMaybe parseJSON)
  _ -> maybe FieldView.NoValue (FieldView.Value . valueText) (Map.lookup field.key stored <|> field.defaultValue)

viewOf :: Plugin.Field -> FieldView.FieldValue -> FieldView
viewOf field value = FieldView.FieldView {key = field.key, label = field.label, kind = field.kind, required = field.required, value}

valueText :: Value -> Text
valueText = \case
  String text -> text
  Bool True -> "true"
  Bool False -> "false"
  other -> decodeUtf8Lenient (LBS.toStrict (encodePretty other))

apply :: (FileSystem :> es, Environment :> es, Xdg :> es) => PluginCatalog.Change -> Eff es (Either Text ())
apply change = do
  path <- Grant.path
  current <- Grant.readJson path
  case current >>= first ("plugins.json: " <>) . editGrants change of
    Left problem -> pure (Left problem)
    Right edited -> do
      written <- Exception.try @IOException $ do
        FileSystem.makeDirectories (V.singleton (takeDirectory path))
        FileSystem.writeTextAtomically path (decodeUtf8Lenient (LBS.toStrict (encodePretty edited)) <> "\n")
      pure (either (\e -> Left ("plugins.json cannot be written: " <> T.show e)) Right written)

editGrants :: PluginCatalog.Change -> Value -> Either Text Value
editGrants (PluginCatalog.Change pluginId edit) = \case
  Object root -> do
    plugins <- objectAt "plugins" root
    entry <- objectAt pluginId plugins
    let edited = editEntry entry
    Right (Object (KeyMap.insert "plugins" (Object (KeyMap.insert (Key.fromText pluginId) (Object edited) plugins)) root))
  _ -> Left "is not a JSON object"
  where
    objectAt name container = case KeyMap.lookup (Key.fromText name) container of
      Nothing -> Right KeyMap.empty
      Just (Object inner) -> Right inner
      Just _ -> Left (name <> " is not a JSON object")
    texts name entry = case KeyMap.lookup name entry of
      Just (Array values) -> [text | String text <- V.toList values]
      _ -> []
    setTexts name values entry = KeyMap.insert name (toJSON (Set.toList (Set.fromList values))) entry
    editEntry entry = case edit of
      PluginCatalog.SetEnabled on -> KeyMap.insert "enabled" (Bool on) entry
      PluginCatalog.SetTrace on -> KeyMap.insert "trace" (Bool on) entry
      PluginCatalog.SetAnswer capability answer ->
        let name = Capability.toText capability
            grants = filter (/= name) (texts "grants" entry)
            declined = filter (/= name) (texts "declined" entry)
        in case answer of
             PluginCatalog.Granted -> setTexts "grants" (name : grants) (setTexts "declined" declined entry)
             PluginCatalog.Declined -> setTexts "grants" grants (setTexts "declined" (name : declined) entry)
             PluginCatalog.Unanswered -> setTexts "grants" grants (setTexts "declined" declined entry)
      PluginCatalog.SetSetting key setting ->
        let settings = fromRight KeyMap.empty (objectAt "settings" entry)
            json = case setting of
              PluginCatalog.SettingText text -> String text
              PluginCatalog.SettingBool flag -> Bool flag
              PluginCatalog.SettingAuthors slots -> toJSON slots
        in KeyMap.insert "settings" (Object (KeyMap.insert (Key.fromText key) json settings)) entry
      PluginCatalog.ClearSetting key ->
        let settings = fromRight KeyMap.empty (objectAt "settings" entry)
        in KeyMap.insert "settings" (Object (KeyMap.delete (Key.fromText key) settings)) entry
