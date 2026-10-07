module MediaCopy.Plugin.Catalog
  ( loadCatalog
  , applyChange
  , editGrants
  ) where

import Ascmhl.Path (pathText)
import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Aeson (Value (..), eitherDecodeStrict, object, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight, isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as V
import MediaCopy.Plugin.Manifest
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import System.File.OsPath qualified as FileIO
import System.OsPath (takeDirectory)

import MediaCopy.Domain.Plugin (PluginRef (..))
import MediaCopy.Domain.PluginCatalog
import MediaCopy.Effects.FileSystem (writeTextAtomically)
import MediaCopy.Effects.Run (runApp)
import MediaCopy.Plugin.Discovery
import MediaCopy.Plugin.Grants
import MediaCopy.Plugin.Wire (fieldValues, slotsJson, slotsOf)

-- $setup
-- >>> import Data.Aeson (Value (..), object, (.=))
-- >>> import Data.Text (Text)
-- >>> import Data.Vector qualified as V

loadCatalog :: IO PluginCatalog
loadCatalog = do
  found <- pluginRoots >>= discover
  grantFile <- grantsPath >>= loadGrants
  let grantMap = fromRight Map.empty grantFile
  pure
    PluginCatalog
      { entries = V.map (entryOf grantMap) found.installed
      , rejected = V.map (\entry -> (pathText entry.folder, entry.reason)) found.rejected
      , problem = either Just (const Nothing) grantFile
      }

entryOf :: Map.Map PluginId Grant -> Installed -> PluginEntry
entryOf grantMap installed =
  PluginEntry
    { plugin = PluginRef {id = pluginId, name = manifest.name}
    , version = manifest.version
    , description = manifest.description
    , folder = pathText installed.folder
    , enabled
    , trace = maybe False (.trace) grant
    , capabilities = V.map (\capability -> CapabilityView {name = capabilityName capability, answer = answerOf capability}) manifest.capabilities
    , settings
    , jobFields = V.map (settingView Map.empty) manifest.jobFields <> V.concatMap (\view -> authorJobFields view.key (authorSlots view.value)) settings
    , active = isRight activated
    , problem = if enabled then inactive <|> unset else Nothing
    }
  where
    manifest = installed.manifest
    PluginId pluginId = manifest.id
    grant = Map.lookup manifest.id grantMap
    enabled = maybe False (.enabled) grant
    answerOf capability = case grant of
      Just g
        | Set.member capability g.grants -> Granted
        | Set.member capability g.declined -> Declined
      _ -> Unanswered
    stored = maybe Map.empty (.settings) grant
    settings = V.map (settingView stored) manifest.settings
    activated = activate grantMap installed
    inactive = either (\entry -> Just entry.reason) (const Nothing) activated
    labelOf key = maybe key (.label) (V.find (\field -> field.key == key) manifest.settings)
    unset = either (\key -> Just ("needs a valid value for " <> labelOf key)) (const Nothing) (fieldValues manifest.settings stored)

settingView :: Map.Map Text Value -> Field -> FieldView
settingView stored field = viewOf field $ case field.kind of
  AuthorsField -> maybe NoValue AuthorList (Map.lookup field.key stored >>= slotsOf)
  _ -> maybe NoValue (Value . valueText) (Map.lookup field.key stored <|> field.defaultValue)

viewOf :: Field -> FieldValue -> FieldView
viewOf field value = FieldView {key = field.key, label = field.label, shape = shapeOf field.kind, required = field.required, value}

shapeOf :: FieldKind -> FieldShape
shapeOf = \case
  TextField -> TextShape
  SecretField -> TextShape
  BoolField -> BoolShape
  ChoiceField choices -> ChoiceShape choices
  PathField -> PathShape
  AuthorsField -> AuthorsShape

valueText :: Value -> Text
valueText = \case
  String text -> text
  Bool True -> "true"
  Bool False -> "false"
  other -> decodeUtf8Lenient (LBS.toStrict (encodePretty other))

applyChange :: CatalogChange -> IO (Either Text ())
applyChange change = do
  path <- grantsPath
  present <- doesFileExist path
  current <-
    if present
      then do
        bytes <- try @IOException (FileIO.readFile' path)
        pure (either (Left . T.show) (first T.pack . eitherDecodeStrict) bytes)
      else pure (Right (object []))
  case current >>= editGrants change of
    Left problem -> pure (Left ("plugins.json: " <> problem))
    Right edited -> do
      written <- try @IOException $ do
        createDirectoryIfMissing True (takeDirectory path)
        runApp (writeTextAtomically path (decodeUtf8Lenient (LBS.toStrict (encodePretty edited)) <> "\n"))
      pure (either (\e -> Left ("plugins.json cannot be written: " <> T.show e)) Right written)

-- |
-- >>> editGrants (SetAnswer "tech.floreal.probe" "block" Granted) (object [])
-- Right (Object (fromList [("plugins",Object (fromList [("tech.floreal.probe",Object (fromList [("declined",Array []),("grants",Array [String "block"])]))]))]))
-- >>> editGrants (SetAnswer "tech.floreal.probe" "block" Declined) (object ["plugins" .= object ["tech.floreal.probe" .= object ["grants" .= ["block" :: Text], "note" .= ("kept" :: Text)]]])
-- Right (Object (fromList [("plugins",Object (fromList [("tech.floreal.probe",Object (fromList [("declined",Array [String "block"]),("grants",Array []),("note",String "kept")]))]))]))
-- >>> editGrants (ClearSetting "tech.floreal.probe" "camera") (object ["plugins" .= object ["tech.floreal.probe" .= object ["settings" .= object ["camera" .= ("B" :: Text), "dit" .= ("Jane" :: Text)]]]])
-- Right (Object (fromList [("plugins",Object (fromList [("tech.floreal.probe",Object (fromList [("settings",Object (fromList [("dit",String "Jane")]))]))]))]))
-- >>> editGrants (SetTrace "tech.floreal.probe" True) (object ["plugins" .= object ["tech.floreal.probe" .= object ["enabled" .= True]]])
-- Right (Object (fromList [("plugins",Object (fromList [("tech.floreal.probe",Object (fromList [("enabled",Bool True),("trace",Bool True)]))]))]))
-- >>> editGrants (SetSetting "tech.floreal.probe" "offline" (SettingBool True)) (Array mempty)
-- Left "is not a JSON object"
-- >>> editGrants (SetSetting "tech.floreal.credits" "authors" (SettingAuthors (V.singleton (AuthorSlot "DIT" "Jane Doe" "" "")))) (object [])
-- Right (Object (fromList [("plugins",Object (fromList [("tech.floreal.credits",Object (fromList [("settings",Object (fromList [("authors",Array [Object (fromList [("email",String ""),("name",String "Jane Doe"),("phone",String ""),("role",String "DIT")])])]))]))]))]))
editGrants :: CatalogChange -> Value -> Either Text Value
editGrants change = \case
  Object root -> do
    plugins <- objectAt "plugins" root
    let pluginId = changedPlugin change
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
    editEntry entry = case change of
      SetEnabled _ on -> KeyMap.insert "enabled" (Bool on) entry
      SetTrace _ on -> KeyMap.insert "trace" (Bool on) entry
      SetAnswer _ capability answer ->
        let grants = filter (/= capability) (texts "grants" entry)
            declined = filter (/= capability) (texts "declined" entry)
        in case answer of
             Granted -> setTexts "grants" (capability : grants) (setTexts "declined" declined entry)
             Declined -> setTexts "grants" grants (setTexts "declined" (capability : declined) entry)
             Unanswered -> setTexts "grants" grants (setTexts "declined" declined entry)
      SetSetting _ key setting ->
        let settings = fromRight KeyMap.empty (objectAt "settings" entry)
            json = case setting of
              SettingText text -> String text
              SettingBool flag -> Bool flag
              SettingAuthors slots -> slotsJson slots
        in KeyMap.insert "settings" (Object (KeyMap.insert (Key.fromText key) json settings)) entry
      ClearSetting _ key ->
        let settings = fromRight KeyMap.empty (objectAt "settings" entry)
        in KeyMap.insert "settings" (Object (KeyMap.delete (Key.fromText key) settings)) entry

changedPlugin :: CatalogChange -> Text
changedPlugin = \case
  SetEnabled pluginId _ -> pluginId
  SetTrace pluginId _ -> pluginId
  SetAnswer pluginId _ _ -> pluginId
  SetSetting pluginId _ _ -> pluginId
  ClearSetting pluginId _ -> pluginId
