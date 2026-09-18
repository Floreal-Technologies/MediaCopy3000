module MediaCopy.Plugin.CatalogTest (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.Capability qualified as Capability
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.PluginCatalog qualified as PluginCatalog
import MediaCopy.Plugin.Catalog qualified as Catalog

tests :: TestTree
tests =
  testGroup
    "Plugin.Catalog"
    [ testCase "editGrants changes only the edited part of the plug-in entry and refuses a non-object file" editGrantsChangesOnlyTheEditedPart
    ]

editGrantsChangesOnlyTheEditedPart :: Assertion
editGrantsChangesOnlyTheEditedPart =
  map (\(change, current, _) -> Catalog.editGrants change current) table @?= map (\(_, _, expected) -> expected) table
  where
    table :: List (PluginCatalog.Change, Value, Either Text Value)
    table =
      [
        ( PluginCatalog.Change "tech.floreal.probe" (PluginCatalog.SetAnswer Capability.Block PluginCatalog.Granted)
        , object []
        , Right (plugins "tech.floreal.probe" ["declined" .= noTexts, "grants" .= ["block" :: Text]])
        )
      ,
        ( PluginCatalog.Change "tech.floreal.probe" (PluginCatalog.SetAnswer Capability.Block PluginCatalog.Declined)
        , plugins "tech.floreal.probe" ["grants" .= ["block" :: Text], "note" .= ("kept" :: Text)]
        , Right (plugins "tech.floreal.probe" ["declined" .= ["block" :: Text], "grants" .= noTexts, "note" .= ("kept" :: Text)])
        )
      ,
        ( PluginCatalog.Change "tech.floreal.probe" (PluginCatalog.ClearSetting "camera")
        , plugins "tech.floreal.probe" ["settings" .= object ["camera" .= ("B" :: Text), "dit" .= ("Jane" :: Text)]]
        , Right (plugins "tech.floreal.probe" ["settings" .= object ["dit" .= ("Jane" :: Text)]])
        )
      ,
        ( PluginCatalog.Change "tech.floreal.probe" (PluginCatalog.SetTrace True)
        , plugins "tech.floreal.probe" ["enabled" .= True]
        , Right (plugins "tech.floreal.probe" ["enabled" .= True, "trace" .= True])
        )
      ,
        ( PluginCatalog.Change "tech.floreal.probe" (PluginCatalog.SetSetting "offline" (PluginCatalog.SettingBool True))
        , Array mempty
        , Left "is not a JSON object"
        )
      ,
        ( PluginCatalog.Change "tech.floreal.credits" (PluginCatalog.SetSetting "authors" (PluginCatalog.SettingAuthors (V.singleton (AuthorSlot.AuthorSlot "DIT" "Jane Doe" "" ""))))
        , object []
        , Right
            ( plugins
                "tech.floreal.credits"
                [ "settings"
                    .= object
                      [ "authors"
                          .= [object ["email" .= ("" :: Text), "name" .= ("Jane Doe" :: Text), "phone" .= ("" :: Text), "role" .= ("DIT" :: Text)]]
                      ]
                ]
            )
        )
      ]
    noTexts = [] :: List Text
    plugins pluginId entry = object ["plugins" .= object [pluginId .= object entry]]
