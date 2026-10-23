module MediaCopy.Plugin.CatalogueTest (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Vector qualified as V
import MediaCopy.Plugin.AuthorSlot qualified as AuthorSlot
import MediaCopy.Plugin.Capability qualified as Capability
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.PluginCatalogue qualified as PluginCatalogue
import MediaCopy.Plugin.Catalogue qualified as Catalogue

tests :: TestTree
tests =
  testGroup
    "Plugin.Catalogue"
    [ testCase "editGrants changes only the edited part of the plug-in entry and refuses a non-object file" editGrantsChangesOnlyTheEditedPart
    ]

editGrantsChangesOnlyTheEditedPart :: Assertion
editGrantsChangesOnlyTheEditedPart =
  map (\(change, current, _) -> Catalogue.editGrants change current) table @?= map (\(_, _, expected) -> expected) table
  where
    table :: List (PluginCatalogue.Change, Value, Either Text Value)
    table =
      [
        ( PluginCatalogue.Change "tech.floreal.probe" (PluginCatalogue.SetAnswer Capability.Block PluginCatalogue.Granted)
        , object []
        , Right (plugins "tech.floreal.probe" ["declined" .= noTexts, "grants" .= ["block" :: Text]])
        )
      ,
        ( PluginCatalogue.Change "tech.floreal.probe" (PluginCatalogue.SetAnswer Capability.Block PluginCatalogue.Declined)
        , plugins "tech.floreal.probe" ["grants" .= ["block" :: Text], "note" .= ("kept" :: Text)]
        , Right (plugins "tech.floreal.probe" ["declined" .= ["block" :: Text], "grants" .= noTexts, "note" .= ("kept" :: Text)])
        )
      ,
        ( PluginCatalogue.Change "tech.floreal.probe" (PluginCatalogue.ClearSetting "camera")
        , plugins "tech.floreal.probe" ["settings" .= object ["camera" .= ("B" :: Text), "dit" .= ("Jane" :: Text)]]
        , Right (plugins "tech.floreal.probe" ["settings" .= object ["dit" .= ("Jane" :: Text)]])
        )
      ,
        ( PluginCatalogue.Change "tech.floreal.probe" (PluginCatalogue.SetTrace True)
        , plugins "tech.floreal.probe" ["enabled" .= True]
        , Right (plugins "tech.floreal.probe" ["enabled" .= True, "trace" .= True])
        )
      ,
        ( PluginCatalogue.Change "tech.floreal.probe" (PluginCatalogue.SetSetting "offline" (PluginCatalogue.SettingBool True))
        , Array mempty
        , Left "is not a JSON object"
        )
      ,
        ( PluginCatalogue.Change "tech.floreal.credits" (PluginCatalogue.SetSetting "authors" (PluginCatalogue.SettingAuthors (V.singleton (AuthorSlot.AuthorSlot "DIT" "Jane Doe" "" ""))))
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
