module MediaCopy.Domain.PluginCatalogueTest (tests) where

import Data.Vector qualified as V
import MediaCopy.Plugin.Field qualified as Field
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.FieldView qualified as FieldView
import MediaCopy.Domain.PluginCatalogue qualified as PluginCatalogue
import MediaCopy.Domain.PluginRef qualified as PluginRef

tests :: TestTree
tests =
  testGroup
    "Domain.PluginCatalogue"
    [ testCase "only enabled and active plug-ins with job fields give job fields" onlyEnabledActivePluginsGiveJobFields
    ]

onlyEnabledActivePluginsGiveJobFields :: Assertion
onlyEnabledActivePluginsGiveJobFields =
  ( PluginCatalogue.enabledJobFields PluginCatalogue.PluginCatalogue {entries = V.fromList [entry "a" True True, entry "b" False True, entry "c" True False], rejected = V.empty, problem = Nothing}
      & V.toList
      & map (\(ref, fields) -> (ref.id, V.length fields))
  )
    @?= [("a", 1)]
  where
    field key = FieldView.FieldView {key, label = key, kind = Field.TextField, required = True, value = FieldView.NoValue}
    entry ident on running =
      PluginCatalogue.Entry
        { plugin = PluginRef.PluginRef {id = ident, name = ident}
        , version = "1"
        , description = ""
        , folder = ""
        , enabled = on
        , trace = False
        , capabilities = V.empty
        , settings = V.empty
        , jobFields = V.singleton (field "operator")
        , active = running
        , problem = Nothing
        }
