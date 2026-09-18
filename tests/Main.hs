module Main (main) where

import Test.Tasty

import MediaCopy.Domain.DirectoryHashTest qualified as DirectoryHashTest
import MediaCopy.Domain.HashFormatTest qualified as HashFormatTest
import MediaCopy.Domain.JobTest qualified as JobTest
import MediaCopy.Domain.PartTest qualified as PartTest
import MediaCopy.Domain.PluginCatalogTest qualified as PluginCatalogTest
import MediaCopy.Domain.PluginReportTest qualified as PluginReportTest
import MediaCopy.Effects.FileSystemTest qualified as FileSystemTest
import MediaCopy.FlowTest qualified as FlowTest
import MediaCopy.Interface.PaletteTest qualified as PaletteTest
import MediaCopy.Interface.View.PlanSheetTest qualified as PlanSheetTest
import MediaCopy.Interface.WordingTest qualified as WordingTest
import MediaCopy.ModelTest qualified as ModelTest
import MediaCopy.Plugin.CatalogTest qualified as CatalogTest
import MediaCopy.Plugin.FieldsTest qualified as FieldsTest
import MediaCopy.Plugin.SetupTest qualified as SetupTest
import MediaCopy.Plugin.WireTest qualified as WireTest

main :: IO ()
main =
  defaultMain
    ( testGroup
        "mediacopy3000"
        [ DirectoryHashTest.tests
        , HashFormatTest.tests
        , JobTest.tests
        , PartTest.tests
        , PluginCatalogTest.tests
        , PluginReportTest.tests
        , FileSystemTest.tests
        , ModelTest.tests
        , PaletteTest.tests
        , PlanSheetTest.tests
        , WordingTest.tests
        , CatalogTest.tests
        , FieldsTest.tests
        , SetupTest.tests
        , WireTest.tests
        , FlowTest.tests
        ]
    )
