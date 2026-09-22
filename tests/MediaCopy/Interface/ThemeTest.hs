module MediaCopy.Interface.ThemeTest (tests) where

import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Demo.Fixtures (paletteListing)
import MediaCopy.Interface.Theme (PaletteMode (..), ThemeSection (..), palettesFrom, themeRowLabel, themeSections)

tests :: TestTree
tests =
  testGroup
    "Interface.Theme"
    [testCase "the dark list holds the sections the manual gives" darkListHoldsTheDocumentedSections]

darkListHoldsTheDocumentedSections :: Assertion
darkListHoldsTheDocumentedSections =
  map named (V.toList (themeSections DarkPalette (palettesFrom "assets/themes" paletteListing)))
    @?= [ ("System", ["System dark"])
        , ("Catppuccin", ["Frappé", "Macchiato", "Mocha"])
        , ("Dracula", ["Dracula"])
        , ("Everforest", ["Hard", "Medium", "Soft"])
        , ("Kanagawa", ["Dragon", "Wave"])
        ]
  where
    named section = (section.heading, map themeRowLabel (V.toList section.themes))
