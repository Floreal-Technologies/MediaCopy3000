module MediaCopy.Interface.ThemeTest (tests) where

import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Demo.Fixtures (paletteListing)
import MediaCopy.Interface.Theme
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Embedded

tests :: TestTree
tests =
  testGroup
    "Interface.Theme"
    [testCase "the dark list holds the sections the manual gives" darkListHoldsTheDocumentedSections]

darkListHoldsTheDocumentedSections :: Assertion
darkListHoldsTheDocumentedSections =
  map named (V.toList (themeSections (embeddedWording English) DarkPalette (palettesFrom "assets/themes" paletteListing)))
    @?= [ ("System", ["System dark"])
        , ("Catppuccin", ["Frappé", "Macchiato", "Mocha"])
        , ("Dracula", ["Dracula"])
        , ("Everforest", ["Hard", "Medium", "Soft"])
        , ("Kanagawa", ["Dragon", "Wave"])
        ]
  where
    named section = (section.heading, map (themeRowLabel (embeddedWording English)) (V.toList section.themes))
