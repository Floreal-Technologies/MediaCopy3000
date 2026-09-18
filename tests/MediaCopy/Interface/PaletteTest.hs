module MediaCopy.Interface.PaletteTest (tests) where

import Floreal.Commons.Palette qualified as CommonsPalette
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Interface.Palette qualified as Palette
import MediaCopy.Interface.Translation (SupportedLanguage (..))
import MediaCopy.Interface.Translation.Embedded (embeddedWording)

tests :: TestTree
tests =
  testGroup
    "Interface.Palette"
    [ testCase "fold strips accents, lowers case and turns hyphens into spaces" foldStripsAccentsAndHyphens
    , testCase "a query opens the palette on the matching rows" aQueryOpensThePalette
    ]

foldStripsAccentsAndHyphens :: Assertion
foldStripsAccentsAndHyphens =
  map (\(input, _) -> CommonsPalette.fold input) table @?= map snd table
  where
    table :: List (Text, Text)
    table =
      [ ("Tâche suivante", "tache suivante")
      , ("new-offload", "new offload")
      , ("À propos", "a propos")
      ]

aQueryOpensThePalette :: Assertion
aQueryOpensThePalette = do
  let view = Palette.paletteView (embeddedWording English) (const True) (Just "pref")
  (view.open, view.query, map (\row -> row.entry.label) view.items) @?= (True, "pref", ["Preferences"])
