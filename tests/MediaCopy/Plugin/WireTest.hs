module MediaCopy.Plugin.WireTest (tests) where

import Data.Either (fromLeft)
import Data.Vector qualified as V
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.PluginRef qualified as PluginRef
import MediaCopy.Plugin.Wire qualified as Wire

tests :: TestTree
tests =
  testGroup
    "Plugin.Wire"
    [ testCase "contributionOf refuses an author that holds a character XML does not allow" contributionOfRefusesNonXmlAuthors
    ]

contributionOfRefusesNonXmlAuthors :: Assertion
contributionOfRefusesNonXmlAuthors =
  map (\(name, _) -> fromLeft "accepted" (Wire.contributionOf probe "urn:x" mempty (contribution name))) table @?= map snd table
  where
    table :: List (Text, Text)
    table =
      [ ("Jane\SOHDoe", "an author holds a character that XML does not allow: \"Jane\\SOHDoe\"")
      , ("Jane Doe", "accepted")
      ]
    probe = PluginRef.PluginRef {id = "tech.floreal.probe", name = "Probe"}
    contribution name =
      Contribute.Result
        { authors = V.singleton Contribute.Author {name, email = Nothing, phone = Nothing, role = Just "DIT"}
        , fileMetadata = V.empty
        , manifestMetadata = Nothing
        }
