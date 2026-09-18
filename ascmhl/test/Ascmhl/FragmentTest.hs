module Ascmhl.FragmentTest (tests) where

import Data.Foldable (for_)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Ascmhl.Fragment qualified as Fragment

tests :: TestTree
tests =
  testGroup
    "Fragment"
    [ testCase "parse keeps each element in the namespace" keepsElementsInTheNamespace
    , testCase "parse drops comments and processing instructions" dropsCommentsAndProcessingInstructions
    , testCase "parse refuses each broken rule with its message" fragmentRules
    ]

keepsElementsInTheNamespace :: Assertion
keepsElementsInTheNamespace =
  fmap (\(Fragment.Fragment nodes) -> length nodes) (Fragment.parse "urn:x" "<a xmlns=\"urn:x\">1</a> <b xmlns=\"urn:x\"/>")
    @?= Right 2

dropsCommentsAndProcessingInstructions :: Assertion
dropsCommentsAndProcessingInstructions =
  Fragment.parse "urn:x" "<a xmlns=\"urn:x\"><!-- v2 -- beta --><?pi x?>1</a>"
    @?= Fragment.parse "urn:x" "<a xmlns=\"urn:x\">1</a>"

fragmentRules :: Assertion
fragmentRules =
  for_ refused $ \(input, expected) ->
    Fragment.parse "urn:x" input @?= Left expected

refused :: List (Text, Text)
refused =
  [ ("<a xmlns=\"urn:y\"/>", "metadata: <a> is in namespace urn:y, not urn:x")
  , ("<a xmlns=\"urn:x\"><b xmlns=\"\"/></a>", "metadata: <b> is in no namespace, not urn:x")
  , ("loose text", "metadata: text outside an element")
  , ("<a xmlns=\"urn:x\" xmlns:m=\"urn:ASC:MHL:v2.0\" m:size=\"1\"/>", "metadata: <a> has the attribute size in namespace urn:ASC:MHL:v2.0, not urn:x")
  , ("<a xmlns=\"urn:x\">a\SOHb</a>", "metadata: <a> holds a character that XML does not allow")
  , ("<a xmlns=\"urn:x\" b=\"\SOH\"/>", "metadata: <a> holds a character that XML does not allow")
  , (T.replicate 33 "<a xmlns=\"urn:x\">" <> T.replicate 33 "</a>", "metadata: more than 32 levels of elements")
  ]
