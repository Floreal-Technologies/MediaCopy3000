module MediaCopy.Interface.Translation.CoverageTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Interface.Translation (parseFtl)
import MediaCopy.Interface.Translation.Coverage (messageShapeFaults)

tests :: TestTree
tests =
  testGroup
    "Interface.Translation.Coverage"
    [ testCase "messageShapeFaults reports missing messages and variables in reference order" reportsFaultsInReferenceOrder
    , testCase "messageShapeFaults counts a variable used only as a selector" countsSelectorVariables
    , testCase "messageShapeFaults ignores messages that only the translation has" ignoresExtraMessages
    , testCase "messageShapeFaults finds nothing when a resource is compared with itself" findsNothingForSameResource
    , testCase "messageShapeFaults reads the first copy of a duplicated message" readsFirstDuplicate
    ]

shapeFaults :: Text -> Text -> Either (NonEmpty Text) (List Text)
shapeFaults reference translation = messageShapeFaults <$> parseFtl reference <*> parseFtl translation

reportsFaultsInReferenceOrder :: Assertion
reportsFaultsInReferenceOrder =
  shapeFaults
    """
    hello = Hi { $name }
    bye = Bye
    """
    """
    hello = Salut { $who }
    """
    @?= Right ["message hello: missing variables: $name", "message hello: unknown variables: $who", "missing message: bye"]

countsSelectorVariables :: Assertion
countsSelectorVariables =
  shapeFaults
    """
    files = { $count ->
        [one] One file
       *[other] { $count } files
    }
    """
    """
    files = { $count ->
        [one] Un fichier
       *[other] Des fichiers
    }
    """
    @?= Right []

ignoresExtraMessages :: Assertion
ignoresExtraMessages =
  shapeFaults
    """
    bye = Bye
    """
    """
    bye = Au revoir
    extra = En plus
    """
    @?= Right []

findsNothingForSameResource :: Assertion
findsNothingForSameResource =
  shapeFaults reference reference @?= Right []
  where
    reference =
      """
      hello = Hi { $name }
      bye = Bye
      files = { $count ->
          [one] One file
         *[other] { $count } files
      }
      """

readsFirstDuplicate :: Assertion
readsFirstDuplicate =
  shapeFaults
    """
    hello = Hi { $name }
    """
    """
    hello = Salut
    hello = Salut { $name }
    """
    @?= Right ["message hello: missing variables: $name"]
