module Main (main) where

import Test.Tasty

import Ascmhl.FragmentTest qualified as FragmentTest
import Ascmhl.HashTest qualified as HashTest
import Ascmhl.ManifestTest qualified as ManifestTest
import Ascmhl.RelPathTest qualified as RelPathTest
import Ascmhl.RoundTripTest qualified as RoundTripTest

main :: IO ()
main = defaultMain (testGroup "ascmhl" [HashTest.tests, RelPathTest.tests, FragmentTest.tests, RoundTripTest.tests, ManifestTest.tests])
