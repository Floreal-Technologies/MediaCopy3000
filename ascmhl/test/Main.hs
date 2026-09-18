module Main (main) where

import Test.Tasty

import Ascmhl.HashTest qualified as HashTest
import Ascmhl.PathTest qualified as PathTest
import Ascmhl.RoundTripTest qualified as RoundTripTest

main :: IO ()
main = defaultMain (testGroup "ascmhl" [HashTest.tests, PathTest.tests, RoundTripTest.tests])
