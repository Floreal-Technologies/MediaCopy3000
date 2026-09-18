module Main (main) where

import System.Environment (lookupEnv)
import Test.Tasty

import MediaCopy.Conformance.Ascmhl (findTools)
import MediaCopy.Conformance.InteropTest qualified as InteropTest
import MediaCopy.Conformance.OursToReferenceTest qualified as OursToReferenceTest
import MediaCopy.Conformance.ReferenceToOursTest qualified as ReferenceToOursTest

main :: IO ()
main = do
  tools <- findTools
  required <- lookupEnv "MEDIACOPY_CONFORMANCE"
  case tools of
    Nothing
      | required == Just "required" ->
          ioError (userError "MEDIACOPY_CONFORMANCE=required but ascmhl / ascmhl-debug are not on PATH; run: just deps-conformance")
      | otherwise ->
          putStrLn "conformance: ascmhl not on PATH, skipping. Run `just deps-conformance` to install it."
    Just found ->
      defaultMain
        ( testGroup
            "conformance"
            [ OursToReferenceTest.tests found
            , ReferenceToOursTest.tests found
            , InteropTest.tests found
            ]
        )
