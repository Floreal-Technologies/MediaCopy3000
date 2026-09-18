module Main (main) where

import Data.Text.IO qualified as T
import System.Environment (lookupEnv)
import Test.Tasty

import MediaCopy.Conformance.Ascmhl qualified as Ascmhl
import MediaCopy.Conformance.OursToReferenceTest qualified as OursToReferenceTest
import MediaCopy.Conformance.ReferenceToOursTest qualified as ReferenceToOursTest

main :: IO ()
main = do
  tools <- Ascmhl.findTools
  required <- lookupEnv "MEDIACOPY_CONFORMANCE"
  case tools of
    Nothing
      | required == Just "required" ->
          ioError (userError "MEDIACOPY_CONFORMANCE=required but ascmhl / ascmhl-debug are not on PATH; run: just deps-conformance")
      | otherwise ->
          T.putStrLn "conformance: ascmhl not on PATH, skipping. Run `just deps-conformance` to install it."
    Just found ->
      defaultMain
        ( testGroup
            "conformance"
            [ OursToReferenceTest.tests found
            , ReferenceToOursTest.tests found
            ]
        )
