module MediaCopy.Conformance.Ascmhl
  ( Tools (..)
  , Outcome (..)
  , findTools
  , schemaCheckManifest
  , schemaCheckChain
  , verifyFolder
  , verifyDirectoryHashes
  , createHistory
  , infoFolder
  , assertOk
  , assertFails
  , assertSchemaValidManifest
  ) where

import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Tasty.HUnit

data Tools = Tools
  { ascmhl :: FilePath
  , ascmhlDebug :: FilePath
  }

data Outcome = Outcome
  { exitCode :: Int
  , out :: Text
  , err :: Text
  }

findTools :: IO (Maybe Tools)
findTools = do
  create <- findExecutable "ascmhl"
  debug <- findExecutable "ascmhl-debug"
  pure (fmap (\pair -> Tools {ascmhl = fst pair, ascmhlDebug = snd pair}) (pairOf create debug))
  where
    pairOf a b = do
      x <- a
      y <- b
      Just (x, y)

run :: FilePath -> List String -> IO Outcome
run exe args = do
  (code, stdoutText, stderrText) <- readProcessWithExitCode exe args ""
  let asInt = case code of
        ExitSuccess -> 0
        ExitFailure n -> n
  pure Outcome {exitCode = asInt, out = T.pack stdoutText, err = T.pack stderrText}

manifestXsd :: FilePath
manifestXsd = "tests/fixtures/xsd/ASCMHL.xsd"

chainXsd :: FilePath
chainXsd = "tests/fixtures/xsd/ASCMHLDirectory__combined.xsd"

schemaCheckManifest :: Tools -> FilePath -> IO Outcome
schemaCheckManifest tools file = run tools.ascmhlDebug ["xsd-schema-check", "-xsd", manifestXsd, file]

schemaCheckChain :: Tools -> FilePath -> IO Outcome
schemaCheckChain tools file = run tools.ascmhlDebug ["xsd-schema-check", "-df", "-xsd", chainXsd, file]

verifyFolder :: Tools -> FilePath -> IO Outcome
verifyFolder tools folder = run tools.ascmhlDebug ["verify", folder]

verifyDirectoryHashes :: Tools -> FilePath -> IO Outcome
verifyDirectoryHashes tools folder = run tools.ascmhlDebug ["verify", "-dh", folder]

createHistory :: Tools -> Text -> FilePath -> IO Outcome
createHistory tools format folder = run tools.ascmhl ["create", "-h", T.unpack format, folder]

infoFolder :: Tools -> FilePath -> IO Outcome
infoFolder tools folder = run tools.ascmhl ["info", "-v", folder]

assertOk :: String -> Outcome -> Assertion
assertOk what outcome =
  assertBool
    (what <> " failed with exit " <> show outcome.exitCode <> "\nstdout:\n" <> T.unpack outcome.out <> "\nstderr:\n" <> T.unpack outcome.err)
    (outcome.exitCode == 0)

assertFails :: String -> Outcome -> Assertion
assertFails what outcome =
  assertBool
    (what <> " unexpectedly succeeded\nstdout:\n" <> T.unpack outcome.out)
    (outcome.exitCode /= 0)

assertSchemaValidManifest :: Tools -> FilePath -> Assertion
assertSchemaValidManifest tools file = do
  outcome <- schemaCheckManifest tools file
  assertOk ("xsd-schema-check " <> file) outcome
