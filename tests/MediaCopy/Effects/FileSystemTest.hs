{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Effects.FileSystemTest (tests) where

import Control.Exception (finally)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful.Exception (throwIO, trySync)
import Hedgehog (Property, evalIO, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, takeDirectory, (</>))
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

import MediaCopy.Domain.Plan (PlannedWrite (..), WriteMode (..))
import MediaCopy.Effects.FileSystem (FileSystem, ReadCache (..), removeTemps, runFileSystemIO, streamFile, writeTemps)
import MediaCopy.Test.InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Effects.FileSystem"
    [ contract "the double" withMemory
    , contract "the disk" withDisk
    , testProperty "streamed bytes equal the file, cold and warm, at chunk edges" streamedBytesEqualTheFile
    ]

data Sandbox = Sandbox
  { root :: OsPath
  , seed :: OsPath -> ByteString -> IO ()
  , exists :: OsPath -> IO Bool
  , run :: forall a. Eff '[FileSystem, IOE] a -> IO a
  }

runIn :: Sandbox -> Eff '[FileSystem, IOE] a -> IO a
runIn Sandbox {run} = run

type WithSandbox = forall r. OsPath -> (Sandbox -> IO r) -> IO r

contract :: TestName -> WithSandbox -> TestTree
contract name withSandbox =
  testGroup
    name
    [ testCase "a write that fails leaves no temp behind" (withSandbox [osp|mediacopy3000-failedwrite-test|] aFailedWriteLeavesNoTemp)
    , testCase "the flush hook runs once, after the last chunk" (withSandbox [osp|mediacopy3000-flush-test|] flushRunsOnceAfterTheLastChunk)
    , testCase "removing the temps keeps every final" (withSandbox [osp|mediacopy3000-removetemps-test|] removingTempsKeepsFinals)
    ]

aFailedWriteLeavesNoTemp :: Sandbox -> Assertion
aFailedWriteLeavesNoTemp box = do
  let source = box.root </> [osp|a.mxf|]
      writes = writesIn box.root
  box.seed source "payload"
  failed <- runIn box (trySync (writeTemps source writes (pure ()) (\_chunk -> throwIO (userError "hook failed"))) <&> isLeft)
  failed @?= True
  leftovers <- traverse (\w -> box.exists w.temp) (V.toList writes)
  leftovers @?= [False, False]

flushRunsOnceAfterTheLastChunk :: Sandbox -> Assertion
flushRunsOnceAfterTheLastChunk box = do
  let source = box.root </> [osp|a.mxf|]
  box.seed source (BS.replicate 9000 7)
  seen <- newIORef []
  writeTemps
    source
    (writesIn box.root)
    (liftIO (modifyIORef' seen (\trace -> False : trace)))
    (\_chunk -> liftIO (modifyIORef' seen (\trace -> True : trace)))
    & void
    & runIn box
  trace <- readIORef seen <&> reverse
  length (filter not trace) @?= 1
  assertBool "expected more than one chunk before the flush" (length (filter id trace) > 1)
  last trace @?= False

removingTempsKeepsFinals :: Sandbox -> Assertion
removingTempsKeepsFinals box = do
  let writes = writesIn box.root
  V.forM_ writes $ \w -> do
    box.seed w.final "copy"
    box.seed w.temp "part"
  runIn box (removeTemps writes)
  finals <- traverse (\w -> box.exists w.final) (V.toList writes)
  temps <- traverse (\w -> box.exists w.temp) (V.toList writes)
  (finals, temps) @?= ([True, True], [False, False])

writesIn :: OsPath -> Vector PlannedWrite
writesIn root =
  V.fromList
    [ PlannedWrite {temp = root </> [osp|d1/a.mxf.part|], final = root </> [osp|d1/a.mxf|], mode = WriteNew}
    , PlannedWrite {temp = root </> [osp|d2/a.mxf.part|], final = root </> [osp|d2/a.mxf|], mode = WriteNew}
    ]

withMemory :: WithSandbox
withMemory _ use = do
  ref <- newIORef emptyMemFS
  use
    Sandbox
      { root = [osp|/fs|]
      , seed = \path content -> modifyIORef' ref (withFile path content)
      , exists = \path -> readIORef ref <&> \fs -> Map.member (slashedPath path) fs.files
      , run = \action -> runEff (runFileSystemMem ref action)
      }

withDisk :: WithSandbox
withDisk name use = do
  tmp <- Dir.getTemporaryDirectory
  let dir = tmp </> name
  present <- Dir.doesDirectoryExist dir
  when present (Dir.removeDirectoryRecursive dir)
  Dir.createDirectoryIfMissing True dir
  use
    Sandbox
      { root = dir
      , seed = \path content -> do
          Dir.createDirectoryIfMissing True (takeDirectory path)
          FileIO.writeFile' path content
      , exists = Dir.doesFileExist
      , run = \action -> runEff (runFileSystemIO chunkBytes action)
      }
    `finally` Dir.removeDirectoryRecursive dir

chunkBytes :: Int
chunkBytes = 4096

streamedBytesEqualTheFile :: Property
streamedBytesEqualTheFile = withTests 30 $ property $ do
  chunks <- forAll (Gen.int (Range.linear 0 3))
  offset <- forAll (Gen.element [-1, 0, 1])
  fill <- forAll (Gen.word8 Range.constantBounded)
  let content = BS.replicate (max 0 (chunks * chunkBytes + offset)) fill
  streamed <- evalIO $ withDisk [osp|mediacopy3000-stream-test|] $ \disk -> do
    let file = disk.root </> [osp|a.mxf|]
    disk.seed file content
    cold <- collect disk FromDevice file
    warm <- collect disk FromCache file
    withMemory [osp|unused|] $ \memory -> do
      memory.seed [osp|/a.mxf|] content
      double <- collect memory FromCache [osp|/a.mxf|]
      pure [cold, warm, double]
  streamed === replicate 3 content
  where
    collect box mode path = do
      seen <- newIORef []
      _ <- runIn box (streamFile mode path (\chunk -> liftIO (modifyIORef' seen (\pieces -> chunk : pieces))))
      readIORef seen <&> \pieces -> pieces & reverse & BS.concat
