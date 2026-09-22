{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module MediaCopy.Effects.FileSystemTest (tests) where

import Control.Exception (finally)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (List)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Exception (throwIO, trySync)
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, decodeFS, takeDirectory, (</>))
import splice System.OsPath (osp)
import Test.Tasty
import Test.Tasty.HUnit

import MediaCopy.Domain.Plan (PlannedWrite (..), WriteMode (..))
import MediaCopy.Effects.FileSystem (FileSystem, ReadCache (..), removeTemps, runFileSystemIO, streamFile, writeTemps)
import MediaCopy.Test.InMemoryFS

tests :: TestTree
tests =
  testGroup
    "Effects.FileSystem"
    [ testCase "a hook that fails leaves no temp behind in the double" doubleRemovesTempsOnFailure
    , testCase "a hook that fails leaves no temp behind on the disk" diskRemovesTempsOnFailure
    , testCase "the double runs the flush hook once, after the last chunk" doubleFlushesAfterTheLastChunk
    , testCase "the disk runs the flush hook once, after the last chunk" diskFlushesAfterTheLastChunk
    , testCase "the double streams a file past one chunk in pieces" doubleStreamsInPieces
    , testCase "a cold read gives the same bytes as a warm read" coldReadMatchesWarmRead
    , testCase "the double removes the temps and keeps every final" doubleRemovesTempsOnly
    , testCase "the disk removes the temps and keeps every final" diskRemovesTempsOnly
    ]

doubleRemovesTempsOnly :: Assertion
doubleRemovesTempsOnly = do
  let writes = writesUnder [osp|/d1|] [osp|/d2|]
      seeded = foldr (\w fs -> fs & withFile w.final "copy" & withFile w.temp "part") emptyMemFS writes
  ref <- newIORef seeded
  runEff (runFileSystemMem ref (removeTemps writes))
  fs <- readIORef ref
  Map.keys fs.files @?= [[osp|/d1/a.mxf|], [osp|/d2/a.mxf|]]

diskRemovesTempsOnly :: Assertion
diskRemovesTempsOnly = do
  tmp <- Dir.getTemporaryDirectory
  let dir = tmp </> [osp|mediacopy3000-removetemps-test|]
      writes = writesUnder (dir </> [osp|d1|]) (dir </> [osp|d2|])
  present <- Dir.doesDirectoryExist dir
  when present (Dir.removeDirectoryRecursive dir)
  ( do
      V.forM_ writes $ \w -> do
        Dir.createDirectoryIfMissing True (takeDirectory w.final)
      V.forM_ writes $ \w -> do
        FileIO.writeFile w.final "copy"
        FileIO.writeFile w.temp "part"
      runEff (runFileSystemIO 4 (removeTemps writes))
      finals <- traverse (\w -> Dir.doesFileExist w.final) writes
      temps <- traverse (\w -> Dir.doesFileExist w.temp) writes
      (finals, temps) @?= ([True, True], [False, False])
    )
    `finally` Dir.removeDirectoryRecursive dir

failingWrite :: (FileSystem :> es) => OsPath -> Vector PlannedWrite -> Eff es Bool
failingWrite source writes =
  trySync (writeTemps source writes (pure ()) (\_chunk -> throwIO (userError "hook failed"))) <&> isLeft

doubleRemovesTempsOnFailure :: Assertion
doubleRemovesTempsOnFailure = do
  ref <- newIORef (emptyMemFS & withFile [osp|/src/a.mxf|] "payload")
  let writes = writesUnder [osp|/d1|] [osp|/d2|]
  failed <- runEff (runFileSystemMem ref (failingWrite [osp|/src/a.mxf|] writes))
  failed @?= True
  fs <- readIORef ref
  Map.keys fs.files @?= [[osp|/src/a.mxf|]]

diskRemovesTempsOnFailure :: Assertion
diskRemovesTempsOnFailure = do
  tmp <- Dir.getTemporaryDirectory
  let dir = tmp </> [osp|mediacopy3000-writetemps-test|]
      source = dir </> [osp|a.mxf|]
      writes = writesUnder (dir </> [osp|d1|]) (dir </> [osp|d2|])
  present <- Dir.doesDirectoryExist dir
  when present (Dir.removeDirectoryRecursive dir)
  Dir.createDirectoryIfMissing True dir
  ( do
      sourcePath <- decodeFS source
      BS.writeFile sourcePath "payload"
      failed <- runEff (runFileSystemIO 4 (failingWrite source writes))
      failed @?= True
      leftovers <- traverse (\w -> Dir.doesFileExist w.temp) (V.toList writes)
      leftovers @?= [False, False]
    )
    `finally` Dir.removeDirectoryRecursive dir

flushTrace :: (FileSystem :> es, IOE :> es) => IORef (List Bool) -> OsPath -> Vector PlannedWrite -> Eff es ()
flushTrace seen source writes =
  writeTemps
    source
    writes
    (liftIO (modifyIORef' seen (False :)))
    (\_chunk -> liftIO (modifyIORef' seen (True :)))
    & void

flushCameLast :: List Bool -> Assertion
flushCameLast trace = do
  length (filter not trace) @?= 1
  assertBool "expected more than one chunk before the flush" (length (filter id trace) > 1)
  last trace @?= False

doubleFlushesAfterTheLastChunk :: Assertion
doubleFlushesAfterTheLastChunk = do
  ref <- newIORef (emptyMemFS & withFile [osp|/src/a.mxf|] (BS.replicate 9000 7))
  seen <- newIORef []
  runEff (runFileSystemMem ref (flushTrace seen [osp|/src/a.mxf|] (writesUnder [osp|/d1|] [osp|/d2|])))
  readIORef seen >>= flushCameLast . reverse

diskFlushesAfterTheLastChunk :: Assertion
diskFlushesAfterTheLastChunk = do
  tmp <- Dir.getTemporaryDirectory
  let dir = tmp </> [osp|mediacopy3000-flushhook-test|]
      source = dir </> [osp|a.mxf|]
      writes = writesUnder (dir </> [osp|d1|]) (dir </> [osp|d2|])
  present <- Dir.doesDirectoryExist dir
  when present (Dir.removeDirectoryRecursive dir)
  Dir.createDirectoryIfMissing True dir
  ( do
      sourcePath <- decodeFS source
      BS.writeFile sourcePath "payload"
      seen <- newIORef []
      runEff (runFileSystemIO 4 (flushTrace seen source writes))
      readIORef seen >>= flushCameLast . reverse
    )
    `finally` Dir.removeDirectoryRecursive dir

writesUnder :: OsPath -> OsPath -> Vector PlannedWrite
writesUnder d1 d2 =
  V.fromList
    [ PlannedWrite {temp = d1 </> [osp|a.mxf.part|], final = d1 </> [osp|a.mxf|], mode = WriteNew}
    , PlannedWrite {temp = d2 </> [osp|a.mxf.part|], final = d2 </> [osp|a.mxf|], mode = WriteNew}
    ]

doubleStreamsInPieces :: Assertion
doubleStreamsInPieces = do
  let content = BS.replicate 9000 7
  ref <- newIORef (emptyMemFS & withFile [osp|/src/a.mxf|] content)
  seen <- newIORef []
  _ <- runEff (runFileSystemMem ref (streamFile FromCache [osp|/src/a.mxf|] (\chunk -> liftIO (modifyIORef' seen (\chunks -> chunk : chunks)))))
  chunks <- readIORef seen <&> reverse
  assertBool "expected more than one chunk" (length chunks > 1)
  BS.concat chunks @?= content

coldReadMatchesWarmRead :: Assertion
coldReadMatchesWarmRead = do
  tmp <- Dir.getTemporaryDirectory
  let dir = tmp </> [osp|mediacopy3000-coldread-test|]
      file = dir </> [osp|big.bin|]
      payload = BS.concat (replicate 3 (BS.replicate (1024 * 1024) 7)) <> BS.replicate 1024 9
  present <- Dir.doesDirectoryExist dir
  when present (Dir.removeDirectoryRecursive dir)
  Dir.createDirectoryIfMissing True dir
  ( do
      FileIO.writeFile' file payload
      warm <- collect FromCache file
      cold <- collect FromDevice file
      BS.length warm @?= BS.length payload
      cold @?= warm
    )
    `finally` Dir.removeDirectoryRecursive dir
  where
    collect mode path = do
      chunks <- newIORef []
      _ <- runEff (runFileSystemIO (4 * 1024 * 1024) (streamFile mode path (\bs -> liftIO (modifyIORef' chunks (bs :)))))
      readIORef chunks <&> \pieces -> BS.concat (reverse pieces)
