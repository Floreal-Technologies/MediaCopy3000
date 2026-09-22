module MediaCopy.Conformance.Harness
  ( withTempTree
  , writeTreeFile
  , runEngineIO
  , offloadSpec
  , offloadSpecWith
  , verifySpec
  , mhlFilesIn
  , chainFileIn
  , readHistoryIO
  ) where

import Ascmhl.Types (MhlHistory)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (List, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector (Vector)
import Effectful
import Effectful.Time (runTime)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeUtf)

import MediaCopy.Domain.History (HistoryError)
import MediaCopy.Domain.Job
import MediaCopy.Effects.Emit
import MediaCopy.Effects.FileSystem (defaultChunkSize, runFileSystemIO)
import MediaCopy.Effects.Hasher (runHasherIO)
import MediaCopy.Engine (defaultToolInfo, readHistory, runJob)

withTempTree :: String -> (FilePath -> IO a) -> IO a
withTempTree label use = withSystemTempDirectory label use

writeTreeFile :: FilePath -> ByteString -> IO ()
writeTreeFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path content

osPathOf :: FilePath -> IO OsPath
osPathOf p = case encodeUtf p of
  Nothing -> ioError (userError ("path is not valid UTF-8: " <> p))
  Just osp' -> pure osp'

runEngineIO :: JobSpec -> IO (Vector JobEvent)
runEngineIO spec =
  runJob defaultToolInfo spec
    & runEmitCollect
    & runTime
    & runHasherIO
    & runFileSystemIO defaultChunkSize
    & runEff
    & fmap (\result -> snd result)

offloadSpec :: FilePath -> FilePath -> IO JobSpec
offloadSpec source parent = offloadSpecWith UseHistory source parent

offloadSpecWith :: SealFirst -> FilePath -> FilePath -> IO JobSpec
offloadSpecWith sealFirst source parent = do
  src <- osPathOf source
  dst <- osPathOf parent
  pure
    JobSpec
      { jobId = JobId 1
      , job = Offload OffloadJob {source = src, destinations = dst :| [], sealFirst, existingCopy = Nothing}
      , createdAt = epoch
      }

verifySpec :: FilePath -> IO JobSpec
verifySpec folder = do
  f <- osPathOf folder
  pure JobSpec {jobId = JobId 1, job = VerifyFolder VerifyJob {folder = f}, createdAt = epoch}

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

mhlFilesIn :: FilePath -> IO (List FilePath)
mhlFilesIn folder = do
  names <- listDirectory (folder </> "ascmhl")
  names
    & filter (\name -> takeExtension name == ".mhl")
    & sort
    & map (\name -> folder </> "ascmhl" </> name)
    & pure

chainFileIn :: FilePath -> FilePath
chainFileIn folder = folder </> "ascmhl" </> "ascmhl_chain.xml"

readHistoryIO :: FilePath -> IO (Either HistoryError (Maybe MhlHistory))
readHistoryIO folder = do
  f <- osPathOf folder
  readHistory f
    & runFileSystemIO defaultChunkSize
    & runEff
