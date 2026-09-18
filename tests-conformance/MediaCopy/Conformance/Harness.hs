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

import Ascmhl.History (History)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Floreal.Commons.Log (withStderrLogger)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (encodeUtf)

import MediaCopy.Domain.HistoryError (HistoryError)
import MediaCopy.Domain.Job qualified as Job
import MediaCopy.Domain.JobEvent (JobEvent)
import MediaCopy.Domain.JobId qualified as JobId
import MediaCopy.Domain.JobSpec (JobSpec)
import MediaCopy.Domain.JobSpec qualified as JobSpec
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Effects.AppStack (AppStack)
import MediaCopy.Effects.AppStack qualified as AppStack
import MediaCopy.Effects.Emit qualified as Emit
import MediaCopy.Effects.Plugins qualified as Plugins
import MediaCopy.Engine qualified as Engine
import MediaCopy.Engine.Planner qualified as Planner
import MediaCopy.Mhl.Store qualified as Store

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
  (Planner.plan spec >>= Plugins.runNone . Engine.execute "localhost")
    & Emit.runCollect
    & runLogged
    & fmap snd

runLogged :: Eff AppStack a -> IO a
runLogged action = withStderrLogger (\logger -> AppStack.run logger action)

offloadSpec :: FilePath -> FilePath -> IO JobSpec
offloadSpec source parent = offloadSpecWith Offload.UseHistory source parent

offloadSpecWith :: Offload.SealFirst -> FilePath -> FilePath -> IO JobSpec
offloadSpecWith sealFirst source parent = do
  src <- osPathOf source
  dst <- osPathOf parent
  pure
    JobSpec.JobSpec
      { id = JobId.JobId 1
      , job = Job.Offload Offload.Offload {source = src, destinations = dst :| [], sealFirst, existingCopy = Nothing}
      , createdAt = epoch
      , pluginFields = Map.empty
      }

verifySpec :: FilePath -> IO JobSpec
verifySpec folder = do
  f <- osPathOf folder
  pure JobSpec.JobSpec {id = JobId.JobId 1, job = Job.VerifyFolder Job.Verify {folder = f}, createdAt = epoch, pluginFields = Map.empty}

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

readHistoryIO :: FilePath -> IO (Either HistoryError (Maybe History))
readHistoryIO folder = do
  f <- osPathOf folder
  Store.readHistory f
    & runLogged
