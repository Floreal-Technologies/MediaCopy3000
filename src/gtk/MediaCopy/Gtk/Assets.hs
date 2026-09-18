-- | Where a bundled asset comes from.
module MediaCopy.Gtk.Assets
  ( resolveAsset
  ) where

import System.Directory (doesPathExist)

import Paths_mediacopy3000 (getDataFileName)

-- | In order to support live reloading of assets, we resolve them
-- baed on their relative filepath and then on the @data-files@
-- directory.
resolveAsset :: FilePath -> IO FilePath
resolveAsset relative = do
  exists <- doesPathExist relative
  if exists then pure relative else getDataFileName relative
