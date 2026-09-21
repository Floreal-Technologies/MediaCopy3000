-- | Whether this run is a development run or an installed one.
module MediaCopy.Gtk.Environment
  ( Environment (..)
  , readEnvironment
  ) where

import Data.Functor ((<&>))
import System.Environment (lookupEnv)

data Environment = Production | Development
  deriving stock (Eq, Show)

-- | @MC3K_ENV=dev@ turns on the development helpers. The runtime reads it once.
readEnvironment :: IO Environment
readEnvironment =
  lookupEnv "MC3K_ENV" <&> \case
    Just "dev" -> Development
    _ -> Production
