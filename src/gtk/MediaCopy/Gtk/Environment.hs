module MediaCopy.Gtk.Environment
  ( Environment (..)
  , readEnvironment
  ) where

import Data.Functor ((<&>))
import System.Environment (lookupEnv)

data Environment = Production | Development
  deriving stock (Eq, Show)

readEnvironment :: IO Environment
readEnvironment =
  lookupEnv "MC3K_ENV" <&> \case
    Just "dev" -> Development
    _ -> Production
