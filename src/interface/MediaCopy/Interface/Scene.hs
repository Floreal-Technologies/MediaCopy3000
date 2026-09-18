module MediaCopy.Interface.Scene
  ( Frame (..)
  ) where

import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Model (Model)

data Frame = Frame
  { model :: Model
  , action :: Maybe Command.Command
  , expand :: Bool
  , scroll :: Bool
  }
