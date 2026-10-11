{-# LANGUAGE TemplateHaskell #-}

module MediaCopy.Resources (bundle) where

import Data.ByteString (ByteString)

import MediaCopy.Resources.Splice (compiledResources)

bundle :: ByteString
bundle = $(compiledResources "assets/tech.floreal.MediaCopy3000.gresource.xml")
