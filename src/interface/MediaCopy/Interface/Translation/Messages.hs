{-# LANGUAGE TemplateHaskell #-}

module MediaCopy.Interface.Translation.Messages where

import MediaCopy.Interface.Translation.English qualified as English
import MediaCopy.Interface.Translation.Splice (messageReferences)

messageReferences English.localeFile
