{-# LANGUAGE TemplateHaskell #-}

module MediaCopy.Interface.Translation.Embedded
  ( embeddedWording
  ) where

import Control.Monad (unless)
import Data.Text qualified as T
import Language.Fluent.AST (Resource)
import Language.Fluent.TH ()
import Language.Haskell.TH (conP, lamCaseE, match, mkName, normalB)
import Language.Haskell.TH.Syntax (lift, runIO)

import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Coverage
import MediaCopy.Interface.Translation.Splice (readFtl)

embeddedWording :: SupportedLanguage -> Wording
embeddedWording language = mkWording language 0 (embeddedResource language)

embeddedResource :: SupportedLanguage -> Resource
embeddedResource =
  $( lamCaseE
       =<< traverse
         ( \language -> do
             let osPath = localeFile language
             resource <- readFtl osPath
             faults <- runIO (wordingFaults (mkWording language 0 resource))
             unless (null faults) (fail (show osPath <> ": messages fail:\n" <> T.unpack (T.unlines faults)))
             pure (match (conP (mkName (show language)) []) (normalB (lift resource)) [])
         )
         [minBound .. maxBound]
   )
