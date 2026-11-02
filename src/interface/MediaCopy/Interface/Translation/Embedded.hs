{-# LANGUAGE TemplateHaskell #-}

module MediaCopy.Interface.Translation.Embedded
  ( embeddedWording
  ) where

import Data.Text qualified as T
import Language.Fluent.AST (Resource)
import Language.Fluent.TH ()
import Language.Haskell.TH (conP, lamCaseE, match, mkName, normalB)
import Language.Haskell.TH.Syntax (lift, runIO)
import System.OsPath qualified as OsPath

import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Coverage
import MediaCopy.Interface.Translation.Splice (readFtl)

embeddedWording :: SupportedLanguage -> Wording
embeddedWording language = mkWording language 0 (embeddedResource language)

embeddedResource :: SupportedLanguage -> Resource
embeddedResource =
  $( do
       reference <- readFtl (localeFile English)
       referencePath <- OsPath.decodeUtf (localeFile English)
       let failOn path heading faults =
             unless (null faults) (fail (path <> heading <> ":\n" <> T.unpack (T.unlines faults)))
       translations <-
         forM (filter (/= English) [minBound .. maxBound]) $ \language -> do
           resource <- readFtl (localeFile language)
           path <- OsPath.decodeUtf (localeFile language)
           failOn path (": messages differ from " <> referencePath) (map ("  " <>) (messageShapeFaults reference resource))
           pure (language, resource)
       matches <-
         forM ((English, reference) : translations) $ \(language, resource) -> do
           path <- OsPath.decodeUtf (localeFile language)
           failOn path ": messages fail" =<< runIO (wordingFaults (mkWording language 0 resource))
           pure (match (conP (mkName (show language)) []) (normalB (lift resource)) [])
       lamCaseE matches
   )
