{-# LANGUAGE TemplateHaskellQuotes #-}

module MediaCopy.Interface.Translation.Splice (readFtl, messageReferences) where

import Data.Char (toLower, toUpper)
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Language.Fluent
import Language.Fluent.AST (Entry (..), Identifier (..), Message (..), Resource (..))
import Language.Haskell.TH (DecsQ, Q, mkName, normalB, sigD, valD, varP)
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject, runIO)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import MediaCopy.Interface.Translation

readFtl :: OsPath -> Q Resource
readFtl relative = do
  relativePath <- OsPath.decodeUtf relative
  path <- makeRelativeToProject relativePath
  addDependentFile path
  source <- runIO (Utf8.readFile path)
  case parseFtl source of
    Left junk -> fail (relativePath <> ": cannot parse:\n" <> T.unpack (T.unlines (NE.toList junk)))
    Right resource -> pure resource

messageReferences :: OsPath -> DecsQ
messageReferences relative = do
  resource <- readFtl relative
  let messages =
        resource.entries
          & List.concatMap
            ( \case
                MessageEntry message -> case message.id of
                  Identifier identifier -> [identifier]
                _ -> []
            )
  concat <$> traverse declare messages
  where
    declare identifier = do
      let name = mkName (camel identifier)
          literal = T.unpack identifier
      sequence
        [ sigD name [t|Reference|]
        , valD (varP name) (normalB [|fromString literal|]) []
        ]
    camel identifier = case T.splitOn "-" identifier of
      [] -> ""
      first' : rest -> T.unpack (T.concat (lower first' : map upper rest))
    lower = maybe "" (\(c, cs) -> T.cons (toLower c) cs) . T.uncons
    upper = maybe "" (\(c, cs) -> T.cons (toUpper c) cs) . T.uncons
