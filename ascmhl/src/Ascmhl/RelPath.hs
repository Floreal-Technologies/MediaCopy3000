module Ascmhl.RelPath
  ( RelPath (..)
  , parse
  , toOsPath
  , parent
  , baseName
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

newtype RelPath = RelPath Text
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Display RelPath where
  displayBuilder (RelPath t) = displayBuilder t

parse :: Text -> Maybe RelPath
parse t
  | T.null t = Nothing
  | T.take 1 t `elem` ["/", "\\"] = Nothing
  | hasDriveLetter = Nothing
  | ".." `elem` T.split isSlash t = Nothing
  | otherwise = Just (RelPath t)
  where
    isSlash c = c == '/' || c == '\\'
    hasDriveLetter = case T.unpack (T.take 2 t) of
      [letter, ':'] -> isAsciiUpper letter || isAsciiLower letter
      _ -> False

-- |
-- >>> parent (RelPath "card/clips/a.mxf")
-- RelPath "card/clips"
-- >>> parent (RelPath "a.mxf")
-- RelPath ""
parent :: RelPath -> RelPath
parent (RelPath t) = case T.breakOnEnd "/" t of
  (before, _) -> RelPath (T.dropEnd 1 before)

-- |
-- >>> baseName (RelPath "card/clips/a.mxf")
-- "a.mxf"
-- >>> baseName (RelPath "a.mxf")
-- "a.mxf"
baseName :: RelPath -> Text
baseName (RelPath t) = case T.breakOnEnd "/" t of
  (_, after) -> after

toOsPath :: OsPath -> RelPath -> OsPath
toOsPath root (RelPath t) = root </> unsafeEncodeUtf (T.unpack t)
