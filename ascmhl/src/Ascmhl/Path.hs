module Ascmhl.Path
  ( RelPath (..)
  , mkRelPath
  , relToOsPath
  , pathText
  , parentOf
  , baseName
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))

-- |
-- >>> pathText (unsafeEncodeUtf "a.mxf")
-- "a.mxf"
pathText :: OsPath -> Text
pathText p = case decodeUtf p of
  Nothing -> T.show p
  Just s -> T.pack s

newtype RelPath = RelPath Text
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Display RelPath where
  displayBuilder (RelPath t) = displayBuilder t

-- |
-- >>> mkRelPath "card/a.mxf"
-- Just (RelPath "card/a.mxf")
-- >>> mkRelPath ""
-- Nothing
-- >>> mkRelPath "/etc/passwd"
-- Nothing
-- >>> mkRelPath "\\windows\\system32"
-- Nothing
-- >>> mkRelPath "C:/media"
-- Nothing
-- >>> mkRelPath "card/../../etc"
-- Nothing
-- >>> mkRelPath "back\\slash.mxf"
-- Just (RelPath "back\\slash.mxf")
mkRelPath :: Text -> Maybe RelPath
mkRelPath t
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
-- >>> parentOf (RelPath "card/clips/a.mxf")
-- RelPath "card/clips"
-- >>> parentOf (RelPath "a.mxf")
-- RelPath ""
parentOf :: RelPath -> RelPath
parentOf (RelPath t) = case T.breakOnEnd "/" t of
  (before, _) -> RelPath (T.dropEnd 1 before)

-- |
-- >>> baseName (RelPath "card/clips/a.mxf")
-- "a.mxf"
-- >>> baseName (RelPath "a.mxf")
-- "a.mxf"
baseName :: RelPath -> Text
baseName (RelPath t) = case T.breakOnEnd "/" t of
  (_, after) -> after

relToOsPath :: OsPath -> RelPath -> OsPath
relToOsPath root (RelPath t) = root </> unsafeEncodeUtf (T.unpack t)
