-- | The path a manifest entry names: relative to the folder the manifest sits in, and nothing else.
module Ascmhl.Path
  ( RelPath (..)
  , mkRelPath
  , relToOsPath
  , pathText
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
  Nothing -> T.pack (show p)
  Just s -> T.pack s

newtype RelPath = RelPath Text
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Display RelPath where
  displayBuilder (RelPath t) = displayBuilder t

-- | Accepts a path that is relative, not empty, and free of any @..@ component. A manifest is
-- portable, so the check never asks the host what an absolute path looks like: it refuses a leading
-- slash of either kind, a drive letter, and a @..@ between slashes of either kind. Windows
-- @isAbsolute@ says no to @/etc/passwd@, and 'relToOsPath' joins by the host's rules, so a Windows
-- host reads @..\\x@ as an escape. A POSIX name such as @back\\slash.mxf@ passes.
--
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

relToOsPath :: OsPath -> RelPath -> OsPath
relToOsPath root (RelPath t) = root </> unsafeEncodeUtf (T.unpack t)
