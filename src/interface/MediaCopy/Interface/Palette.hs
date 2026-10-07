module MediaCopy.Interface.Palette where

import Data.Char (GeneralCategory (NonSpacingMark), generalCategory, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.List (List, sortOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Normalize (NormalizationMode (NFD), normalize)
import Data.Vector (Vector)
import Data.Vector qualified as V

import MediaCopy.Interface.Command
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Model (Model (..), commandEnabled)

data Target = OnLabel | OnId
  deriving stock (Eq, Show)

data Match = Match
  { score :: Int
  , target :: Target
  , hits :: List Int
  }
  deriving stock (Eq, Show)

foldChar :: Char -> List Char
foldChar c
  | c == '-' = [' ']
  | otherwise =
      T.unpack (normalize NFD (T.singleton c))
        & filter (\d -> generalCategory d /= NonSpacingMark)
        & map toLower

-- |
-- >>> fold "Tâche suivante"
-- "tache suivante"
-- >>> fold "new-offload"
-- "new offload"
-- >>> fold "À propos"
-- "a propos"
fold :: Text -> Text
fold = T.pack . concatMap foldChar . T.unpack

indexed :: Text -> Vector (Int, Char)
indexed text = V.fromList (concat (zipWith (\i c -> map (\d -> (i, d)) (foldChar c)) [0 ..] (T.unpack text)))

matchText :: Target -> Text -> Text -> Maybe Match
matchText target query candidate
  | T.null needle = Just Match {score = 0, target, hits = []}
  | otherwise = do
      found <- walk (T.unpack needle) 0
      let bonus = if needle `T.isInfixOf` T.pack (map snd (V.toList stream)) then 10 else 0
      pure
        Match
          { score = sum (zipWith hitScore (Nothing : map Just found) found) + bonus
          , target
          , hits = nubOrd (map (\position -> fst (stream V.! position)) found)
          }
  where
    needle = T.strip (fold query)
    stream = indexed candidate
    walk :: List Char -> Int -> Maybe (List Int)
    walk [] _ = Just []
    walk (q : qs) from = do
      position <- V.findIndex (\(_, c) -> c == q) (V.drop from stream)
      let here = from + position
      rest <- walk qs (here + 1)
      pure (here : rest)
    hitScore :: Maybe Int -> Int -> Int
    hitScore previous here
      | here == 0 || snd (stream V.! (here - 1)) == ' ' = 3
      | previous == Just (here - 1) = 2
      | otherwise = 1

bestMatch :: Wording -> Text -> Command -> Maybe Match
bestMatch wording query command =
  listToMaybe (sortOn (Down . (.score)) (catMaybes [matchText OnLabel query (commandLabel wording command), matchText OnId query (commandId command)]))

rankCommands :: Wording -> Text -> List (Command, Match)
rankCommands wording query =
  sortOn (\(_, match) -> Down match.score) (mapMaybe (\command -> fmap (\match -> (command, match)) (bestMatch wording query command)) commands)

data PaletteRow = PaletteRow
  { command :: Command
  , match :: Match
  , enabled :: Bool
  }
  deriving stock (Eq, Show)

paletteRows :: Model -> List PaletteRow
paletteRows model = case model.palette of
  Nothing -> []
  Just query ->
    [ PaletteRow {command, match, enabled = commandEnabled model command}
    | (command, match) <- rankCommands model.wording query
    , command /= CommandPalette
    ]
