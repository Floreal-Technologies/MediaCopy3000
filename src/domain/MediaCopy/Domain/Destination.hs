module MediaCopy.Domain.Destination
  ( TargetFacts (..)
  , Classified (..)
  , classify
  , modeFor
  , neededPerDest
  , targetFinding
  , writesFor
  ) where

import Ascmhl.Path (RelPath, pathText, relToOsPath)
import Ascmhl.Types (Chain (..), ChainEntry (..))
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (List)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.OsPath (OsPath)

import MediaCopy.Domain.FileSystem (Tree (..), partPath, stripPart)
import MediaCopy.Domain.History (HistoryError, readOr)
import MediaCopy.Domain.Job (ExistingCopy (..), FileSize)
import MediaCopy.Domain.Plan

-- $setup
-- >>> import Ascmhl.Path (RelPath (..))

data TargetFacts = TargetFacts
  { root :: OsPath
  , freeBytes :: Maybe Int64
  , history :: Either HistoryError (Maybe Chain)
  , existing :: Maybe Tree
  }

data EntryKind = HeldFinal FileSize | PartOfSource | Foreign

data Held = Held
  { finals :: Map RelPath FileSize
  , firstForeign :: Maybe RelPath
  }

heldOf :: Set RelPath -> Set RelPath -> Tree -> Held
heldOf sourceFiles sourceDirs tree = Held {finals, firstForeign}
  where
    kindOf (p, size)
      | Set.member p sourceFiles = HeldFinal size
      | maybe False (\final -> Set.member final sourceFiles) (stripPart p) = PartOfSource
      | otherwise = Foreign
    kinds = tree.files & V.toList & map (\pair -> (fst pair, kindOf pair))
    finals = kinds & mapMaybe (\(p, kind) -> case kind of HeldFinal size -> Just (p, size); _ -> Nothing) & Map.fromList
    foreignFiles = kinds & mapMaybe (\(p, kind) -> case kind of Foreign -> Just p; _ -> Nothing)
    foreignDirs = tree.dirs & V.toList & filter (\d -> not (Set.member d sourceDirs))
    firstForeign = case foreignFiles <> foreignDirs of
      [] -> Nothing
      offenders -> Just (minimum offenders)

chainCheck :: Map RelPath ChainEntry -> Chain -> Maybe Finding
chainCheck sourceByPath destChain =
  destChain.entries
    & V.toList
    & mapMaybe check
    & listToMaybe
  where
    check entry = case Map.lookup entry.path sourceByPath of
      Nothing -> Just Finding {severity = Blocker, code = DestinationForeign, detail = "ascmhl/" <> display entry.path}
      Just theirs -> case (entry.c4, theirs.c4) of
        (Just mine, Just its)
          | mine /= its -> Just Finding {severity = Blocker, code = DestinationOtherSource, detail = display entry.path}
        _ -> Nothing

data Classified = Classified
  { target :: Target
  , finals :: Map RelPath FileSize
  , findings :: List Finding
  }

classify :: Maybe ExistingCopy -> Set RelPath -> Set RelPath -> Map RelPath ChainEntry -> TargetFacts -> Classified
classify choice sourceFiles sourceDirs sourceByPath target = case target.existing of
  Nothing -> plain Absent
  Just tree ->
    let held = heldOf sourceFiles sourceDirs tree
        destChain = readOr (Chain {entries = V.empty}) target.history
        hasParts = V.any (\pair -> isJust (stripPart (fst pair))) tree.files
        empty = Map.null held.finals && isNothing held.firstForeign && not hasParts && V.null destChain.entries
        foreignFinding p = Finding {severity = Blocker, code = DestinationForeign, detail = display p}
        blocked = catMaybes [fmap foreignFinding held.firstForeign, chainCheck sourceByPath destChain]
        undecided = findingIf (isNothing choice) Finding {severity = Blocker, code = DestinationPartial, detail = "choose Resume or Replace under Before Copying"}
    in if empty
         then plain Fresh
         else
           Classified
             { target = Target {root = target.root, freeBytes = free, state = if null blocked then Partial else NotEmpty}
             , finals = held.finals
             , findings = if null blocked then undecided else blocked
             }
  where
    free = target.freeBytes
    plain state = Classified {target = Target {root = target.root, freeBytes = free, state}, finals = Map.empty, findings = []}

-- |
-- >>> modeFor Nothing Map.empty (RelPath "a.mxf", 10)
-- WriteNew
-- >>> modeFor (Just Resume) (Map.fromList [(RelPath "a.mxf", 10)]) (RelPath "a.mxf", 10)
-- Reuse
-- >>> modeFor (Just Resume) (Map.fromList [(RelPath "a.mxf", 9)]) (RelPath "a.mxf", 10)
-- Overwrite
-- >>> modeFor (Just Replace) (Map.fromList [(RelPath "a.mxf", 10)]) (RelPath "a.mxf", 10)
-- Overwrite
modeFor :: Maybe ExistingCopy -> Map RelPath FileSize -> (RelPath, FileSize) -> WriteMode
modeFor choice finals (path, size) = case (Map.lookup path finals, choice) of
  (Nothing, _) -> WriteNew
  (Just there, Just Resume) | there == size -> Reuse
  (Just _, _) -> Overwrite

neededPerDest :: Int -> Vector PlanStep -> Vector Int64
neededPerDest count steps =
  V.accumulate (+) (V.replicate count 0) (V.concatMap needs steps)
  where
    needs step = V.imapMaybe (\dest w -> if w.mode == Reuse then Nothing else Just (dest, step.size)) step.writes

targetFinding :: Int64 -> Target -> Maybe Finding
targetFinding needed target = case target.freeBytes of
  Nothing -> Just Finding {severity = Blocker, code = DestinationUnavailable, detail = pathText target.root}
  Just free
    | free < needed -> Just Finding {severity = Blocker, code = InsufficientSpace, detail = pathText target.root}
    | otherwise -> Nothing

writesFor :: Maybe ExistingCopy -> Vector (OsPath, Map RelPath FileSize) -> (RelPath, FileSize) -> Vector PlannedWrite
writesFor choice dests pair =
  V.map (\(dest, finals) -> let final = relToOsPath dest (fst pair) in PlannedWrite {final, temp = partPath final, mode = modeFor choice finals pair}) dests
