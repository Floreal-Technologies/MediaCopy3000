module MediaCopy.Domain.Destination
  ( Destination (..)
  , Classified (..)
  , classify
  , bytesNeeded
  , spaceFinding
  , writes
  , folderFor
  ) where

import Ascmhl.Chain (Chain)
import Ascmhl.Chain qualified as Chain
import Ascmhl.ChainEntry (ChainEntry)
import Ascmhl.ChainEntry qualified as ChainEntry
import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Ascmhl.RelPath qualified as RelPath
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import System.OsPath (takeFileName, (</>))

import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.Finding (Finding)
import MediaCopy.Domain.Finding qualified as Finding
import MediaCopy.Domain.HistoryError (HistoryError)
import MediaCopy.Domain.HistoryError qualified as HistoryError
import MediaCopy.Domain.Offload qualified as Offload
import MediaCopy.Domain.Part qualified as Part
import MediaCopy.Domain.PlanStep (PlanStep)
import MediaCopy.Domain.PlanStep qualified as PlanStep
import MediaCopy.Domain.PlannedWrite (PlannedWrite)
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite
import MediaCopy.Domain.Severity qualified as Severity
import MediaCopy.Domain.Target (Target)
import MediaCopy.Domain.Target qualified as Target
import MediaCopy.Domain.Tree (Tree)
import MediaCopy.Domain.Tree qualified as Tree

data Destination = Destination
  { root :: OsPath
  , freeBytes :: Maybe Int64
  , history :: Either HistoryError (Maybe Chain)
  , existing :: Maybe Tree
  }

data Held = Held
  { finals :: Map RelPath FileSize
  , firstForeign :: Maybe RelPath
  }

heldOf :: Set RelPath -> Set RelPath -> Tree -> Held
heldOf sourceFiles sourceDirs tree = Held {finals, firstForeign}
  where
    files = V.toList tree.files
    finals = files & filter (\pair -> Set.member (fst pair) sourceFiles) & Map.fromList
    partOfSource p = maybe False (\final -> Set.member final sourceFiles) (Part.strip p)
    foreignFiles = files & map fst & filter (\p -> not (Set.member p sourceFiles || partOfSource p))
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
      Nothing -> Just Finding.Finding {severity = Severity.Blocker, code = Finding.DestinationForeign, detail = "ascmhl/" <> display entry.path}
      Just theirs -> case (entry.c4, theirs.c4) of
        (Just mine, Just its)
          | mine /= its -> Just Finding.Finding {severity = Severity.Blocker, code = Finding.DestinationOtherSource, detail = display entry.path}
        _ -> Nothing

data Classified = Classified
  { target :: Target
  , finals :: Map RelPath FileSize
  , findings :: List Finding
  }

classify :: Maybe Offload.ExistingCopy -> Set RelPath -> Set RelPath -> Map RelPath ChainEntry -> Destination -> Classified
classify choice sourceFiles sourceDirs sourceByPath target = case target.existing of
  Nothing -> plain Target.Absent
  Just tree ->
    let held = heldOf sourceFiles sourceDirs tree
        destChain = HistoryError.readOr (Chain.Chain {entries = V.empty}) target.history
        hasParts = V.any (\entry -> entry & fst & Part.strip & isJust) tree.files
        empty = Map.null held.finals && isNothing held.firstForeign && not hasParts && V.null destChain.entries
        foreignFinding p = Finding.Finding {severity = Severity.Blocker, code = Finding.DestinationForeign, detail = display p}
        blocked = catMaybes [fmap foreignFinding held.firstForeign, chainCheck sourceByPath destChain]
        undecided = Finding.onlyIf (isNothing choice) Finding.Finding {severity = Severity.Blocker, code = Finding.DestinationPartial, detail = "choose Resume or Replace under Before Copying"}
    in if empty
         then plain Target.Fresh
         else
           Classified
             { target = Target.Target {root = target.root, freeBytes = free, state = if null blocked then Target.Partial else Target.NotEmpty}
             , finals = held.finals
             , findings = if null blocked then undecided else blocked
             }
  where
    free = target.freeBytes
    plain state = Classified {target = Target.Target {root = target.root, freeBytes = free, state}, finals = Map.empty, findings = []}

modeFor :: Maybe Offload.ExistingCopy -> Map RelPath FileSize -> (RelPath, FileSize) -> PlannedWrite.Mode
modeFor choice finals (path, size) = case (Map.lookup path finals, choice) of
  (Nothing, _) -> PlannedWrite.New
  (Just there, Just Offload.Resume) | there == size -> PlannedWrite.Reuse
  (Just _, _) -> PlannedWrite.Overwrite

bytesNeeded :: Int -> Vector PlanStep -> Vector Int64
bytesNeeded count steps =
  V.accumulate (+) (V.replicate count 0) (V.concatMap needs steps)
  where
    needs step = V.imapMaybe (\dest w -> if w.mode == PlannedWrite.Reuse then Nothing else Just (dest, step.size)) step.writes

spaceFinding :: Int64 -> Target -> Maybe Finding
spaceFinding needed target = case target.freeBytes of
  Nothing -> Just Finding.Finding {severity = Severity.Blocker, code = Finding.DestinationUnavailable, detail = OsPath.toText target.root}
  Just free
    | free < needed -> Just Finding.Finding {severity = Severity.Blocker, code = Finding.InsufficientSpace, detail = OsPath.toText target.root}
    | otherwise -> Nothing

writes :: Maybe Offload.ExistingCopy -> Vector (OsPath, Map RelPath FileSize) -> (RelPath, FileSize) -> Vector PlannedWrite
writes choice dests pair =
  V.map (\(dest, finals) -> let final = RelPath.toOsPath dest (fst pair) in PlannedWrite.PlannedWrite {final, temp = Part.path final, mode = modeFor choice finals pair}) dests

folderFor :: OsPath -> OsPath -> OsPath
folderFor parent source = parent </> takeFileName source
