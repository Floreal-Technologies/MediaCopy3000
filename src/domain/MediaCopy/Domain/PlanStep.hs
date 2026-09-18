module MediaCopy.Domain.PlanStep
  ( PlanStep (..)
  , FileOp (..)
  , Expected (..)
  , bytesToRead
  , verifyOps
  ) where

import Ascmhl.Hash (Hash)
import Ascmhl.RelPath (RelPath)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V

import MediaCopy.Domain.FileSize (FileSize)
import MediaCopy.Domain.PlannedWrite (PlannedWrite)
import MediaCopy.Domain.PlannedWrite qualified as PlannedWrite

data Expected
  = NoOriginal
  | Recorded Hash
  | FromSealPass
  deriving stock (Eq, Show)

data FileOp
  = Copy Expected
  | VerifyAgainst Hash
  | ReportMissing
  | ReportNew
  deriving stock (Eq, Show)

verifyOps :: Map RelPath Hash -> Vector RelPath -> Vector (RelPath, FileOp)
verifyOps manifest disk = onDisk <> missing
  where
    diskSet = Set.fromList (V.toList disk)
    onDisk = V.map (\p -> (p, maybe ReportNew VerifyAgainst (Map.lookup p manifest))) disk
    missing =
      Map.keys manifest
        & filter (\p -> not (Set.member p diskSet))
        & map (\p -> (p, ReportMissing))
        & V.fromList

data PlanStep = PlanStep
  { path :: RelPath
  , size :: FileSize
  , op :: FileOp
  , writes :: Vector PlannedWrite
  }
  deriving stock (Eq, Show)

bytesToRead :: PlanStep -> Int64
bytesToRead step =
  let reuses = V.length (V.filter (\w -> w.mode == PlannedWrite.Reuse) step.writes)
      allReuse = reuses == V.length step.writes
      hasExpected = case step.op of
        (Copy (Recorded _); Copy FromSealPass) -> True
        _ -> False
      sourceBytes = if allReuse && hasExpected then 0 else step.size
  in sourceBytes + fromIntegral (V.length step.writes) * step.size
