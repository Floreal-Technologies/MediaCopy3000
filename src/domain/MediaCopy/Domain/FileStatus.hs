module MediaCopy.Domain.FileStatus
  ( FileStatus (..)
  , isFailure
  , isDone
  ) where

import MediaCopy.Domain.FileOutcome (FileOutcome)
import MediaCopy.Domain.FileOutcome qualified as FileOutcome

data FileStatus
  = Pending
  | Hashing
  | Copying
  | Flushing
  | Publishing
  | Verifying
  | Done FileOutcome
  deriving stock (Eq, Show)

instance Display FileStatus where
  displayBuilder = \case
    Pending -> "Pending"
    Hashing -> "Hashing"
    Copying -> "Copying"
    Flushing -> "Saving to disk"
    Publishing -> "Naming the copy"
    Verifying -> "Verifying"
    Done FileOutcome.Ok -> "Verified"
    Done (FileOutcome.HashMismatch _) -> "Hash mismatch"
    Done FileOutcome.Missing -> "Missing"
    Done FileOutcome.New -> "New (not in manifest)"
    Done (FileOutcome.IoError message) -> "I/O error: " <> displayBuilder message
    Done (FileOutcome.Replaced _) -> "Replaced after mismatch"

isFailure :: FileStatus -> Bool
isFailure = \case
  Done outcome -> FileOutcome.failed outcome
  _ -> False

isDone :: FileStatus -> Bool
isDone = \case
  Done _ -> True
  _ -> False
