module MediaCopy.Domain.FileOutcome
  ( FileOutcome (..)
  , Mismatch (..)
  , failed
  ) where

import Ascmhl.Hash (Hash)

data Mismatch = Mismatch
  { expected :: Hash
  , actual :: Hash
  }
  deriving stock (Eq, Show)

data FileOutcome
  = Ok
  | HashMismatch Mismatch
  | Missing
  | New
  | IoError Text
  | Replaced Mismatch
  deriving stock (Eq, Show)

failed :: FileOutcome -> Bool
failed = \case
  (Ok; New; Replaced _) -> False
  (HashMismatch _; IoError _; Missing) -> True
