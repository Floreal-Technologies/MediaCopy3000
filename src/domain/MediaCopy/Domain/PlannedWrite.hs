module MediaCopy.Domain.PlannedWrite
  ( PlannedWrite (..)
  , Mode (..)
  ) where

data Mode
  = New
  | Overwrite
  | Reuse
  deriving stock (Bounded, Enum, Eq, Show)

instance Display Mode where
  displayBuilder = \case
    New -> "copy"
    Overwrite -> "overwrite"
    Reuse -> "reuse"

data PlannedWrite = PlannedWrite
  { final :: OsPath
  , temp :: OsPath
  , mode :: Mode
  }
  deriving stock (Eq, Show)
