module MediaCopy.Domain.Target
  ( Target (..)
  , State (..)
  ) where

data State = Fresh | NotEmpty | Absent | Partial
  deriving stock (Bounded, Enum, Eq, Show)

instance Display State where
  displayBuilder = \case
    Fresh -> "empty"
    NotEmpty -> "not empty"
    Absent -> "will be created"
    Partial -> "partial copy"

data Target = Target
  { root :: OsPath
  , freeBytes :: Maybe Int64
  , state :: State
  }
  deriving stock (Eq, Show)
