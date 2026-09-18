module MediaCopy.Domain.Offload
  ( Offload (..)
  , SealFirst (..)
  , OnSealFailure (..)
  , ExistingCopy (..)
  ) where

data SealFirst
  = UseHistory
  | SealBeforeCopy OnSealFailure
  deriving stock (Eq, Show)

data OnSealFailure = StopBeforeCopy | CopyAnyway
  deriving stock (Eq, Show)

instance Display OnSealFailure where
  displayBuilder = \case
    StopBeforeCopy -> "stop before copying"
    CopyAnyway -> "copy anyway"

data ExistingCopy = Resume | Replace
  deriving stock (Eq, Show)

instance Display ExistingCopy where
  displayBuilder = \case
    Resume -> "resume"
    Replace -> "replace"

data Offload = Offload
  { source :: OsPath
  , destinations :: NonEmpty OsPath
  , sealFirst :: SealFirst
  , existingCopy :: Maybe ExistingCopy
  }
  deriving stock (Eq, Show)
