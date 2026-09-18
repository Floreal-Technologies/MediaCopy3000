module MediaCopy.Domain.Job
  ( Job (..)
  , Kind (..)
  , Verify (..)
  , Seal (..)
  , kind
  , root
  , historyFolder
  , label
  ) where

import Ascmhl.OsPath qualified as OsPath
import System.OsPath (takeFileName)

import MediaCopy.Domain.Offload (Offload)
import MediaCopy.Domain.Offload qualified as Offload

data Kind = OffloadKind | VerifyKind | SealKind
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Display Kind where
  displayBuilder = \case
    OffloadKind -> "offload"
    VerifyKind -> "verify"
    SealKind -> "seal"

data Verify = Verify
  { folder :: OsPath
  }
  deriving stock (Eq, Show)

data Seal = Seal
  { folder :: OsPath
  }
  deriving stock (Eq, Show)

data Job
  = Offload Offload
  | VerifyFolder Verify
  | SealMediaSource Seal
  deriving stock (Eq, Show)

kind :: Job -> Kind
kind = \case
  Offload _ -> OffloadKind
  VerifyFolder _ -> VerifyKind
  SealMediaSource _ -> SealKind

root :: Job -> OsPath
root = \case
  Offload oj -> oj.source
  VerifyFolder vj -> vj.folder
  SealMediaSource sj -> sj.folder

historyFolder :: Job -> Maybe OsPath
historyFolder = \case
  Offload _ -> Nothing
  VerifyFolder vj -> Just vj.folder
  SealMediaSource sj -> Just sj.folder

label :: Job -> Text
label job = OsPath.toText (takeFileName (root job))
