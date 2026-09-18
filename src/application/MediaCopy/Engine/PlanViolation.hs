module MediaCopy.Engine.PlanViolation
  ( PlanViolation (..)
  , orThrow
  ) where

import Ascmhl.OsPath qualified as OsPath
import Ascmhl.RelPath (RelPath)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Error.Static (Error, throwError)

import MediaCopy.Domain.DirectoryHash (DirectoryHashError)
import MediaCopy.Domain.HistoryError (HistoryError)

data PlanViolation
  = PlanFormatMissing
  | EntryWithoutHash RelPath
  | EntriesNotPlaced (Vector RelPath)
  | OriginalsUnresolved HistoryError
  | GenerationRaced OsPath Int Int
  | ManifestNameUnusable OsPath
  | HashUndecodable DirectoryHashError
  | HistoryFaultAt OsPath HistoryError
  | OriginalsMissingAfterSeal OsPath
  deriving stock (Eq, Show)

instance Display PlanViolation where
  displayBuilder = \case
    PlanFormatMissing -> "the plan settled no hash format"
    EntryWithoutHash path -> "ASC MHL: manifest entry carries no hash: " <> displayBuilder path
    EntriesNotPlaced paths ->
      "ASC MHL: manifest entries not placed in the tree: "
        <> displayBuilder (paths & V.toList & map display & T.intercalate ", ")
    OriginalsUnresolved e -> displayBuilder e
    GenerationRaced folder planned found ->
      "ASC MHL: the history moved under "
        <> displayBuilder (OsPath.toText folder)
        <> ": the plan named generation "
        <> displayBuilder (T.show planned)
        <> ", the chain now gives "
        <> displayBuilder (T.show found)
    ManifestNameUnusable path ->
      "ASC MHL: the manifest file name is not a relative path: " <> displayBuilder (OsPath.toText path)
    HashUndecodable err -> displayBuilder err
    HistoryFaultAt folder e ->
      "ASC MHL: the history under "
        <> displayBuilder (OsPath.toText folder)
        <> " cannot be read: "
        <> displayBuilder e
    OriginalsMissingAfterSeal source ->
      "ASC MHL: the seal wrote no history under " <> displayBuilder (OsPath.toText source)

orThrow :: (Error PlanViolation :> es) => Either PlanViolation a -> Eff es a
orThrow = either throwError pure
