-- | What the engine found self-contradictory in a plan that planning already passed.
module MediaCopy.Engine.Violation
  ( PlanViolation (..)
  , orThrow
  ) where

import Ascmhl.Path (RelPath, pathText)
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (Error, throwError)
import System.OsPath (OsPath)

import MediaCopy.Domain.DirectoryHash (DirectoryHashError)
import MediaCopy.Domain.History (HistoryError)

-- | Every one of these means the plan and the engine disagree about the same job. A plan
-- that planning accepted can never produce one. They are faults of this code, not of a media source.
data PlanViolation
  = -- | 'planBlocked' lets only a settled plan through, so nothing can reach a missing format.
    PlanFormatMissing
  | EntryWithoutHash RelPath
  | EntriesNotPlaced (Vector RelPath)
  | OriginalsUnresolved HistoryError
  | GenerationRaced OsPath Int Int
  | -- | The engine named the manifest, so a name that is not a relative path is the engine's fault.
    ManifestNameUnusable OsPath
  | -- | The domain's own error, rendered by the domain, so the sentence exists once.
    HashUndecodable DirectoryHashError
  | -- | A history this code already read, or just wrote, cannot be read now.
    HistoryFaultAt OsPath HistoryError
  | -- | The seal pass wrote a generation into the media source, so its history cannot be absent.
    OriginalsMissingAfterSeal OsPath
  deriving stock (Eq, Show)

instance Display PlanViolation where
  displayBuilder = \case
    PlanFormatMissing -> "the plan settled no hash format"
    EntryWithoutHash path -> "ASC MHL: manifest entry carries no hash: " <> displayBuilder path
    EntriesNotPlaced paths ->
      "ASC MHL: manifest entries not placed in the tree: "
        <> displayBuilder (paths & V.toList & map (\path -> display path) & T.intercalate ", ")
    OriginalsUnresolved e -> displayBuilder e
    GenerationRaced folder planned found ->
      "ASC MHL: the history moved under "
        <> displayBuilder (pathText folder)
        <> ": the plan named generation "
        <> displayBuilder (T.pack (show planned))
        <> ", the chain now gives "
        <> displayBuilder (T.pack (show found))
    ManifestNameUnusable path ->
      "ASC MHL: the manifest file name is not a relative path: " <> displayBuilder (pathText path)
    HashUndecodable err -> displayBuilder err
    HistoryFaultAt folder e ->
      "ASC MHL: the history under "
        <> displayBuilder (pathText folder)
        <> " cannot be read: "
        <> displayBuilder e
    OriginalsMissingAfterSeal source ->
      "ASC MHL: the seal wrote no history under " <> displayBuilder (pathText source)

-- | A pure decision reports its failure. This is the only place where one becomes the job's
-- failure.
orThrow :: (Error PlanViolation :> es) => Either PlanViolation a -> Eff es a
orThrow = either (\violation -> throwError violation) pure
