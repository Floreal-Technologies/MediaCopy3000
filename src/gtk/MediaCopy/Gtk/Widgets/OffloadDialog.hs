-- | The "New Offload" dialog: one source folder and a list of destinations.
module MediaCopy.Gtk.Widgets.OffloadDialog
  ( OffloadDialog
  , newOffloadDialog
  , renderOffloadDialog
  ) where

import Ascmhl.Path (pathText)
import Data.Function ((&))
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import System.OsPath (OsPath, takeFileName)

import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Model (Model (..), OffloadDraft (..), UiMessage (..), draftReady)

data OffloadDialog = OffloadDialog
  { draftCell :: Cell (Maybe OffloadDraft)
  , openCell :: Cell Bool
  }

newOffloadDialog :: Adw.ApplicationWindow -> (UiMessage -> IO ()) -> IO OffloadDialog
newOffloadDialog window dispatch = do
  (sourceGroup, sourceRow) <- newSourceGroup dispatch
  (destGroup, addRow) <- newDestGroup dispatch
  body <- paddedBox Gtk.OrientationVertical 18 12
  Gtk.boxAppend body sourceGroup
  Gtk.boxAppend body destGroup
  shell <-
    newDialogShell
      [#title := "New Offload", #contentWidth := 480]
      ShellButtons
        { leading = ShellButton {label = "_Cancel", clicked = dispatch CloseOffloadDialog}
        , primary = ShellButton {label = "_Review Plan…", clicked = dispatch ReviewPlan}
        }
      []
      body
  destRows <- newIORef V.empty
  let form = DraftForm {sourceRow, destGroup, addRow, destRows, reviewButton = shell.primaryButton}
  draftCell <- newCell (renderDraft form dispatch)
  openCell <- newOpenCell shell.dialog window
  onDialogClosed shell.dialog openCell (dispatch CloseOffloadDialog)
  pure OffloadDialog {draftCell, openCell}

-- | What a draft paint writes: the source line, the destination list, and the button that leaves.
data DraftForm = DraftForm
  { sourceRow :: Adw.ActionRow
  , destGroup :: Adw.PreferencesGroup
  , addRow :: Adw.ActionRow
  , destRows :: IORef (Vector Adw.ActionRow)
  , reviewButton :: Gtk.Button
  }

newSourceGroup :: (UiMessage -> IO ()) -> IO (Adw.PreferencesGroup, Adw.ActionRow)
newSourceGroup dispatch = do
  sourceGroup <- new Adw.PreferencesGroup [#title := "Source"]
  sourceRow <- new Adw.ActionRow [#title := "No folder chosen"]
  chooseButton <-
    new
      Gtk.Button
      [ #label := "Ch_oose…"
      , #useUnderline := True
      , #valign := Gtk.AlignCenter
      , On #clicked (dispatch PickSource)
      ]
  Gtk.widgetAddCssClass chooseButton "flat"
  Adw.actionRowAddSuffix sourceRow chooseButton
  Adw.preferencesGroupAdd sourceGroup sourceRow
  pure (sourceGroup, sourceRow)

-- | An activatable row, not an 'Adw.ButtonRow': that row arrives in libadwaita 1.6, above the
-- floor the oldest supported distribution sets. See .tasks/lessons.md.
newDestGroup :: (UiMessage -> IO ()) -> IO (Adw.PreferencesGroup, Adw.ActionRow)
newDestGroup dispatch = do
  destGroup <-
    new
      Adw.PreferencesGroup
      [ #title := "Destinations"
      , #description := ""
      ]
  addRow <-
    new
      Adw.ActionRow
      [ #title := "_Add Destination…"
      , #useUnderline := True
      , #activatable := True
      , On #activated (dispatch AddDestination)
      ]
  addIcon <- new Gtk.Image [#iconName := "list-add-symbolic"]
  Adw.actionRowAddPrefix addRow addIcon
  Adw.preferencesGroupAdd destGroup addRow
  pure (destGroup, addRow)

renderDraft :: DraftForm -> (UiMessage -> IO ()) -> Maybe OffloadDraft -> IO ()
renderDraft form dispatch = mapM_ $ \draft -> do
  renderSourceRow form.sourceRow draft.mediaSource
  -- The render lifts the add button out and puts it back, so it always sits below the destinations.
  Adw.preferencesGroupRemove form.destGroup form.addRow
  renderActionRows form.destRows (InGroup form.destGroup) (draft.destinations & V.fromList & V.imap (destRow dispatch))
  Adw.preferencesGroupAdd form.destGroup form.addRow
  set form.reviewButton [#sensitive := draftReady draft]

renderSourceRow :: Adw.ActionRow -> Maybe OsPath -> IO ()
renderSourceRow sourceRow = \case
  Nothing -> set sourceRow [#title := "No folder chosen", #subtitle := ""]
  Just folder -> set sourceRow [#title := pathText (takeFileName folder), #subtitle := pathText folder]

destRow :: (UiMessage -> IO ()) -> Int -> OsPath -> Row
destRow dispatch index folder =
  (plainRow (pathText (takeFileName folder)) (pathText folder)) {suffix = Just (removeButton dispatch index folder)}

removeButton :: (UiMessage -> IO ()) -> Int -> OsPath -> IO Gtk.Widget
removeButton dispatch index folder = do
  button <-
    new
      Gtk.Button
      [ #iconName := "edit-delete-symbolic"
      , #valign := Gtk.AlignCenter
      , #tooltipText := "Remove Destination"
      , On #clicked (dispatch (RemoveDestination index))
      ]
  flatNamed button ("Remove " <> pathText (takeFileName folder))
  Gtk.toWidget button

renderOffloadDialog :: OffloadDialog -> Model -> IO ()
renderOffloadDialog widgets current = do
  renderCell widgets.draftCell current.draft
  renderCell widgets.openCell (isJust current.draft)
