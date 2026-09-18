-- | The alert that appears when a close of the window will stop a job that still runs.
module MediaCopy.Gtk.Widgets.CloseConfirm
  ( CloseConfirm (..)
  , newCloseConfirm
  , renderCloseConfirm
  ) where

import Data.GI.Base (AttrOp (On, (:=)), new)
import GI.Adw qualified as Adw

import MediaCopy.Gtk.Widgets.Common (Cell, newOpenCell, renderCell)
import MediaCopy.Model (Model (..), UiMessage (..))

newtype CloseConfirm = CloseConfirm {openCell :: Cell Bool}

newCloseConfirm
  :: Adw.ApplicationWindow
  -- ^  Window
  -> (UiMessage -> IO ())
  -- ^ Dispatcher
  -> IO CloseConfirm
newCloseConfirm window dispatch = do
  dialog <-
    new
      Adw.AlertDialog
      [ #heading := "Stop the running job?"
      , #body := "A job is still running. Closing the window stops it. The files already copied stay where they are."
      , -- A render that closes the alert also fires "keep", which only clears a flag already clear.
        On #response $ \answer ->
          if answer == "stop" then dispatch ConfirmClose else dispatch CancelClose
      ]
  Adw.alertDialogAddResponse dialog "keep" "_Keep Running"
  Adw.alertDialogAddResponse dialog "stop" "_Stop and Close"
  Adw.alertDialogSetResponseAppearance dialog "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse dialog (Just "keep")
  Adw.alertDialogSetCloseResponse dialog "keep"
  asDialog <- Adw.toDialog dialog
  openCell <- newOpenCell asDialog window
  pure CloseConfirm {openCell}

renderCloseConfirm :: CloseConfirm -> Model -> IO ()
renderCloseConfirm widget current = renderCell widget.openCell current.closeConfirm
