module MediaCopy.Gtk.Widgets.CloseConfirm
  ( newCloseConfirm
  ) where

import Data.GI.Base (AttrOp ((:=)), new)
import Effectful (Eff)
import GI.Adw qualified as Adw

import MediaCopy.Gtk.Eff (onE)
import MediaCopy.Gtk.Environment (Ui)
import MediaCopy.Gtk.Widgets.Common (Cell, newOpenCell)
import MediaCopy.Model (UiMessage (..))

newCloseConfirm
  :: (Ui es)
  => Adw.ApplicationWindow
  -> (UiMessage -> Eff es ())
  -> Eff es (Cell es Bool)
newCloseConfirm window dispatch = do
  dialog <-
    new
      Adw.AlertDialog
      [ #heading := "Stop the running job?"
      , #body := "A job is still running. Closing the window stops it. The files already copied stay where they are."
      ]
  _ <- onE dialog #response $ \answer ->
    if answer == "stop" then dispatch ConfirmClose else dispatch CancelClose
  Adw.alertDialogAddResponse dialog "keep" "_Keep Running"
  Adw.alertDialogAddResponse dialog "stop" "_Stop and Close"
  Adw.alertDialogSetResponseAppearance dialog "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse dialog (Just "keep")
  Adw.alertDialogSetCloseResponse dialog "keep"
  asDialog <- Adw.toDialog dialog
  newOpenCell asDialog window
