module MediaCopy.Gtk.Widgets.CloseConfirm
  ( newCloseConfirm
  ) where

import Data.GI.Base (AttrOp (On, (:=)), new)
import GI.Adw qualified as Adw

import MediaCopy.Gtk.Widgets.Bind (bind, dialog)
import MediaCopy.Model (UiMessage (..))

newCloseConfirm
  :: Adw.ApplicationWindow
  -> (UiMessage -> IO ())
  -> IO (Bool -> IO ())
newCloseConfirm window dispatch = do
  alert <-
    new
      Adw.AlertDialog
      [ #heading := "Stop the running job?"
      , #body := "A job is still running. Closing the window stops it. The files already copied stay where they are."
      , On #response $ \answer ->
          if answer == "stop" then dispatch ConfirmClose else dispatch CancelClose
      ]
  Adw.alertDialogAddResponse alert "keep" "_Keep Running"
  Adw.alertDialogAddResponse alert "stop" "_Stop and Close"
  Adw.alertDialogSetResponseAppearance alert "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse alert (Just "keep")
  Adw.alertDialogSetCloseResponse alert "keep"
  asDialog <- Adw.toDialog alert
  openControl <- dialog asDialog window (pure ())
  bind openControl (\_ -> pure ())
