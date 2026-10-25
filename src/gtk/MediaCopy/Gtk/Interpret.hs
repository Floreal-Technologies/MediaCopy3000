module MediaCopy.Gtk.Interpret
  ( gtkShell
  ) where

import Control.Exception (displayException)
import Data.GI.Base (AttrOp ((:=)), new, set)
import Data.GI.Base.GError (catchGErrorJustDomain)
import Data.Text qualified as T
import Floreal.Commons.Exception (guarded)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.OsPath (encodeFS)

import MediaCopy.Gtk.View (Widgets (..))
import MediaCopy.Model
import MediaCopy.Runtime (Shell (..))

-- | The window side of the runtime. The second argument hands a message to the loop from any
-- thread.
gtkShell :: Widgets -> (Message -> IO ()) -> Shell
gtkShell widgets post =
  Shell
    { render = widgets.render
    , post
    , openDialog = openDialog widgets.window post
    , destroyWindow = Gtk.windowDestroy widgets.window
    , showChrome = widgets.present
    , showFolder = showFolder widgets.window post
    , notify = notify widgets.window
    }

toasting :: (Message -> IO ()) -> IO () -> IO ()
toasting post action = guarded action >>= either (toastFailure post) pure

toastFailure :: (Message -> IO ()) -> SomeException -> IO ()
toastFailure post err = post (ShowToast (T.pack (displayException err)))

openDialog :: Adw.ApplicationWindow -> (Message -> IO ()) -> DialogKind -> (OsPath -> Message) -> IO ()
openDialog window post kind toMessage = do
  dialog <- new Gtk.FileDialog []
  let picked finish = toasting post (sendPicked post toMessage finish)
  case kind of
    PickFolder -> do
      set dialog [#title := "Choose a Folder"]
      Gtk.fileDialogSelectFolder dialog (Just window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogSelectFolderFinish dialog result)
    PickFile -> do
      set dialog [#title := "Choose a File"]
      Gtk.fileDialogOpen dialog (Just window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogOpenFinish dialog result)
    SaveAs title suggested -> do
      set dialog [#title := title, #initialName := suggested]
      Gtk.fileDialogSave dialog (Just window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
        picked (Gtk.fileDialogSaveFinish dialog result)

showFolder :: Adw.ApplicationWindow -> (Message -> IO ()) -> Text -> IO ()
showFolder window post folder = do
  file <- Gio.fileNewForPath (T.unpack folder)
  launcher <- Gtk.fileLauncherNew (Just file)
  Gtk.fileLauncherLaunch launcher (Just window) (Nothing @Gio.Cancellable) $ Just $ \_source result ->
    toasting post (Gtk.fileLauncherLaunchFinish launcher result)

notify :: Adw.ApplicationWindow -> Notification -> IO ()
notify window notification = do
  isActive <- Gtk.windowIsActive window
  case isActive of
    True -> pure ()
    False -> do
      mbApp <- Gtk.windowGetApplication window
      case mbApp of
        Nothing -> pure ()
        Just app -> do
          notif <- Gio.notificationNew notification.title
          Gio.applicationSendNotification app Nothing notif
          
-- | gi-gio versions disagree on whether the picked file is nullable.
class PickedFile file where
   pickedFile :: file -> Maybe Gio.File

instance PickedFile Gio.File where
  pickedFile = Just

instance PickedFile (Maybe Gio.File) where
  pickedFile = id

sendPicked :: (PickedFile file) => (Message -> IO ()) -> (OsPath -> Message) -> IO file -> IO ()
sendPicked post toMessage finish =
  catchGErrorJustDomain
    (finish >>= \picked -> forM_ (pickedFile picked) (sendPath post toMessage))
    (reportDialogError post)

reportDialogError :: (Message -> IO ()) -> Gtk.DialogError -> Text -> IO ()
reportDialogError post dialogError message = case dialogError of
  (Gtk.DialogErrorCancelled; Gtk.DialogErrorDismissed) -> pure ()
  _ -> post (ShowToast message)

sendPath :: (Message -> IO ()) -> (OsPath -> Message) -> Gio.File -> IO ()
sendPath post toMessage file =
  Gio.fileGetPath file >>= \case
    Nothing -> post (ShowToast "Chosen location has no filesystem path")
    Just raw -> encodeFS raw >>= \path -> post (toMessage path)
