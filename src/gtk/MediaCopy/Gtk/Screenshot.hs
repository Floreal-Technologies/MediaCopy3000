module MediaCopy.Gtk.Screenshot
  ( Startup (..)
  , seeded
  ) where

import Control.Monad.Extra
import Data.GI.Base (AttrOp ((:=)), castTo, set)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful.Log (logAttention_)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gsk qualified as Gsk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Environment (Environment, logWith)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Model (Model)

data Startup = Startup
  { frame :: Model
  , action :: Maybe Command.Command
  , shot :: Maybe FilePath
  , expand :: Bool
  , scroll :: Bool
  }

seeded :: Environment -> Adw.ApplicationWindow -> (Command.Command -> IO ()) -> (Model -> IO ()) -> Startup -> IO ()
seeded environment window activate showFrame startup = do
  Gtk.widgetSetCanTarget window False
  Gtk.windowSetFocusVisible window False
  void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 250 $ do
    showFrame startup.frame
    mapM_ activate startup.action
    void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT 600 prepare)
    pure False
  where
    prepare = do
      when startup.expand (expandAll window)
      when startup.scroll (scrollToEnd window)
      mapM_ (GLib.timeoutAdd GLib.PRIORITY_DEFAULT settleMs . takeShot) startup.shot
      pure False
    settleMs = 3_500
    takeShot path = do
      outcome <- saveWindowPng window path
      either (logWith environment . logAttention_) pure outcome
      Gtk.windowDestroy window
      pure False

saveWindowPng :: Adw.ApplicationWindow -> FilePath -> IO (Either Text ())
saveWindowPng window path = do
  widget <- Gtk.toWidget window
  width <- Gtk.widgetGetWidth widget
  height <- Gtk.widgetGetHeight widget
  paintable <- Gtk.widgetPaintableNew (Just widget)
  snapshot <- Gtk.snapshotNew
  Gdk.paintableSnapshot paintable snapshot (fromIntegral width) (fromIntegral height)
  node <- Gtk.snapshotToNode snapshot
  renderer <- Gtk.nativeGetRenderer window
  case (node, renderer) of
    (Nothing, _) -> pure (Left "the window drew nothing")
    (_, Nothing) -> pure (Left "the window has no renderer")
    (Just rendered, Just gpu) -> do
      texture <- Gsk.rendererRenderTexture gpu rendered Nothing
      saved <- Gdk.textureSaveToPng texture path
      pure (if saved then Right () else Left (T.pack ("the file could not be written: " <> path)))

expandAll :: (Gtk.IsWidget widget) => widget -> IO ()
expandAll widget = do
  asWidget <- Gtk.toWidget widget
  whenJustM (castTo Adw.ExpanderRow asWidget) $ \expander ->
    set expander [#expanded := True]
  children asWidget >>= mapM_ expandAll

scrollToEnd :: (Gtk.IsWidget widget) => widget -> IO ()
scrollToEnd widget = do
  asWidget <- Gtk.toWidget widget
  whenJustM (castTo Gtk.ScrolledWindow asWidget) $ \scrolled -> do
    adjustment <- Gtk.scrolledWindowGetVadjustment scrolled
    upper <- Gtk.adjustmentGetUpper adjustment
    page <- Gtk.adjustmentGetPageSize adjustment
    Gtk.adjustmentSetValue adjustment (upper - page)
  children asWidget >>= mapM_ scrollToEnd

children :: Gtk.Widget -> IO (List Gtk.Widget)
children parent = Gtk.widgetGetFirstChild parent >>= siblings
  where
    siblings = \case
      Nothing -> pure []
      Just child -> do
        rest <- Gtk.widgetGetNextSibling child >>= siblings
        pure (child : rest)
