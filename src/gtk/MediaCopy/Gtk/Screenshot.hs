module MediaCopy.Gtk.Screenshot
  ( Startup (..)
  , seeded
  ) where

import Control.Exception (SomeException)
import Control.Monad.Extra
import Data.GI.Base (AttrOp ((:=)), castTo, set)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, liftIO)
import Effectful.Exception (catchSync)
import Effectful.Log (logAttention_)
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gsk qualified as Gsk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Eff (timeoutE)
import MediaCopy.Gtk.Environment (Ui)
import MediaCopy.Model (Model)

data Startup = Startup
  { frame :: Model
  , action :: Maybe Text
  , shot :: Maybe FilePath
  , expand :: Bool
  , scroll :: Bool
  }

seeded :: (Ui es) => Adw.ApplicationWindow -> Adw.Application -> (Model -> Eff es ()) -> (SomeException -> Eff es ()) -> Startup -> Eff es ()
seeded window app showFrame abort startup = do
  Gtk.widgetSetCanTarget window False
  Gtk.windowSetFocusVisible window False
  void $ stage 250 $ do
    showFrame startup.frame
    mapM_ (activateNamed window app) startup.action
    void (stage 600 prepare)
  where
    stage milliseconds body = timeoutE milliseconds ((body `catchSync` abort) >> pure False)
    prepare = do
      when startup.expand (liftIO (expandAll window))
      when startup.scroll (liftIO (scrollToEnd window))
      mapM_ (\path -> void (stage settleMs (takeShot path))) startup.shot
    settleMs = 3_500
    takeShot path = do
      outcome <- liftIO (saveWindowPng window path)
      either (\err -> logAttention_ err) pure outcome
      Gtk.windowDestroy window

activateNamed :: (Ui es) => Adw.ApplicationWindow -> Adw.Application -> Text -> Eff es ()
activateNamed window app full = case T.breakOn "." full of
  ("app", rest) -> Gio.actionGroupActivateAction app (T.drop 1 rest) Nothing
  ("win", rest) -> Gio.actionGroupActivateAction window (T.drop 1 rest) Nothing
  _ -> logAttention_ ("no such action: " <> full)

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
