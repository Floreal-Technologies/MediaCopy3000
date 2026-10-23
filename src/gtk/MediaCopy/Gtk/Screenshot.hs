module MediaCopy.Gtk.Screenshot
  ( Startup (..)
  , seeded
  ) where

import Control.Monad.Extra
import Data.GI.Base (AttrOp ((:=)), castTo, set)
import Data.Text qualified as T
import Effectful.Exception (onException)
import Effectful.Log (logAttention_)
import Floreal.Commons.GLib.MainLoop (postAfter)
import GI.Adw qualified as Adw
import GI.Gdk qualified as Gdk
import GI.Gsk qualified as Gsk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Environment (Environment, runIn)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Scene (Frame (..))
import MediaCopy.Model (Model)

data Startup = Startup
  { scene :: Frame
  , shot :: Maybe FilePath
  }

seeded :: Environment -> Adw.ApplicationWindow -> (Command.Command -> IO ()) -> (Model -> IO ()) -> Startup -> IO ()
seeded environment window activate showFrame startup = do
  Gtk.widgetSetCanTarget window False
  Gtk.windowSetFocusVisible window False
  runIn environment $ postAfter 250 $ abandoning $ do
    liftIO $ do
      showFrame startup.scene.model
      forM_ startup.scene.action activate
    postAfter 600 (abandoning prepare)
  where
    abandoning action
      | isJust startup.shot = action `onException` liftIO (Gtk.windowDestroy window)
      | otherwise = action
    prepare = do
      liftIO $ do
        when startup.scene.expand (expandAll window)
        when startup.scene.scroll (scrollToEnd window)
      forM_ startup.shot (\path -> path & takeShot & abandoning & postAfter settleMs)
    settleMs = 3_500
    takeShot path = do
      outcome <- liftIO (saveWindowPng window path)
      either logAttention_ pure outcome
      liftIO (Gtk.windowDestroy window)

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
