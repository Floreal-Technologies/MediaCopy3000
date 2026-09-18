-- | The seeded run behind the images in @manual/@.
module MediaCopy.Gtk.Screenshot
  ( Startup (..)
  , defaultStartup
  , seeded
  ) where

import Control.Monad.Extra
import Data.GI.Base (AttrOp ((:=)), castTo, set)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gsk qualified as Gsk
import GI.Gtk qualified as Gtk
import System.IO (hPutStrLn, stderr)

import MediaCopy.Model (Model)

data Startup = Startup
  { frames :: List Model
  -- ^ The run shows these models in order
  , action :: Maybe Text
  -- ^ A GAction to activate after the run shows the frames, for a screen the
  -- model does not hold.
  , shot :: Maybe FilePath
  -- ^ Where to save a picture of the window. The window closes after the save.
  , expand :: Bool
  -- ^ Open every expander before the run takes the picture.
  , scroll :: Bool
  -- ^ Send every scrolled area to its end before the run takes the picture.
  }

defaultStartup :: Startup
defaultStartup = Startup {frames = [], action = Nothing, shot = Nothing, expand = False, scroll = False}

seeded :: Adw.ApplicationWindow -> Adw.Application -> (Model -> IO ()) -> Startup -> IO ()
seeded window app showFrame startup = case startup.frames of
  [] -> pure ()
  first : rest -> do
    Gtk.widgetSetCanTarget window False
    Gtk.windowSetFocusVisible window False
    void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT 250 (frame first rest))
  where
    frame model rest = do
      showFrame model
      case rest of
        next : more -> void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT 600 (frame next more))
        [] -> do
          mapM_ (activateNamed window app) startup.action
          void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT 600 prepare)
      pure False
    prepare = do
      when startup.expand (expandAll window)
      when startup.scroll (scrollToEnd window)
      mapM_ (\path -> void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT settleMs (takeShot path))) startup.shot
      pure False
    -- An overlay scrollbar shows itself when its content is laid out and fades two seconds later,
    -- over one more. A shot before the fade ends catches a different frame of it every run.
    settleMs = 3_500
    takeShot path = do
      outcome <- saveWindowPng window path
      either (\err -> hPutStrLn stderr (T.unpack err)) pure outcome
      Gtk.windowDestroy window
      pure False

-- | @app.about@ gets to the application and @win.something@ gets to the window. No other name exists.
activateNamed :: Adw.ApplicationWindow -> Adw.Application -> Text -> IO ()
activateNamed window app full = case T.breakOn "." full of
  ("app", rest) -> Gio.actionGroupActivateAction app (T.drop 1 rest) Nothing
  ("win", rest) -> Gio.actionGroupActivateAction window (T.drop 1 rest) Nothing
  _ -> hPutStrLn stderr ("no such action: " <> T.unpack full)

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

-- | Opens every expander in the window. An expander holds its own state, not
-- the model's, so no other way gets a screenshot of an open one.
expandAll :: (Gtk.IsWidget widget) => widget -> IO ()
expandAll widget = do
  asWidget <- Gtk.toWidget widget
  whenJustM (castTo Adw.ExpanderRow asWidget) $ \expander ->
    set expander [#expanded := True]
  children asWidget >>= mapM_ expandAll

-- | Sends every scrolled area to its own end.
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
