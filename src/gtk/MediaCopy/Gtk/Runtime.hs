{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( start
  , buildView
  ) where

import Data.GI.Base (AttrOp (On, (:=)), new)
import Effectful.Time (currentTime)
import Floreal.Commons.GLib.MainLoop (postIdle, postMain)
import Floreal.Commons.Gtk.Signal (everyOnBehalf, onSelf)
import Floreal.Commons.Signals (onStopSignal)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

import MediaCopy.Gtk.Environment (Environment (..), runIn, withEnvironment)
import MediaCopy.Gtk.Interpret (gtkShell)
import MediaCopy.Gtk.Reload (loadCss, loadFonts, loadWording)
import MediaCopy.Gtk.Screenshot (Startup (..), seeded)
import MediaCopy.Gtk.Theme
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Embedded
import MediaCopy.Model
import MediaCopy.Resources (bundle)
import MediaCopy.Runtime (Runtime, currentModel, dispatch, newRuntime, perform, replaceModel, stopWorkers)

data Loop = Loop
  { widgets :: Widgets
  , runtime :: Runtime
  }

start :: Maybe Startup -> IO ()
start startup = withEnvironment $ \environment -> do
  GLib.bytesNew (Just bundle) >>= Gio.resourceNewFromData >>= Gio.resourcesRegister
  loopRef <- newIORef Nothing
  app <-
    new
      Adw.Application
      [ #applicationId := "tech.floreal.MediaCopy3000"
      , On #activate (activate loopRef environment startup ?self)
      ]
  onStopSignal (runIn environment (postMain (liftIO (Gio.applicationQuit app))))
  status <- Gio.applicationRun app Nothing `finally` (readIORef loopRef >>= mapM_ (\loop -> stopWorkers loop.runtime) >> writeIORef loopRef Nothing)
  when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))

activate :: IORef (Maybe Loop) -> Environment -> Maybe Startup -> Adw.Application -> IO ()
activate loopRef environment startup app =
  readIORef loopRef >>= \case
    Just loop -> Gtk.windowPresent loop.widgets.window
    Nothing -> buildAndPresent loopRef environment startup app

buildView :: Environment -> Adw.Application -> (UiMessage -> IO ()) -> (UiMessage -> IO ()) -> IO (ThemeAdapter, Widgets)
buildView environment app dispatchUi postUi = do
  themeAdapter <- newThemeAdapter environment
  widgets <- buildWidgets app (apply themeAdapter) (embeddedWording English) dispatchUi postUi
  loadFonts environment widgets.window
  loadCss environment
  pure (themeAdapter, widgets)

buildAndPresent
  :: IORef (Maybe Loop)
  -> Environment
  -> Maybe Startup
  -> Adw.Application
  -> IO ()
buildAndPresent loopRef environment startup app = do
  startedAt <- runIn environment currentTime
  let dispatchNow msg = readIORef loopRef >>= mapM_ (\loop -> dispatch loop.runtime msg)
      post msg = postIdle GLib.PRIORITY_DEFAULT_IDLE (dispatchNow msg)
  (themeAdapter, widgets) <- buildView environment app (dispatchNow . Ui) (post . Ui)
  desktop <- readDesktopBase themeAdapter
  desktopAccent <- readDesktopAccent themeAdapter
  runtime <- newRuntime (gtkShell widgets post) environment.runner (initialModel startedAt desktop desktopAccent)
  writeIORef loopRef (Just Loop {widgets, runtime})
  model <- currentModel runtime
  loadWording environment model.wording.language (post . WordingReloaded) (post . ShowToast)
  onDesktopBase themeAdapter (post . DesktopBase)
  onDesktopAccent themeAdapter (post . DesktopAccent)
  installCloseRequest widgets.window dispatchNow
  installTicker environment startup widgets.window dispatchNow
  widgets.render model
  Gtk.windowPresent widgets.window
  when (isNothing startup) (perform runtime LoadCatalogue)
  forM_ startup (seeded environment widgets.window (\command -> command & RunCommand & Ui & dispatchNow) (replaceModel runtime))

installCloseRequest :: Adw.ApplicationWindow -> (Message -> IO ()) -> IO ()
installCloseRequest window dispatchNow =
  void $ onSelf window #closeRequest $ do
    dispatchNow (Ui RequestClose)
    pure True

installTicker :: Environment -> Maybe Startup -> Adw.ApplicationWindow -> (Message -> IO ()) -> IO ()
installTicker environment startup window dispatchNow =
  when (isNothing startup) $
    everyOnBehalf window 1_000 $ \_ ->
      runIn environment currentTime >>= dispatchNow . Tick
