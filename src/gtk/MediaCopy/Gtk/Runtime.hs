{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( start
  , buildView
  ) where

import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Effectful.Time (currentTime)
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
  onStopSignal (void (GLib.idleAdd GLib.PRIORITY_DEFAULT (Gio.applicationQuit app >> pure False)))
  status <- Gio.applicationRun app Nothing `finally` (readIORef loopRef >>= mapM_ (\loop -> stopWorkers loop.runtime))
  when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))

activate :: IORef (Maybe Loop) -> Environment -> Maybe Startup -> Adw.Application -> IO ()
activate loopRef environment startup app =
  readIORef loopRef >>= \case
    Just loop -> Gtk.windowPresent loop.widgets.window
    Nothing -> buildAndPresent loopRef environment startup app

buildView :: Environment -> Adw.Application -> (UiMessage -> IO ()) -> IO (ThemeAdapter, Widgets)
buildView environment app dispatchUi = do
  themeAdapter <- newThemeAdapter environment
  widgets <- buildWidgets app (apply themeAdapter) (embeddedWording English) dispatchUi
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
      post msg = void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatchNow msg >> pure False))
  (themeAdapter, widgets) <- buildView environment app (dispatchNow . Ui)
  desktop <- readDesktopBase themeAdapter
  desktopAccent <- readDesktopAccent themeAdapter
  runtime <- newRuntime (gtkShell widgets post) environment.runner (initialModel startedAt desktop desktopAccent)
  writeIORef loopRef (Just Loop {widgets, runtime})
  model <- currentModel runtime
  loadWording environment model.wording.language (post . WordingReloaded) (post . ShowToast)
  onDesktopBase themeAdapter (post . DesktopBase)
  onDesktopAccent themeAdapter (post . DesktopAccent)
  installCloseRequest widgets.window dispatchNow
  installTicker environment startup dispatchNow
  widgets.render model
  Gtk.windowPresent widgets.window
  when (isNothing startup) (perform runtime LoadCatalog)
  forM_ startup (seeded environment widgets.window (\command -> command & RunCommand & Ui & dispatchNow) (replaceModel runtime))

installCloseRequest :: Adw.ApplicationWindow -> (Message -> IO ()) -> IO ()
installCloseRequest window dispatchNow =
  void $ on window #closeRequest $ do
    dispatchNow (Ui RequestClose)
    pure True

installTicker :: Environment -> Maybe Startup -> (Message -> IO ()) -> IO ()
installTicker environment startup dispatchNow =
  when (isNothing startup) $
    void
      ( GLib.timeoutAdd
          GLib.PRIORITY_DEFAULT
          1_000
          ( runIn environment currentTime >>= \t ->
              dispatchNow (Tick t) >> pure True
          )
      )
