{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Runtime
  ( Interpreter (..)
  , Interpret
  , start
  , buildView
  ) where

import Control.Exception (finally)
import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

import MediaCopy.Gtk.Environment (Environment, withEnvironment)
import MediaCopy.Gtk.Reload (loadCss, loadWording)
import MediaCopy.Gtk.Resources (registerResources)
import MediaCopy.Gtk.Screenshot (Startup (..), seeded)
import MediaCopy.Gtk.Theme
import MediaCopy.Gtk.View (Widgets (..), buildWidgets)
import MediaCopy.Interface.Theme (PaletteMode (..), themeSections)
import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Embedded
import MediaCopy.Model
import MediaCopy.Signals (onStopSignal)

data Interpreter = Interpreter
  { run :: Model -> AppEffect -> IO ()
  , stop :: IO ()
  }

type Interpret = Widgets -> (Message -> IO ()) -> IO Interpreter

data Loop = Loop
  { modelRef :: IORef Model
  , widgets :: Widgets
  , interpreter :: Interpreter
  , busy :: IORef Bool
  , pending :: IORef (Vector Message)
  }

start :: Interpret -> Maybe Startup -> IO ()
start interpret startup = withEnvironment $ \environment -> do
  registerResources
  loopRef <- newIORef Nothing
  app <-
    new
      Adw.Application
      [ #applicationId := "tech.floreal.MediaCopy3000"
      , On #activate (activate loopRef environment interpret startup ?self)
      ]
  onStopSignal (void (GLib.idleAdd GLib.PRIORITY_DEFAULT (Gio.applicationQuit app >> pure False)))
  status <- Gio.applicationRun app Nothing `finally` (readIORef loopRef >>= mapM_ (\loop -> loop.interpreter.stop))
  when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))

activate :: IORef (Maybe Loop) -> Environment -> Interpret -> Maybe Startup -> Adw.Application -> IO ()
activate loopRef environment interpret startup app =
  readIORef loopRef >>= \case
    Just loop -> Gtk.windowPresent loop.widgets.window
    Nothing -> buildAndPresent loopRef environment interpret startup app

buildView :: Environment -> Adw.Application -> (UiMessage -> IO ()) -> IO (ThemeAdapter, Widgets)
buildView environment app dispatchUi = do
  themeAdapter <- newThemeAdapter environment
  palettes <- loadPalettes environment
  let wording = embeddedWording English
  widgets <-
    buildWidgets
      app
      (apply themeAdapter)
      wording
      (themeSections wording LightPalette palettes)
      (themeSections wording DarkPalette palettes)
      dispatchUi
  loadCss environment
  pure (themeAdapter, widgets)

buildAndPresent
  :: IORef (Maybe Loop)
  -> Environment
  -> Interpret
  -> Maybe Startup
  -> Adw.Application
  -> IO ()
buildAndPresent loopRef environment interpret startup app = do
  startedAt <- getCurrentTime
  let dispatchNow msg = readIORef loopRef >>= mapM_ (\loop -> dispatch loop msg)
      post msg = void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (dispatchNow msg >> pure False))
  (themeAdapter, widgets) <- buildView environment app (dispatchNow . Ui)
  desktop <- readDesktopBase themeAdapter
  modelRef <- newIORef (initialModel startedAt desktop)
  interpreter <- interpret widgets post
  busy <- newIORef False
  pending <- newIORef V.empty
  writeIORef loopRef (Just Loop {modelRef, widgets, interpreter, busy, pending})
  model <- readIORef modelRef
  loadWording environment model.wording.language (post . WordingReloaded) (post . ShowToast)
  onDesktopBase themeAdapter (post . DesktopBase)
  installCloseRequest widgets.window dispatchNow
  installTicker startup dispatchNow
  widgets.render model
  Gtk.windowPresent widgets.window
  when (isNothing startup) (interpreter.run model LoadCatalog)
  let showFrame frame = do
        writeIORef modelRef frame
        widgets.render frame
  forM_ startup (seeded environment widgets.window (dispatchNow . Ui . RunCommand) showFrame)

dispatch :: Loop -> Message -> IO ()
dispatch loop msg = do
  modifyIORef' loop.pending (`V.snoc` msg)
  running <- readIORef loop.busy
  unless running $ do
    writeIORef loop.busy True
    drain loop `finally` (writeIORef loop.busy False >> writeIORef loop.pending V.empty)

drain :: Loop -> IO ()
drain loop =
  readIORef loop.pending >>= \queued -> case V.uncons queued of
    Nothing -> pure ()
    Just (next, rest) -> do
      writeIORef loop.pending rest
      step loop next
      drain loop

step :: Loop -> Message -> IO ()
step loop msg = do
  old <- readIORef loop.modelRef
  let (current, cmds) = update msg old
  writeIORef loop.modelRef current
  loop.widgets.render current
  forM_ cmds (loop.interpreter.run current)

installCloseRequest :: Adw.ApplicationWindow -> (Message -> IO ()) -> IO ()
installCloseRequest window dispatchNow =
  void $ on window #closeRequest $ do
    dispatchNow (Ui RequestClose)
    pure True

installTicker :: Maybe Startup -> (Message -> IO ()) -> IO ()
installTicker startup dispatchNow =
  when (isNothing startup) $
    void
      ( GLib.timeoutAdd
          GLib.PRIORITY_DEFAULT
          1_000
          ( getCurrentTime >>= \t ->
              dispatchNow (Tick t) >> pure True
          )
      )
