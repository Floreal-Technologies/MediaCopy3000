module MediaCopy.Gtk.Widgets.Bind where

import Control.Exception (bracket_)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Foldable (find)
import Data.GI.Base (on)
import Data.GI.Base.Signals (SignalHandlerId)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (List)
import Data.Text (Text)
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.GObject qualified as GObject
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common (paintEditable)

data Control a = Control
  { paint :: a -> IO ()
  , connect :: IO () -> IO (IO () -> IO ())
  , current :: IO a
  }

bind :: (Eq a) => Control a -> (a -> IO ()) -> IO (a -> IO ())
bind control react = fst <$> bindQuietly control react

bindQuietly :: (Eq a) => Control a -> (a -> IO ()) -> IO (a -> IO (), IO () -> IO ())
bindQuietly control react = do
  painted <- newIORef Nothing
  quietly <- control.connect $ do
    value <- control.current
    writeIORef painted (Just value)
    react value
  let paint wanted = do
        previous <- readIORef painted
        when (previous /= Just wanted) $ do
          quietly (control.paint wanted)
          writeIORef painted (Just wanted)
  pure (paint, quietly)

pair :: Control a -> Control b -> Control (a, b)
pair left right =
  Control
    { paint = \(a, b) -> left.paint a >> right.paint b
    , connect = \callback -> do
        quietLeft <- left.connect callback
        quietRight <- right.connect callback
        pure (quietLeft . quietRight)
    , current = (,) <$> left.current <*> right.current
    }

closing :: IO () -> Bool -> IO ()
closing close open = unless open close

blocking :: (GObject.IsObject o) => o -> SignalHandlerId -> IO () -> IO ()
blocking object handler =
  bracket_ (GObject.signalHandlerBlock object handler) (GObject.signalHandlerUnblock object handler)

switch :: Gtk.Switch -> Control Bool
switch widget =
  Control
    { paint = Gtk.switchSetActive widget
    , connect = \callback -> blocking widget <$> on widget (Gtk.PropertyNotify #active) (const callback)
    , current = Gtk.switchGetActive widget
    }

switchRow :: Adw.SwitchRow -> Control Bool
switchRow row =
  Control
    { paint = Adw.switchRowSetActive row
    , connect = \callback -> blocking row <$> on row (Adw.PropertyNotify #active) (const callback)
    , current = Adw.switchRowGetActive row
    }

comboRow :: Adw.ComboRow -> Control Word32
comboRow row =
  Control
    { paint = Adw.comboRowSetSelected row
    , connect = \callback -> blocking row <$> on row (Adw.PropertyNotify #selected) (const callback)
    , current = Adw.comboRowGetSelected row
    }

dropDown :: Gtk.DropDown -> Control Word32
dropDown widget =
  Control
    { paint = Gtk.dropDownSetSelected widget
    , connect = \callback -> blocking widget <$> on widget (Gtk.PropertyNotify #selected) (const callback)
    , current = Gtk.dropDownGetSelected widget
    }

toggleRadio :: (Eq a) => List (a, Gtk.ToggleButton) -> Control (Maybe a)
toggleRadio =
  radio Gtk.toggleButtonGetActive Gtk.toggleButtonSetActive (\button callback -> on button #toggled callback)

checkRadio :: (Eq a) => List (a, Gtk.CheckButton) -> Control (Maybe a)
checkRadio =
  radio Gtk.checkButtonGetActive Gtk.checkButtonSetActive (\button callback -> on button #toggled callback)

radio
  :: (Eq a, GObject.IsObject b)
  => (b -> IO Bool)
  -> (b -> Bool -> IO ())
  -> (b -> IO () -> IO SignalHandlerId)
  -> List (a, b)
  -> Control (Maybe a)
radio getActive setActive onToggled buttons =
  Control
    { paint = \case
        Nothing -> forM_ buttons (\(_, button) -> setActive button False)
        Just wanted -> forM_ buttons (\(value, button) -> when (value == wanted) (setActive button True))
    , connect = \callback -> do
        handlers <- forM buttons $ \(_, button) -> do
          handler <- onToggled button (getActive button >>= \active -> when active callback)
          pure (blocking button handler)
        pure (\act -> foldr (\quiet inner -> quiet inner) act handlers)
    , current = do
        states <- forM buttons (\(value, button) -> (\active -> (value, active)) <$> getActive button)
        pure (fst <$> find snd states)
    }

listSelection
  :: Gtk.ListBox
  -> (Gtk.ListBoxRow -> IO (Maybe a))
  -> (a -> IO (Maybe Gtk.ListBoxRow))
  -> Control (Maybe a)
listSelection list keyOf rowOf =
  Control
    { paint = \case
        Nothing -> Gtk.listBoxUnselectAll list
        Just key -> rowOf key >>= mapM_ (Gtk.listBoxSelectRow list . Just)
    , connect = \callback -> blocking list <$> on list #rowSelected (const callback)
    , current = Gtk.listBoxGetSelectedRow list >>= maybe (pure Nothing) keyOf
    }

searchText :: Gtk.SearchEntry -> Control Text
searchText entry =
  Control
    { paint = paintEditable entry
    , connect = \callback -> do
        handler <- on entry #changed callback
        pure (blocking entry handler)
    , current = Gtk.editableGetText entry
    }

entryApply :: Adw.EntryRow -> Control Text
entryApply row =
  Control
    { paint = paintEditable row
    , connect = \callback -> do
        handler <- on row #apply callback
        pure (blocking row handler)
    , current = Gtk.editableGetText row
    }

dialog :: Adw.Dialog -> Adw.ApplicationWindow -> IO () -> IO (Control Bool)
dialog shown window presented = do
  wanted <- newIORef False
  pure
    Control
      { paint = \open -> do
          wasOpen <- readIORef wanted
          writeIORef wanted open
          if open
            then Adw.dialogPresent shown (Just window) >> presented
            else when wasOpen (void (Adw.dialogClose shown))
      , connect = \callback -> do
          handler <- on shown #closed $ do
            wasOpen <- readIORef wanted
            writeIORef wanted False
            when wasOpen callback
          pure (blocking shown handler)
      , current = readIORef wanted
      }
