module MediaCopy.Gtk.Widgets.CommandPalette
  ( newCommandPalette
  ) where

import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (List)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Bind (bind, closing, dialog, searchText)
import MediaCopy.Gtk.Widgets.Common hiding (newRow)
import MediaCopy.Interface.Command (commandId)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Palette (Match (..), PaletteRow (..), Target (..))
import MediaCopy.Interface.View.Palette (PaletteItem (..), PaletteView (..))
import MediaCopy.Model (UiMessage (..))

data Parts = Parts
  { entry :: Gtk.SearchEntry
  , list :: Gtk.ListBox
  , placeholder :: Gtk.Label
  , scroll :: Gtk.ScrolledWindow
  , shown :: IORef (Vector (Adw.ActionRow, Command.Command))
  }

newCommandPalette
  :: Adw.ApplicationWindow
  -> (Command.Command -> Maybe Text)
  -> (UiMessage -> IO ())
  -> IO (PaletteView -> IO ())
newCommandPalette window accelLabel dispatch = do
  shown <- newIORef V.empty
  entry <- new Gtk.SearchEntry [#hexpand := True]
  placeholder <- newLabel "" [#marginTop := 24, #marginBottom := 24] ["dim-label"]
  list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
  Gtk.widgetAddCssClass list "boxed-list"
  Gtk.listBoxSetPlaceholder list (Just placeholder)
  scroll <-
    new
      Gtk.ScrolledWindow
      [ #child := list
      , #hscrollbarPolicy := Gtk.PolicyTypeNever
      , #propagateNaturalHeight := True
      , #maxContentHeight := 420
      ]
  box <- paddedBox Gtk.OrientationVertical 12 12
  Gtk.boxAppend box entry
  Gtk.boxAppend box scroll
  dialog' <- new Adw.Dialog [#contentWidth := 520, #child := box]
  let parts = Parts {entry, list, placeholder, scroll, shown}
  void $ on entry #activate (runSelected parts dispatch)
  void $ on entry #stopSearch (void (Adw.dialogClose dialog'))
  void $ on list #rowSelected (\row -> forM_ row (revealRow parts))
  void $ on list #rowActivated (runRow parts dispatch)
  keys <- new Gtk.EventControllerKey []
  void $ on keys #keyPressed $ \keyval _ _ -> moveSelection parts keyval
  Gtk.widgetAddController entry keys
  openControl <- dialog dialog' window (void (Gtk.widgetGrabFocus entry))
  paintOpen <- bind openControl (closing (dispatch ClosePalette))
  paintQuery <- bind (searchText entry) (dispatch . SetPaletteQuery)
  itemsCell <- newCell (paintItems parts accelLabel)
  pure $ \view -> do
    renderCell itemsCell (view.items, view.placeholder, view.noMatch)
    paintQuery view.query
    paintOpen view.open

paintItems :: Parts -> (Command.Command -> Maybe Text) -> (List PaletteItem, Text, Text) -> IO ()
paintItems parts accelLabel (items, placeholder, noMatch) = do
  set parts.entry [#placeholderText := placeholder]
  set parts.placeholder [#label := noMatch]
  Gtk.listBoxRemoveAll parts.list
  built <- V.fromList <$> traverse (newRow accelLabel) items
  V.forM_ built (\(row, _) -> Gtk.listBoxAppend parts.list row)
  writeIORef parts.shown built
  V.forM_ (V.take 1 built) (\(row, _) -> Gtk.listBoxSelectRow parts.list (Just row))

newRow :: (Command.Command -> Maybe Text) -> PaletteItem -> IO (Adw.ActionRow, Command.Command)
newRow accelLabel item = do
  title <- case item.row.match.target of
    OnLabel -> emphasize item.row.match.hits item.label
    OnId -> GLib.markupEscapeText item.label (-1)
  built <-
    new
      Adw.ActionRow
      [ #useMarkup := True
      , #title := title
      , #subtitle := commandId item.row.command
      , #activatable := True
      , #sensitive := item.row.enabled
      ]
  forM_ (accelLabel item.row.command) $ \text -> do
    suffix <- newLabel text [] ["dim-label"]
    Adw.actionRowAddSuffix built suffix
  pure (built, item.row.command)

emphasize :: List Int -> Text -> IO Text
emphasize hits label = T.concat <$> traverse piece (zip [0 ..] (T.unpack label))
  where
    piece (index, c) = do
      escaped <- GLib.markupEscapeText (T.singleton c) (-1)
      pure (if index `elem` hits then "<b>" <> escaped <> "</b>" else escaped)

revealRow :: Parts -> Gtk.ListBoxRow -> IO ()
revealRow parts row = do
  index <- Gtk.listBoxRowGetIndex row
  rows <- readIORef parts.shown
  above <- V.mapM (\(built, _) -> Gtk.widgetGetHeight built) (V.take (fromIntegral index) rows)
  height <- Gtk.widgetGetHeight row
  let top = fromIntegral (V.sum above)
  vadj <- Gtk.scrolledWindowGetVadjustment parts.scroll
  Gtk.adjustmentClampPage vadj top (top + fromIntegral height)

runSelected :: Parts -> (UiMessage -> IO ()) -> IO ()
runSelected parts dispatch = do
  selected <- Gtk.listBoxGetSelectedRow parts.list
  forM_ selected (runRow parts dispatch)

runRow :: Parts -> (UiMessage -> IO ()) -> Gtk.ListBoxRow -> IO ()
runRow parts dispatch row = do
  index <- Gtk.listBoxRowGetIndex row
  rows <- readIORef parts.shown
  forM_ (rows V.!? fromIntegral index) $ \(_, command) -> dispatch (RunCommand command)

moveSelection :: Parts -> Word32 -> IO Bool
moveSelection parts keyval
  | keyval == Gdk.KEY_Down = step 1
  | keyval == Gdk.KEY_Up = step (-1)
  | otherwise = pure False
  where
    step :: Int32 -> IO Bool
    step offset = do
      current <- Gtk.listBoxGetSelectedRow parts.list
      index <- maybe (pure (-1)) Gtk.listBoxRowGetIndex current
      next <- Gtk.listBoxGetRowAtIndex parts.list (max 0 (index + offset))
      forM_ next (Gtk.listBoxSelectRow parts.list . Just)
      pure True
