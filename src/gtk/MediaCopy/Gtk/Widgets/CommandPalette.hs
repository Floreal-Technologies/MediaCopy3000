module MediaCopy.Gtk.Widgets.CommandPalette
  ( CommandPalette
  , newCommandPalette
  , renderCommandPalette
  ) where

import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (List)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Widgets.Common
import MediaCopy.Interface.Command (commandId, commandLabel, paletteNoMatch, palettePlaceholder)
import MediaCopy.Interface.Command qualified as Command
import MediaCopy.Interface.Palette (Match (..), PaletteRow (..), Target (..), paletteRows)
import MediaCopy.Interface.Translation (Wording)
import MediaCopy.Model (Model (..), UiMessage (..), commandEnabled)

data CommandPalette = CommandPalette
  { openCell :: Cell Bool
  , queryCell :: Cell Text
  , rowsCell :: Cell (Wording, List PaletteRow)
  }

data Parts = Parts
  { entry :: Gtk.SearchEntry
  , list :: Gtk.ListBox
  , placeholder :: Gtk.Label
  , scroll :: Gtk.ScrolledWindow
  , shown :: IORef (Vector (Adw.ActionRow, Command.Command))
  , suppress :: IORef Bool
  }

newCommandPalette
  :: Adw.ApplicationWindow
  -> (Command.Command -> Maybe Text)
  -> (UiMessage -> IO ())
  -> IO CommandPalette
newCommandPalette window accelLabel dispatch = do
  suppress <- newIORef False
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
  dialog <- new Adw.Dialog [#contentWidth := 520, #child := box]
  let parts = Parts {entry, list, placeholder, scroll, shown, suppress}
  void $ on entry #changed $ unlessSuppressed suppress $ do
    text <- Gtk.editableGetText entry
    dispatch (SetPaletteQuery text)
  void $ on entry #activate (runSelected parts dispatch)
  void $ on entry #stopSearch (void (Adw.dialogClose dialog))
  void $ on list #rowSelected (\row -> forM_ row (revealRow parts))
  void $ on list #rowActivated (runRow parts dispatch)
  keys <- new Gtk.EventControllerKey []
  void $ on keys #keyPressed $ \keyval _ _ -> moveSelection parts keyval
  Gtk.widgetAddController entry keys
  openCell <- newOpenCellWith (void (Gtk.widgetGrabFocus entry)) dialog window
  onDialogClosed dialog openCell (dispatch ClosePalette)
  queryCell <- newCell (suppressing suppress . paintEditable entry)
  rowsCell <- newCell (paintRows parts accelLabel)
  pure CommandPalette {openCell, queryCell, rowsCell}

renderCommandPalette :: CommandPalette -> Model -> IO ()
renderCommandPalette palette model = do
  renderCell palette.rowsCell (model.wording, paletteRows (commandEnabled model) model.wording model.palette)
  renderCell palette.queryCell (fromMaybe "" model.palette)
  renderCell palette.openCell (isJust model.palette)

paintRows :: Parts -> (Command.Command -> Maybe Text) -> (Wording, List PaletteRow) -> IO ()
paintRows parts accelLabel (wording, rows) = do
  set parts.entry [#placeholderText := palettePlaceholder wording]
  set parts.placeholder [#label := paletteNoMatch wording]
  Gtk.listBoxRemoveAll parts.list
  built <- V.fromList <$> traverse (newRow wording accelLabel) rows
  V.forM_ built (\(row, _) -> Gtk.listBoxAppend parts.list row)
  writeIORef parts.shown built
  V.forM_ (V.take 1 built) (\(row, _) -> Gtk.listBoxSelectRow parts.list (Just row))

newRow :: Wording -> (Command.Command -> Maybe Text) -> PaletteRow -> IO (Adw.ActionRow, Command.Command)
newRow wording accelLabel row = do
  let label = commandLabel wording row.command
  title <- case row.match.target of
    OnLabel -> emphasize row.match.hits label
    OnId -> GLib.markupEscapeText label (-1)
  built <-
    new
      Adw.ActionRow
      [ #useMarkup := True
      , #title := title
      , #subtitle := commandId row.command
      , #activatable := True
      , #sensitive := row.enabled
      ]
  forM_ (accelLabel row.command) $ \text -> do
    suffix <- newLabel text [] ["dim-label"]
    Adw.actionRowAddSuffix built suffix
  pure (built, row.command)

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
