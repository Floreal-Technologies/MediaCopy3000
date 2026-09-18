module MediaCopy.Gtk.Widgets.Common
  ( -- ** Cell
    Cell
  , newCell
  , newOpenCell
  , renderCell
  , toggleClass
  , nameAccessible
  , flatNamed
  , suppressing
  , unlessSuppressed
  , paddedBox
  , newLabel
  , DialogShell (..)
  , ShellButtons (..)
  , ShellButton (..)
  , newDialogShell
  , Row (..)
  , plainRow
  , RowHost (..)
  , renderActionRows
  , onDialogClosed
  ) where

import Control.Exception (bracket_)
import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp (On, (:=)), new, on)
import Data.GI.Base.Attributes (AttrOpTag (AttrConstruct))
import Data.GI.Base.GValue (toGValue)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (List)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

toggleClass
  :: (Gtk.IsWidget w)
  => w
  -- ^  Widget
  -> Text
  -- ^ Class name
  -> Bool
  -- ^ On/Off
  -> IO ()
toggleClass widget className wanted =
  if wanted
    then Gtk.widgetAddCssClass widget className
    else Gtk.widgetRemoveCssClass widget className

-- | Add a property label to the widget for screen readers.
nameAccessible :: (Gtk.IsAccessible w) => w -> Text -> IO ()
nameAccessible widget name = do
  value <- toGValue (Just name)
  Gtk.accessibleUpdateProperty widget [Gtk.AccessiblePropertyLabel] [value]

-- | A flat control draws no frame, so the name is all a screen reader has of it.
flatNamed :: (Gtk.IsWidget w, Gtk.IsAccessible w) => w -> Text -> IO ()
flatNamed widget name = do
  nameAccessible widget name
  Gtk.widgetAddCssClass widget "flat"

-- | The value a widget last showed, and how to paint it. Only for a paint that is not a no-op on an
-- equal value: a toast shown, rows rebuilt, a dialog presented. A plain setter needs no cell. The
-- paint runs when the wanted value differs from the painted one, and the cell records the value
-- before it paints, so a paint that dispatches cannot drive itself again.
data Cell a
  = Cell
      (IORef (Maybe a))
      -- ^ Last value
      (a -> IO ())
      -- ^ How to paint it

newCell :: (a -> IO ()) -> IO (Cell a)
newCell paint = do
  ref <- newIORef Nothing
  pure (Cell ref paint)

-- | A dialog starts closed, so the cell is painted 'False' and the first render
-- leaves it alone.
newOpenCell :: Adw.Dialog -> Adw.ApplicationWindow -> IO (Cell Bool)
newOpenCell dialog window = do
  ref <- newIORef (Just False)
  pure $ Cell ref $ \open ->
    if open
      then Adw.dialogPresent dialog (Just window)
      else void (Adw.dialogClose dialog)

-- | Trigger the painting callback of a cell. If the `wanted` value differs from the one
-- the cell holds, the cell takes the new value and the callback runs on it.
renderCell
  :: (Eq a)
  => Cell a
  -- ^ The cell
  -> a
  -- ^ The wanted value
  -> IO ()
renderCell (Cell ref paint) wanted = do
  painted <- readIORef ref
  when (painted /= Just wanted) $ do
    writeIORef ref (Just wanted)
    paint wanted

-- | 'Nothing' before the first paint.
cellValue :: Cell a -> IO (Maybe a)
cellValue (Cell ref _) = readIORef ref

-- | The flag is up for the whole of the action, even if it throws. A widget that a
-- render writes never reports itself as an operator's change.
suppressing :: IORef Bool -> IO a -> IO a
suppressing flag act = bracket_ (writeIORef flag True) (writeIORef flag False) act

-- | The mirror of 'suppressing' — a render's own write reaches no dispatch.
unlessSuppressed :: IORef Bool -> IO () -> IO ()
unlessSuppressed suppress act = do
  quiet <- readIORef suppress
  unless quiet act

-- | A box with the same margin on all four sides, which is how this application pads a panel.
paddedBox :: Gtk.Orientation -> Int32 -> Int32 -> IO Gtk.Box
paddedBox orientation spacing margin =
  new
    Gtk.Box
    [ #orientation := orientation
    , #spacing := spacing
    , #marginTop := margin
    , #marginBottom := margin
    , #marginStart := margin
    , #marginEnd := margin
    ]

-- | A label with its text, its construction attributes and its CSS classes in one place.
newLabel :: Text -> List (AttrOp Gtk.Label 'AttrConstruct) -> List Text -> IO Gtk.Label
newLabel text attrs classes = do
  label <- new Gtk.Label ((#label := text) : attrs)
  mapM_ (\klass -> Gtk.widgetAddCssClass label klass) classes
  pure label

-- | The pieces of a task dialog that its caller writes to after the shell is built.
data DialogShell = DialogShell
  { dialog :: Adw.Dialog
  , primaryButton :: Gtk.Button
  }

-- | 'leading' packs at the start of the header bar, 'primary' at its end. The caller's
-- own end buttons follow the primary, to its left.
data ShellButtons = ShellButtons
  { leading :: ShellButton
  , primary :: ShellButton
  }

data ShellButton = ShellButton
  { label :: Text
  , clicked :: IO ()
  }

-- | The primary button starts insensitive, so Enter does nothing until a render says the
-- dialog is complete. The caller carries the dialog's title and size in the attribute list.
newDialogShell :: (Gtk.IsWidget body) => List (AttrOp Adw.Dialog 'AttrConstruct) -> ShellButtons -> List Gtk.Button -> body -> IO DialogShell
newDialogShell attrs buttons endButtons body = do
  dialog <- new Adw.Dialog attrs
  toolbar <- new Adw.ToolbarView []
  header <- new Adw.HeaderBar [#showEndTitleButtons := False]
  leadingButton <-
    new
      Gtk.Button
      [ #label := buttons.leading.label
      , #useUnderline := True
      , On #clicked buttons.leading.clicked
      ]
  primaryButton <-
    new
      Gtk.Button
      [ #label := buttons.primary.label
      , #useUnderline := True
      , #sensitive := False
      , On #clicked buttons.primary.clicked
      ]
  Gtk.widgetAddCssClass primaryButton "suggested-action"
  Adw.headerBarPackStart header leadingButton
  Adw.headerBarPackEnd header primaryButton
  forM_ endButtons (\b -> Adw.headerBarPackEnd header b)
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewSetContent toolbar (Just body)
  Adw.dialogSetChild dialog (Just toolbar)
  -- A disabled default does not fire, so Enter does nothing until a render makes the primary sensitive.
  Adw.dialogSetDefaultWidget dialog (Just primaryButton)
  pure DialogShell {dialog, primaryButton}

-- | What a row says, apart from its place. Every list of rows in this application uses
-- this shape and comes from 'renderActionRows', so the lists cannot drift apart in style.
data Row = Row
  { title :: Text
  , subtitle :: Text
  , cssClass :: Maybe Text
  , suffix :: Maybe (IO Gtk.Widget)
  }

-- | A row with no styling and nothing on its right-hand side.
plainRow :: Text -> Text -> Row
plainRow title subtitle = Row {title, subtitle, cssClass = Nothing, suffix = Nothing}

-- | Where a list of rows lives. A preferences group and an expander row take rows by different names.
data RowHost = InGroup Adw.PreferencesGroup | InExpander Adw.ExpanderRow

-- | This function throws the rows away and builds them again, so the widgets cannot
-- disagree with the model that named them.
renderActionRows :: IORef (Vector Adw.ActionRow) -> RowHost -> Vector Row -> IO ()
renderActionRows rowsRef host wanted = do
  existing <- readIORef rowsRef
  V.mapM_ (\row -> removeFrom host row) existing
  fresh <- V.mapM (\row -> newRow row) wanted
  V.mapM_ (\row -> addTo host row) fresh
  writeIORef rowsRef fresh

removeFrom :: RowHost -> Adw.ActionRow -> IO ()
removeFrom host row = case host of
  InGroup group -> Adw.preferencesGroupRemove group row
  InExpander expander -> Adw.expanderRowRemove expander row

addTo :: RowHost -> Adw.ActionRow -> IO ()
addTo host row = case host of
  InGroup group -> Adw.preferencesGroupAdd group row
  InExpander expander -> Adw.expanderRowAddRow expander row

newRow :: Row -> IO Adw.ActionRow
newRow row = do
  built <- new Adw.ActionRow [#title := row.title, #subtitle := row.subtitle]
  mapM_ (\name -> Gtk.widgetAddCssClass built name) row.cssClass
  mapM_ (\build -> build >>= \widget -> Adw.actionRowAddSuffix built widget) row.suffix
  pure built

-- | The cell records False before a render closes the dialog itself. The handler then
-- sees a closed dialog and reports only the operator's close.
onDialogClosed :: Adw.Dialog -> Cell Bool -> IO () -> IO ()
onDialogClosed dialog openCell report =
  void $ on dialog #closed $ do
    wasOpen <- cellValue openCell
    when (wasOpen == Just True) report
