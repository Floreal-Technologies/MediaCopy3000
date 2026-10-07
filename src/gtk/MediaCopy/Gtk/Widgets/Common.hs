module MediaCopy.Gtk.Widgets.Common
  ( Cell
  , newCell
  , newOpenCell
  , newOpenCellWith
  , renderCell
  , toggleClass
  , nameAccessible
  , flatNamed
  , suppressing
  , unlessSuppressed
  , paddedBox
  , newLabel
  , paintEditable
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
import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import Data.GI.Base.Attributes (AttrOpTag (AttrConstruct))
import Data.GI.Base.GValue (toGValue)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
  -> Text
  -> Bool
  -> IO ()
toggleClass widget className wanted =
  if wanted
    then Gtk.widgetAddCssClass widget className
    else Gtk.widgetRemoveCssClass widget className

paintEditable :: (Gtk.IsEditable w) => w -> Text -> IO ()
paintEditable editable text = do
  shown <- Gtk.editableGetText editable
  when (shown /= text) (Gtk.editableSetText editable text)

nameAccessible :: (Gtk.IsAccessible w) => w -> Text -> IO ()
nameAccessible widget name = do
  value <- toGValue (Just name)
  Gtk.accessibleUpdateProperty widget [Gtk.AccessiblePropertyLabel] [value]

flatNamed :: (Gtk.IsWidget w, Gtk.IsAccessible w) => w -> Text -> IO ()
flatNamed widget name = do
  nameAccessible widget name
  Gtk.widgetAddCssClass widget "flat"

data Cell a
  = Cell
      (IORef (Maybe a))
      (a -> IO ())

newCell :: (a -> IO ()) -> IO (Cell a)
newCell paint = do
  ref <- newIORef Nothing
  pure (Cell ref paint)

newOpenCell :: Adw.Dialog -> Adw.ApplicationWindow -> IO (Cell Bool)
newOpenCell = newOpenCellWith (pure ())

newOpenCellWith :: IO () -> Adw.Dialog -> Adw.ApplicationWindow -> IO (Cell Bool)
newOpenCellWith presented dialog window = do
  ref <- newIORef (Just False)
  pure $ Cell ref $ \open ->
    if open
      then Adw.dialogPresent dialog (Just window) >> presented
      else void (Adw.dialogClose dialog)

renderCell
  :: (Eq a)
  => Cell a
  -> a
  -> IO ()
renderCell (Cell ref paint) wanted = do
  painted <- readIORef ref
  when (painted /= Just wanted) $ do
    writeIORef ref (Just wanted)
    paint wanted

cellValue :: Cell a -> IO (Maybe a)
cellValue (Cell ref _) = readIORef ref

suppressing :: IORef Int -> IO a -> IO a
suppressing flag act = bracket_ (modifyIORef' flag (\val -> val + 1)) (modifyIORef' flag (\val -> val - 1)) act

unlessSuppressed :: IORef Int -> IO () -> IO ()
unlessSuppressed suppress act = do
  quiet <- readIORef suppress
  unless (quiet == 0) act

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

newLabel :: Text -> List (AttrOp Gtk.Label 'AttrConstruct) -> List Text -> IO Gtk.Label
newLabel text attrs classes = do
  label <- new Gtk.Label ((#label := text) : attrs)
  forM_ classes (Gtk.widgetAddCssClass label)
  pure label

data DialogShell = DialogShell
  { dialog :: Adw.Dialog
  , primaryButton :: Gtk.Button
  }

data ShellButtons = ShellButtons
  { leading :: ShellButton
  , primary :: ShellButton
  }

data ShellButton = ShellButton
  { label :: Text
  , clicked :: IO ()
  }

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
  forM_ endButtons (Adw.headerBarPackEnd header)
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewSetContent toolbar (Just body)
  Adw.dialogSetChild dialog (Just toolbar)
  Adw.dialogSetDefaultWidget dialog (Just primaryButton)
  pure DialogShell {dialog, primaryButton}

data Row = Row
  { title :: Text
  , subtitle :: Text
  , cssClass :: Maybe Text
  , suffix :: Maybe (IO Gtk.Widget)
  }

plainRow :: Text -> Text -> Row
plainRow title subtitle = Row {title, subtitle, cssClass = Nothing, suffix = Nothing}

data RowHost = InGroup Adw.PreferencesGroup | InExpander Adw.ExpanderRow

renderActionRows :: IORef (Vector Adw.ActionRow) -> RowHost -> Vector Row -> IO ()
renderActionRows rowsRef host wanted = do
  existing <- readIORef rowsRef
  V.mapM_ (removeFrom host) existing
  fresh <- V.mapM newRow wanted
  V.mapM_ (addTo host) fresh
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
  built <- new Adw.ActionRow [#useMarkup := False]
  set built [#title := row.title, #subtitle := row.subtitle]
  forM_ row.cssClass (Gtk.widgetAddCssClass built)
  forM_ row.suffix (\build -> build >>= \widget -> Adw.actionRowAddSuffix built widget)
  pure built

onDialogClosed :: Adw.Dialog -> Cell Bool -> IO () -> IO ()
onDialogClosed dialog openCell report =
  void $ on dialog #closed $ do
    wasOpen <- cellValue openCell
    when (wasOpen == Just True) report
