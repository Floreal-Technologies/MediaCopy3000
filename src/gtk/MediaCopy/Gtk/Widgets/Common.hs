module MediaCopy.Gtk.Widgets.Common
  ( Cell
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

import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp ((:=)), new)
import Data.GI.Base.Attributes (AttrOpTag (AttrConstruct))
import Data.GI.Base.GValue (toGValue)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (List)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, MonadIO, liftIO, (:>))
import Effectful.Exception (bracket_)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import MediaCopy.Gtk.Eff (onE)
import MediaCopy.Gtk.Environment (Ui)

toggleClass
  :: (MonadIO m, Gtk.IsWidget w)
  => w
  -> Text
  -> Bool
  -> m ()
toggleClass widget className wanted =
  if wanted
    then Gtk.widgetAddCssClass widget className
    else Gtk.widgetRemoveCssClass widget className

nameAccessible :: (MonadIO m, Gtk.IsAccessible w) => w -> Text -> m ()
nameAccessible widget name = do
  value <- liftIO (toGValue (Just name))
  Gtk.accessibleUpdateProperty widget [Gtk.AccessiblePropertyLabel] [value]

flatNamed :: (MonadIO m, Gtk.IsWidget w, Gtk.IsAccessible w) => w -> Text -> m ()
flatNamed widget name = do
  nameAccessible widget name
  Gtk.widgetAddCssClass widget "flat"

data Cell es a
  = Cell
      (IORef (Maybe a))
      (a -> Eff es ())

newCell :: (IOE :> es) => (a -> Eff es ()) -> Eff es (Cell es a)
newCell paint = do
  ref <- liftIO (newIORef Nothing)
  pure (Cell ref paint)

newOpenCell :: (IOE :> es) => Adw.Dialog -> Adw.ApplicationWindow -> Eff es (Cell es Bool)
newOpenCell dialog window = do
  ref <- liftIO (newIORef (Just False))
  pure $ Cell ref $ \open ->
    if open
      then Adw.dialogPresent dialog (Just window)
      else void (Adw.dialogClose dialog)

renderCell
  :: (IOE :> es, Eq a)
  => Cell es a
  -> a
  -> Eff es ()
renderCell (Cell ref paint) wanted = do
  painted <- liftIO (readIORef ref)
  when (painted /= Just wanted) $ do
    liftIO (writeIORef ref (Just wanted))
    paint wanted

cellValue :: (IOE :> es) => Cell es a -> Eff es (Maybe a)
cellValue (Cell ref _) = liftIO (readIORef ref)

suppressing :: (IOE :> es) => IORef Bool -> Eff es a -> Eff es a
suppressing flag act = bracket_ (liftIO (writeIORef flag True)) (liftIO (writeIORef flag False)) act

unlessSuppressed :: (IOE :> es) => IORef Bool -> Eff es () -> Eff es ()
unlessSuppressed suppress act = do
  quiet <- liftIO (readIORef suppress)
  unless quiet act

paddedBox :: (MonadIO m) => Gtk.Orientation -> Int32 -> Int32 -> m Gtk.Box
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

newLabel :: (MonadIO m) => Text -> List (AttrOp Gtk.Label 'AttrConstruct) -> List Text -> m Gtk.Label
newLabel text attrs classes = do
  label <- new Gtk.Label ((#label := text) : attrs)
  mapM_ (\klass -> Gtk.widgetAddCssClass label klass) classes
  pure label

data DialogShell = DialogShell
  { dialog :: Adw.Dialog
  , primaryButton :: Gtk.Button
  }

data ShellButtons es = ShellButtons
  { leading :: ShellButton es
  , primary :: ShellButton es
  }

data ShellButton es = ShellButton
  { label :: Text
  , clicked :: Eff es ()
  }

newDialogShell :: (Ui es, Gtk.IsWidget body) => List (AttrOp Adw.Dialog 'AttrConstruct) -> ShellButtons es -> List Gtk.Button -> body -> Eff es DialogShell
newDialogShell attrs buttons endButtons body = do
  dialog <- new Adw.Dialog attrs
  toolbar <- new Adw.ToolbarView []
  header <- new Adw.HeaderBar [#showEndTitleButtons := False]
  leadingButton <-
    new
      Gtk.Button
      [ #label := buttons.leading.label
      , #useUnderline := True
      ]
  _ <- onE leadingButton #clicked buttons.leading.clicked
  primaryButton <-
    new
      Gtk.Button
      [ #label := buttons.primary.label
      , #useUnderline := True
      , #sensitive := False
      ]
  _ <- onE primaryButton #clicked buttons.primary.clicked
  Gtk.widgetAddCssClass primaryButton "suggested-action"
  Adw.headerBarPackStart header leadingButton
  Adw.headerBarPackEnd header primaryButton
  forM_ endButtons (\b -> Adw.headerBarPackEnd header b)
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewSetContent toolbar (Just body)
  Adw.dialogSetChild dialog (Just toolbar)
  Adw.dialogSetDefaultWidget dialog (Just primaryButton)
  pure DialogShell {dialog, primaryButton}

data Row es = Row
  { title :: Text
  , subtitle :: Text
  , cssClass :: Maybe Text
  , suffix :: Maybe (Eff es Gtk.Widget)
  }

plainRow :: Text -> Text -> Row es
plainRow title subtitle = Row {title, subtitle, cssClass = Nothing, suffix = Nothing}

data RowHost = InGroup Adw.PreferencesGroup | InExpander Adw.ExpanderRow

renderActionRows :: (IOE :> es) => IORef (Vector Adw.ActionRow) -> RowHost -> Vector (Row es) -> Eff es ()
renderActionRows rowsRef host wanted = do
  existing <- liftIO (readIORef rowsRef)
  V.mapM_ (\row -> removeFrom host row) existing
  fresh <- V.mapM (\row -> newRow row) wanted
  V.mapM_ (\row -> addTo host row) fresh
  liftIO (writeIORef rowsRef fresh)

removeFrom :: (MonadIO m) => RowHost -> Adw.ActionRow -> m ()
removeFrom host row = case host of
  InGroup group -> Adw.preferencesGroupRemove group row
  InExpander expander -> Adw.expanderRowRemove expander row

addTo :: (MonadIO m) => RowHost -> Adw.ActionRow -> m ()
addTo host row = case host of
  InGroup group -> Adw.preferencesGroupAdd group row
  InExpander expander -> Adw.expanderRowAddRow expander row

newRow :: (IOE :> es) => Row es -> Eff es Adw.ActionRow
newRow row = do
  built <- new Adw.ActionRow [#title := row.title, #subtitle := row.subtitle]
  mapM_ (\name -> Gtk.widgetAddCssClass built name) row.cssClass
  mapM_ (\build -> build >>= \widget -> Adw.actionRowAddSuffix built widget) row.suffix
  pure built

onDialogClosed :: (Ui es) => Adw.Dialog -> Cell es Bool -> Eff es () -> Eff es ()
onDialogClosed dialog openCell report =
  void $ onE dialog #closed $ do
    wasOpen <- cellValue openCell
    when (wasOpen == Just True) report
