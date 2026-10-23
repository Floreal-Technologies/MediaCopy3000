{-# LANGUAGE ImplicitParams #-}

module MediaCopy.Gtk.Widgets.Lists
  ( KeyedList (..)
  , newKeyedList
  , renderKeyed
  , keyedSelection
  , choiceRow
  ) where

import Data.GI.Base (AttrOp ((:=)), SignalProxy (PropertySet), new, set)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Floreal.Commons.Gtk.Bind (Control (..), connectSelf)
import Floreal.Commons.Gtk.ListStore (ListStore, itemValue, listModel, newListStore, recycleItems, replaceItems, spliceItems)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

data KeyedList k c v r = KeyedList
  { store :: ListStore k
  , factory :: Gtk.SignalListItemFactory
  , shown :: IORef (Vector k)
  , bound :: IORef (Map k (Gtk.Widget, r))
  , snapshot :: IORef (Maybe (c, Map k v))
  , update :: c -> k -> v -> r -> IO ()
  }

newKeyedList
  :: (Ord k, Typeable k, Typeable r, Gtk.IsWidget w)
  => IO (w, r)
  -> (c -> k -> v -> r -> IO ())
  -> IO (KeyedList k c v r)
newKeyedList build update = do
  store <- newListStore
  factory <- new Gtk.SignalListItemFactory []
  shown <- newIORef V.empty
  bound <- newIORef Map.empty
  snapshot <- newIORef Nothing
  recycleItems factory create $ \(widget, row) key ->
    readIORef snapshot >>= \case
      Just (context, values) | Just value <- Map.lookup key values -> do
        update context key value row
        modifyIORef' bound (Map.insert key (widget, row))
        pure (modifyIORef' bound (Map.update (\entry -> if fst entry == widget then Nothing else Just entry) key))
      _ -> pure (pure ())
  pure KeyedList {store, factory, shown, bound, snapshot, update}
  where
    create = do
      (child, row) <- build
      widget <- Gtk.toWidget child
      pure (widget, (widget, row))

renderKeyed :: (Ord k, Typeable k) => KeyedList k c v r -> c -> Map k v -> Vector k -> IO ()
renderKeyed list context values keys = do
  writeIORef list.snapshot (Just (context, values))
  syncKeys list keys
  bound <- readIORef list.bound
  forM_ (Map.toList (Map.intersectionWith (,) bound values)) $ \(key, ((_, row), value)) ->
    list.update context key value row

syncKeys :: (Eq k, Typeable k) => KeyedList k c v r -> Vector k -> IO ()
syncKeys list wanted = do
  old <- readIORef list.shown
  let common = V.length (V.takeWhile id (V.zipWith (==) old wanted))
      after keys = V.reverse (V.drop common keys)
      trailing = V.length (V.takeWhile id (V.zipWith (==) (after old) (after wanted)))
      removed = V.length old - common - trailing
      added = V.slice common (V.length wanted - common - trailing) wanted
  unless (removed == 0 && V.null added) $
    spliceItems list.store (fromIntegral common) (fromIntegral removed) (V.toList added)
  writeIORef list.shown wanted

keyedSelection :: (Eq k, Typeable k) => Gtk.SingleSelection -> KeyedList k c v r -> Control (Maybe k)
keyedSelection selection list =
  Control
    { paint = \case
        Nothing -> Gtk.singleSelectionSetSelected selection Gtk.INVALID_LIST_POSITION
        Just key -> readIORef list.shown >>= traverse_ (Gtk.singleSelectionSetSelected selection . fromIntegral) . V.elemIndex key
    , connect = \report -> connectSelf selection (PropertySet #selected) (\_ -> selectedKey ?self >>= report . const)
    , current = selectedKey selection
    }
  where
    selectedKey from = Gtk.singleSelectionGetSelectedItem from >>= fmap join . traverse itemValue

choiceRow :: (Eq a, Typeable a) => (a -> Text) -> NonEmpty a -> Adw.ComboRow -> IO (Control a)
choiceRow name values row = do
  store <- newListStore
  replaceItems store (NE.toList values)
  factory <- new Gtk.SignalListItemFactory []
  recycleItems factory newLabel $ \label value -> do
    set label [#label := name value]
    pure (pure ())
  model <- listModel store
  set row [#factory := factory, #model := model]
  pure
    Control
      { paint = \value -> traverse_ (Adw.comboRowSetSelected row . fromIntegral) (V.elemIndex value options)
      , connect = \report -> connectSelf row (PropertySet #selected) (\_ -> selectedValue ?self >>= report . const)
      , current = selectedValue row
      }
  where
    options = V.fromList (NE.toList values)
    newLabel = new Gtk.Label [#xalign := 0] <&> \label -> (label, label)
    selectedValue from = Adw.comboRowGetSelectedItem from >>= fmap join . traverse itemValue <&> fromMaybe (NE.head values)
