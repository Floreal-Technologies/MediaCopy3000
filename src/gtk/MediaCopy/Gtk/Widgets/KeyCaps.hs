module MediaCopy.Gtk.Widgets.KeyCaps
  ( keyNames
  , newKeyCaps
  ) where

import Data.GI.Base (AttrOp ((:=)), new)
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

modifierOrder :: List Gdk.ModifierType
modifierOrder = [Gdk.ModifierTypeControlMask, Gdk.ModifierTypeShiftMask, Gdk.ModifierTypeAltMask, Gdk.ModifierTypeSuperMask, Gdk.ModifierTypeHyperMask, Gdk.ModifierTypeMetaMask]

keyNames :: Text -> IO (List Text)
keyNames accel = do
  (parsed, key, modifiers) <- Gtk.acceleratorParse accel
  if not parsed || key == 0
    then pure []
    else do
      modifierNames <- traverse (\modifier -> Gtk.acceleratorGetLabel 0 [modifier]) (filter (\modifier -> modifier `elem` modifiers) modifierOrder)
      keyName <- Gtk.acceleratorGetLabel key []
      pure (modifierNames <> [keyName])

newKeyCaps :: Text -> IO Gtk.Box
newKeyCaps accel = do
  box <- new Gtk.Box [#spacing := 6, #valign := Gtk.AlignCenter]
  names <- keyNames accel
  forM_ (zip [0 :: Int ..] names) $ \(index, name) -> do
    when (index > 0) (new Gtk.Label [#label := "+", #cssClasses := ["dim-label"]] >>= Gtk.boxAppend box)
    new Gtk.Label [#label := name, #cssClasses := ["keycap"]] >>= Gtk.boxAppend box
  pure box
