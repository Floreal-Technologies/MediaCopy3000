{-# LANGUAGE TemplateHaskell #-}

module MediaCopy.Gtk.Resources (registerResources) where

import Data.ByteString (ByteString)
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio

import MediaCopy.Gtk.Resources.Splice (compiledResources)

registerResources :: IO ()
registerResources = GLib.bytesNew (Just bundle) >>= Gio.resourceNewFromData >>= Gio.resourcesRegister

bundle :: ByteString
bundle = $(compiledResources "assets/tech.floreal.MediaCopy3000.gresource.xml")
