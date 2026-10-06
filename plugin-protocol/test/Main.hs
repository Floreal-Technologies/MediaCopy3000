module Main (main) where

import Test.Tasty

import MediaCopy.Plugin.JsonRpcTest qualified as JsonRpcTest
import MediaCopy.Plugin.ManifestTest qualified as ManifestTest
import MediaCopy.Plugin.ProtocolTest qualified as ProtocolTest
import MediaCopy.Plugin.SchemaTest qualified as SchemaTest

main :: IO ()
main = defaultMain (testGroup "plugin-protocol" [JsonRpcTest.tests, ManifestTest.tests, ProtocolTest.tests, SchemaTest.tests])
