{-# LANGUAGE AllowAmbiguousTypes #-}

module MediaCopy.Plugin.Schema
  ( protocolSchema
  , renderSchema
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.List (nub, (\\))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import MediaCopy.Plugin.JsonSchema
import MediaCopy.Plugin.Manifest
import MediaCopy.Plugin.Protocol

-- | The schema of @plugin.json@ and of every message of the protocol, as @schema/protocol-1.schema.json@ holds it.
protocolSchema :: Value
protocolSchema
  | not (null repeated) = error ("the schema defines these $defs more than once: " <> T.unpack (T.intercalate ", " repeated))
  | not (null missing) = error ("the schema refers to $defs that do not exist: " <> T.unpack (T.intercalate ", " missing))
  | otherwise = document
  where
    names = map fst defs
    repeated = nub (names \\ nub names)
    missing = dangling names document
    document =
      object
        [ "$schema" .= String "https://json-schema.org/draft/2020-12/schema"
        , "$id" .= String "https://floreal.tech/schemas/mediacopy3000/plugin-protocol-1.schema.json"
        , "title" .= ("MediaCopy 3000 plug-in protocol, API " <> T.show apiMajor <> "." <> T.show apiMinor)
        , "description" .= String "JSON-RPC 2.0 protocol for MC3K and plugins"
        , "x-methods"
            .= object
              [ method methodInitialize (embed @InitializeParams) (embed @InitializeResult)
              , method methodInspectPlan (embed @InspectPlanParams) (embed @InspectPlanResult)
              , method methodContribute (embed @ContributeParams) (embed @ContributeResult)
              , method methodInspectFile (embed @InspectFileParams) (embed @InspectFileResult)
              , method methodShutdown (object ["type" .= String "object"]) (object [])
              ]
        , "x-notifications" .= object [Key.fromText notifyProgress .= embed @Progress, Key.fromText notifyLog .= embed @LogLine]
        , "$ref" .= String "#/$defs/manifest"
        , "$defs" .= object [Key.fromText name .= value | (name, value) <- defs]
        ]
    method name params result = Key.fromText name .= object ["params" .= params, "result" .= result]
    defs =
      concat
        [ def @PluginId
        , def @Capability
        , def @Severity
        , def @Field
        , def @PluginManifest
        , def @JobInfo
        , def @FileInfo
        , def @Finding
        , def @HashValue
        , def @Annotation
        , def @Author
        , def @FileMetadata
        , def @InitializeParams
        , def @InitializeResult
        , def @InspectPlanParams
        , def @InspectPlanResult
        , def @ContributeParams
        , def @ContributeResult
        , def @InspectFileParams
        , def @InspectFileResult
        , def @Progress
        , def @LogLine
        , [("authorSlot", authorSlotSchema), ("jobKind", jobKindSchema), ("hashAlgo", hashAlgoSchema)]
        ]

def :: forall a. (JsonSchema a) => [(Text, Value)]
def = [(name, schema @a) | Just name <- [defName @a]]

-- | The names under @$defs@ that the document refers to but does not define.
--
-- >>> dangling ["a"] (object ["x" .= object ["$ref" .= String "#/$defs/a"], "y" .= [object ["$ref" .= String "#/$defs/b"]]])
-- ["b"]
dangling :: [Text] -> Value -> [Text]
dangling defined = \case
  Object o -> [name | Just (String target) <- [KeyMap.lookup "$ref" o], Just name <- [T.stripPrefix defsPrefix target], name `notElem` defined] <> concatMap (dangling defined) (KeyMap.elems o)
  Array items -> concatMap (dangling defined) (V.toList items)
  _ -> []

renderSchema :: ByteString
renderSchema = render protocolSchema
