{-# LANGUAGE AllowAmbiguousTypes #-}

module MediaCopy.Plugin.Schema
  ( document
  , render
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty', keyOrder)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.List (List, nub, (\\))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.Capability qualified as Plugin (Capability)
import MediaCopy.Plugin.Field qualified as Plugin (Field)
import MediaCopy.Plugin.Id qualified as Plugin (Id)
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.JsonSchema qualified as Plugin (JsonSchema)
import MediaCopy.Plugin.Manifest qualified as Plugin (Manifest)
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.FileInfo qualified as Protocol (FileInfo)
import MediaCopy.Plugin.Protocol.Finding qualified as Finding
import MediaCopy.Plugin.Protocol.Finding qualified as Protocol (Finding)
import MediaCopy.Plugin.Protocol.HashValue qualified as Protocol (HashValue)
import MediaCopy.Plugin.Protocol.Initialize qualified as Initialize
import MediaCopy.Plugin.Protocol.InspectFile qualified as InspectFile
import MediaCopy.Plugin.Protocol.InspectPlan qualified as InspectPlan
import MediaCopy.Plugin.Protocol.JobInfo qualified as Protocol (JobInfo)
import MediaCopy.Plugin.Protocol.LogLine qualified as Protocol (LogLine)
import MediaCopy.Plugin.Protocol.Method qualified as Method
import MediaCopy.Plugin.Protocol.Progress qualified as Protocol (Progress)

-- | The schema of @plugin.json@ and of every message of the protocol, as @schema/protocol-1.schema.json@ holds it.
document :: Value
document
  | not (null repeated) = error ("the schema defines these $defs more than once: " <> T.unpack (T.intercalate ", " repeated))
  | not (null missing) = error ("the schema refers to $defs that do not exist: " <> T.unpack (T.intercalate ", " missing))
  | otherwise = assembled
  where
    names = map fst defs
    repeated = nub (names \\ nub names)
    missing = dangling names assembled
    assembled =
      object
        [ "$schema" .= String "https://json-schema.org/draft/2020-12/schema"
        , "$id" .= String "https://floreal.tech/schemas/mediacopy3000/plugin-protocol-1.schema.json"
        , "title" .= ("MediaCopy 3000 plug-in protocol, API " <> T.show Api.major <> "." <> T.show Api.minor)
        , "description" .= String "JSON-RPC 2.0 protocol for MC3K and plugins"
        , "x-methods"
            .= object
              [ method Method.initialize (JsonSchema.embed @Initialize.Params) (JsonSchema.embed @Initialize.Result)
              , method Method.inspectPlan (JsonSchema.embed @InspectPlan.Params) (JsonSchema.embed @InspectPlan.Result)
              , method Method.contribute (JsonSchema.embed @Contribute.Params) (JsonSchema.embed @Contribute.Result)
              , method Method.inspectFile (JsonSchema.embed @InspectFile.Params) (JsonSchema.embed @InspectFile.Result)
              , method Method.shutdown (object ["type" .= String "object"]) (object [])
              ]
        , "x-notifications" .= object [Key.fromText Method.progress .= JsonSchema.embed @Protocol.Progress, Key.fromText Method.log .= JsonSchema.embed @Protocol.LogLine]
        , "$ref" .= String "#/$defs/manifest"
        , "$defs" .= object [Key.fromText name .= value | (name, value) <- defs]
        ]
    method name params result = Key.fromText name .= object ["params" .= params, "result" .= result]
    defs =
      concat
        [ def @Plugin.Id
        , def @Plugin.Capability
        , def @Finding.Severity
        , def @Plugin.Field
        , def @Plugin.Manifest
        , def @Protocol.JobInfo
        , def @Protocol.FileInfo
        , def @Protocol.Finding
        , def @Protocol.HashValue
        , def @InspectFile.Annotation
        , def @Contribute.Author
        , def @Contribute.FileMetadata
        , def @Initialize.Params
        , def @Initialize.Result
        , def @InspectPlan.Params
        , def @InspectPlan.Result
        , def @Contribute.Params
        , def @Contribute.Result
        , def @InspectFile.Params
        , def @InspectFile.Result
        , def @Protocol.Progress
        , def @Protocol.LogLine
        , def @Plugin.AuthorSlot
        ,
          [ ("jobKind", object ["enum" .= (["offload", "verify", "seal"] :: [Text])])
          ,
            ( "hashAlgo"
            , object ["enum" .= (["xxh64", "md5", "sha1", "c4"] :: [Text])]
                & JsonSchema.describe "The name of a hash format, as the element name in an ASC MHL manifest."
            )
          ]
        ]

def :: forall a. (Plugin.JsonSchema a) => List (Text, Value)
def = [(name, JsonSchema.schema @a) | Just name <- [JsonSchema.defName @a]]

-- | The names under @$defs@ that the document refers to but does not define.
--
-- >>> dangling ["a"] (object ["x" .= object ["$ref" .= String "#/$defs/a"], "y" .= [object ["$ref" .= String "#/$defs/b"]]])
-- ["b"]
dangling :: [Text] -> Value -> [Text]
dangling defined = \case
  Object o -> [name | Just (String target) <- [KeyMap.lookup "$ref" o], Just name <- [T.stripPrefix JsonSchema.defsPrefix target], name `notElem` defined] <> concatMap (dangling defined) (KeyMap.elems o)
  Array items -> concatMap (dangling defined) (V.toList items)
  _ -> []

render :: ByteString
render =
  encodePretty' defConfig {confIndent = Spaces 2, confCompare = keyOrder keywords <> compare, confTrailingNewline = True} document

keywords :: [Text]
keywords =
  [ "$schema"
  , "$id"
  , "title"
  , "description"
  , "$ref"
  , "x-methods"
  , "x-notifications"
  , "$defs"
  , "params"
  , "result"
  , "type"
  , "const"
  , "enum"
  , "pattern"
  , "minimum"
  , "maximum"
  , "required"
  , "properties"
  , "items"
  , "additionalProperties"
  , "contains"
  , "minContains"
  , "maxContains"
  , "not"
  , "allOf"
  , "if"
  , "then"
  , "default"
  ]
