{-# LANGUAGE AllowAmbiguousTypes #-}

module MediaCopy.Plugin.Schema
  ( document
  , render
  ) where

import Autodocodec (HasCodec)
import Autodocodec.Schema (jsonSchemaViaCodec)
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty', keyOrder)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.List (List, nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.AuthorSlot qualified as Plugin (AuthorSlot)
import MediaCopy.Plugin.JsonSchema qualified as JsonSchema
import MediaCopy.Plugin.Manifest qualified as Plugin (Manifest)
import MediaCopy.Plugin.Protocol.Contribute qualified as Contribute
import MediaCopy.Plugin.Protocol.Initialise qualified as Initialise
import MediaCopy.Plugin.Protocol.InspectFile qualified as InspectFile
import MediaCopy.Plugin.Protocol.InspectPlan qualified as InspectPlan
import MediaCopy.Plugin.Protocol.JobFinished qualified as JobFinished
import MediaCopy.Plugin.Protocol.LogLine qualified as Protocol (LogLine)
import MediaCopy.Plugin.Protocol.Method qualified as Method
import MediaCopy.Plugin.Protocol.Progress qualified as Protocol (Progress)
import MediaCopy.Plugin.Schema.Patch (patches)

-- | The schema of @plugin.json@ and of every message of the protocol, as @schema/protocol-1.schema.json@ holds it.
document :: Value
document
  | not (null conflicting) = error ("the codecs give two schemas to these $defs: " <> T.unpack (T.intercalate ", " conflicting))
  | not (null unknown) = error ("these patches name $defs that do not exist: " <> T.unpack (T.intercalate ", " unknown))
  | not (null missing) = error ("the schema refers to $defs that do not exist: " <> T.unpack (T.intercalate ", " missing))
  | otherwise = assembled
  where
    roots =
      [ root @Plugin.Manifest
      , root @Initialise.Params
      , root @Initialise.Result
      , root @InspectPlan.Params
      , root @InspectPlan.Result
      , root @Contribute.Params
      , root @Contribute.Result
      , root @InspectFile.Params
      , root @InspectFile.Result
      , root @JobFinished.Params
      , root @JobFinished.Result
      , root @Protocol.Progress
      , root @Protocol.LogLine
      , root @Plugin.AuthorSlot
      ]
    collected = Map.fromListWith (<>) [(name, [value]) | (_, used) <- roots, (name, value) <- used] & Map.map nub
    conflicting = Map.keys (Map.filter (\values -> length values > 1) collected)
    unknown = map fst patches & filter (`Map.notMember` collected)
    defs = Map.fromList [(name, maybe id ($) (lookup name patches) value) | (name, value : _) <- Map.toList collected]
    missing = dangling (Map.keys defs) assembled
    refTo :: forall a. (HasCodec a) => Value
    refTo = JsonSchema.ref (fst (root @a))
    assembled =
      tidy $
        object
          [ "$schema" .= String "https://json-schema.org/draft/2020-12/schema"
          , "$id" .= String "https://floreal.tech/schemas/mediacopy3000/plugin-protocol-1.schema.json"
          , "title" .= ("MediaCopy 3000 plug-in protocol, API " <> T.show Api.major <> "." <> T.show Api.minor)
          , "description" .= String "JSON-RPC 2.0 protocol for MC3K and plugins"
          , "x-methods"
              .= object
                [ method Method.initialise (refTo @Initialise.Params) (refTo @Initialise.Result)
                , method Method.inspectPlan (refTo @InspectPlan.Params) (refTo @InspectPlan.Result)
                , method Method.contribute (refTo @Contribute.Params) (refTo @Contribute.Result)
                , method Method.inspectFile (refTo @InspectFile.Params) (refTo @InspectFile.Result)
                , method Method.jobFinished (refTo @JobFinished.Params) (refTo @JobFinished.Result)
                , method Method.shutdown (object ["type" .= String "object"]) (object [])
                ]
          , "x-notifications" .= object [Key.fromText Method.progress .= refTo @Protocol.Progress, Key.fromText Method.log .= refTo @Protocol.LogLine]
          , "$ref" .= (JsonSchema.defsPrefix <> fst (root @Plugin.Manifest))
          , "$defs" .= object [Key.fromText name .= value | (name, value) <- Map.toList defs]
          ]
    method name params result = Key.fromText name .= object ["params" .= params, "result" .= result]

-- | The name of the root schema in @$defs@, and every schema in @$defs@ that the codec of @a@ uses.
root :: forall a. (HasCodec a) => (Text, List (Text, Value))
root = case hoist (toJSON (jsonSchemaViaCodec @a)) of
  (Object o, defs)
    | Just (String target) <- KeyMap.lookup "$ref" o
    , Just name <- T.stripPrefix JsonSchema.defsPrefix target ->
        (name, defs)
  (other, _) -> error ("the codec has no name in $defs: " <> show other)

-- | Takes out every @$defs@. autodocodec-schema writes one next to the first use of each named schema.
hoist :: Value -> (Value, List (Text, Value))
hoist = \case
  Object o ->
    let (kept, found) = unzip [((key, value'), defs) | (key, value) <- KeyMap.toList o, key /= "$defs", let (value', defs) = hoist value]
        lifted = [(Key.toText name, schema') : defs | Just (Object here) <- [KeyMap.lookup "$defs" o], (name, schema) <- KeyMap.toList here, let (schema', defs) = hoist schema]
    in (Object (KeyMap.fromList kept), concat found <> concat lifted)
  Array items ->
    let (items', found) = unzip (map hoist (V.toList items))
    in (Array (V.fromList items'), concat found)
  other -> (other, [])

-- | Rewrites two habits of autodocodec-schema:
--
-- * It writes documentation as @$comment@. Editors show @description@.
-- * An object with a constant part gets @allOf@ with an empty object schema.
--
-- >>> tidy (object ["$comment" .= String "doc", "allOf" .= [object ["type" .= String "object"], object ["required" .= ["status" :: Text]]]])
-- Object (fromList [("description",String "doc"),("required",Array [String "status"])])
tidy :: Value -> Value
tidy = \case
  Object o -> Object (collapse (KeyMap.fromList [(if key == "$comment" then "description" else key, tidy value) | (key, value) <- KeyMap.toList o]))
  Array items -> Array (V.map tidy items)
  other -> other
  where
    empty = object ["type" .= String "object"]
    collapse o = case KeyMap.lookup "allOf" o of
      Just (Array parts)
        | empty `elem` parts
        , [Object part] <- filter (/= empty) (V.toList parts)
        , all (`elem` ["type", "allOf"]) (KeyMap.keys (KeyMap.intersection o part)) ->
            KeyMap.union part (KeyMap.delete "allOf" o)
      _ -> o

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
  , "oneOf"
  , "pattern"
  , "minLength"
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
