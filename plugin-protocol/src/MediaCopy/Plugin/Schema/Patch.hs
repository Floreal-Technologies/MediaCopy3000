-- | What autodocodec-schema cannot say about the schemas in @$defs@.
module MediaCopy.Plugin.Schema.Patch
  ( patches
  ) where

import Data.Aeson
import Data.Function ((&))
import Data.List (List)
import Data.Text (Text)

import MediaCopy.Plugin.Api qualified as Api
import MediaCopy.Plugin.Capability qualified as Capability
import MediaCopy.Plugin.Field qualified as Field
import MediaCopy.Plugin.JsonSchema (Patch, describe, extend, extendProperty, property, ref, require)
import MediaCopy.Plugin.Manifest qualified as Manifest

-- | The patches, by name in @$defs@.
patches :: List (Text, Patch)
patches =
  [ ("pluginId", extend ["pattern" .= String "^(?!.*\\.\\.)[a-z0-9_-][a-z0-9._-]*\\.[a-z0-9._-]*[a-z0-9_-]$"])
  , ("manifest", manifest)
  , ("authorSlot", extendProperty "role" ["minLength" .= (1 :: Int)])
  , ("field", field)
  , ("initialiseParams", property "entitlement" (object ["type" .= String "null"] & describe "Always null in API 1.0."))
  , ("initialiseResult", extendProperty "api" ["const" .= Api.major])
  , ("jobKind", \_ -> object ["enum" .= (["offload", "verify", "seal"] :: [Text])])
  ,
    ( "hashAlgo"
    , \_ -> object ["enum" .= (["xxh64", "md5", "sha1", "c4"] :: [Text])] & describe "The name of a hash format, as the element name in an ASC MHL manifest."
    )
  ]

manifest :: Patch
manifest schema =
  schema
    & extendProperty "description" ["pattern" .= String "\\S"]
    & extendProperty "api" ["const" .= Api.major]
    & extendProperty "namespace" ["not" .= object ["enum" .= Manifest.reservedNamespaces]]
    & extendProperty "executable" ["additionalProperties" .= object ["type" .= String "string", "pattern" .= String "^(?![/\\\\])(?!.*:)(?!(?:.*[/\\\\])?\\.\\.(?:[/\\\\]|$)).+$"]]
    & extendProperty "capabilities" ["contains" .= object ["enum" .= Capability.hooks]]
    & extendProperty "settings" ["contains" .= authors, "minContains" .= (0 :: Int), "maxContains" .= (1 :: Int)]
    & extendProperty "jobFields" ["items" .= (ref "field" & property "kind" (object ["not" .= object ["const" .= String "authors"]]))]
    & require "capabilities"
    & extend
      [ "allOf"
          .= [ object ["if" .= declares Capability.Block, "then" .= declares Capability.PlanInspect]
             , object ["if" .= declares Capability.ManifestWrite, "then" .= object ["required" .= ["namespace" :: Text]]]
             , object ["if" .= object ["properties" .= object ["settings" .= object ["contains" .= authors]], "required" .= ["settings" :: Text]], "then" .= declares Capability.ManifestWrite]
             ]
      ]
  where
    authors = object ["properties" .= object ["kind" .= object ["const" .= String "authors"]], "required" .= ["kind" :: Text]]
    declares capability = object ["properties" .= object ["capabilities" .= object ["contains" .= object ["const" .= capability]]]]

field :: Patch
field schema =
  schema
    & extendProperty "kind" ["enum" .= map Field.kindName Field.fieldKinds]
    & extendProperty "required" ["default" .= False]
    & extend
      [ "allOf"
          .= [ object ["if" .= kindIs "choice", "then" .= object ["required" .= ["options" :: Text]]]
             , object ["if" .= kindIs "authors", "then" .= object ["not" .= object ["required" .= ["default" :: Text]]]]
             ]
      ]
  where
    kindIs :: Text -> Value
    kindIs name = object ["properties" .= object ["kind" .= object ["const" .= name]]]
