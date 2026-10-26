module MediaCopy.Plugin.Generators
  ( genText
  , genOptionalText
  , genValue
  ) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Vector qualified as V
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genText :: Gen Text
genText = Gen.text (Range.linear 0 20) Gen.unicode

-- | An optional text is never empty: the empty text decodes as 'Nothing'.
genOptionalText :: Gen (Maybe Text)
genOptionalText = Gen.maybe (Gen.text (Range.linear 1 20) Gen.unicode)

genValue :: Gen Value
genValue =
  Gen.recursive
    Gen.choice
    [ pure Null
    , Bool <$> Gen.bool
    , String <$> genText
    , Number . fromIntegral <$> Gen.int (Range.linearFrom 0 minBound maxBound)
    , toJSON <$> Gen.double (Range.linearFracFrom 0 (-1e9) 1e9)
    ]
    [ Array . V.fromList <$> Gen.list (Range.linear 0 4) genValue
    , Object . KeyMap.fromList <$> Gen.list (Range.linear 0 4) ((\k v -> (Key.fromText k, v)) <$> genText <*> genValue)
    ]
