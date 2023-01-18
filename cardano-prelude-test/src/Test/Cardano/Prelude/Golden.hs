{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions for use in golden testing of datatypes
module Test.Cardano.Prelude.Golden (
  discoverGolden,
  eachOf,
  goldenTestCanonicalJSONDec,
  goldenTestJSONDec,
  goldenTestJSON,
  goldenTestJSONPretty,
  getText,
)
where

import Cardano.Prelude

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Encode.Pretty (
  Config (..),
  Indent (..),
  NumberFormat (..),
  encodePretty',
  keyOrder,
 )
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import qualified Text.JSON.Canonical as Canonical

import Hedgehog (
  Gen,
  Group,
  Property,
  PropertyT,
  TestLimit,
  discoverPrefix,
  forAll,
  property,
  withTests,
  (===),
 )
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Internal.TH (TExpQ)

discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden"

-- | Check that @eachOf@ @testLimit@ generated @things@ @hasProperty@
eachOf ::
  (Show a, HasCallStack) =>
  TestLimit ->
  Gen a ->
  (a -> PropertyT IO ()) ->
  Property
eachOf testLimit things hasProperty =
  withFrozenCallStack
    $ withTests testLimit
      . property
    $ forAll things
      >>= hasProperty

-- | Check that Canonical JSON decodes to the datatype
goldenTestCanonicalJSONDec ::
  (Eq a, Canonical.FromJSON (Either SchemaError) a, HasCallStack, Show a) =>
  a ->
  FilePath ->
  Property
goldenTestCanonicalJSONDec x path = withFrozenCallStack $ do
  withTests 1 . property $ do
    bs <- liftIO (LB.readFile path)
    case Canonical.parseCanonicalJSON bs of
      Left err -> failWith Nothing $ "could not parse: " <> show err
      Right jsv -> case Canonical.fromJSON jsv of
        Left (schErr :: SchemaError) ->
          failWith Nothing $ LT.unpack $ toLazyText $ build schErr
        Right x' -> x === x'

-- | Only check that the datatype equals the decoding of the file
goldenTestJSONDec ::
  (Eq a, FromJSON a, HasCallStack, Show a) => a -> FilePath -> Property
goldenTestJSONDec x path = withFrozenCallStack $ withTests 1 . property $ do
  bs <- liftIO (LB.readFile path)
  case eitherDecode bs of
    Left err -> failWith Nothing $ "could not decode: " <> show err
    Right x' -> x === x'

goldenTestJSON ::
  (Eq a, FromJSON a, HasCallStack, Show a, ToJSON a) =>
  a ->
  FilePath ->
  Property
goldenTestJSON x path = withFrozenCallStack $ withTests 1 . property $ do
  bs <- liftIO (LB.readFile path)
  encode x === bs
  case eitherDecode bs of
    Left err -> failWith Nothing $ "could not decode: " <> show err
    Right x' -> x === x'

goldenTestJSONPretty ::
  (Eq a, FromJSON a, HasCallStack, Show a, ToJSON a) =>
  a ->
  FilePath ->
  Property
goldenTestJSONPretty x path =
  withFrozenCallStack
    $ withTests 1
      . property
    $ do
      bs <- liftIO (LB.readFile path)
      -- Sort keys by their order of appearance in the argument list
      -- of `keyOrder`. Keys not in the argument list are moved to the
      -- end, while their order is preserved.
      let
        defConfig' =
          Config
            { confIndent = Spaces 4
            , confCompare = keyOrder ["file", "hash"]
            , confNumFormat = Generic
            , confTrailingNewline = False
            }
      encodePretty' defConfig' x === bs
      case eitherDecode bs of
        Left err -> failWith Nothing $ "could not decode: " <> show err
        Right x' -> x === x'

-- | Text used for example values in a number of golden tests
--
--   Changing existing values in this string will break existing golden
--   tests, but it us OK to append more data to the end.
staticText :: Text
staticText =
  "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
  \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
  \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
  \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
  \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
  \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText
