{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- A module with orphan instances of basic types. We provide them separately,
-- since someone may want to write his own examples for the basic types.
-- Ideally someone should be able to provide his own instances for a specific
-- basic type, while keeping all the others, but Haskell does not allow this.
module Cardano.Prelude.CanonicalExamples.Orphans () where

import Cardano.Prelude.Base

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (Rational)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Cardano.Prelude.CanonicalExamples.Class

{-------------------------------------------------------------------------------
  CanonicalExamples
-------------------------------------------------------------------------------}

instance CanonicalExamples Bool where
  canonicalExamples = return [False]

instance CanonicalExamples Char where
  canonicalExamples = return ['0']

instance CanonicalExamples Double where
  canonicalExamples = return [0]

instance CanonicalExamples Float where
  canonicalExamples = return [0]

instance CanonicalExamples Integer where
  canonicalExamples = return [0]

instance CanonicalExamples Int where
  canonicalExamples = return [0]

instance CanonicalExamples Int8 where
  canonicalExamples = return [0]

instance CanonicalExamples Int16 where
  canonicalExamples = return [0]

instance CanonicalExamples Int32 where
  canonicalExamples = return [0]

instance CanonicalExamples Int64 where
  canonicalExamples = return [0]

instance CanonicalExamples Word where
  canonicalExamples = return [0]

instance CanonicalExamples Word8 where
  canonicalExamples = return [0]

instance CanonicalExamples Word16 where
  canonicalExamples = return [0]

instance CanonicalExamples Word32 where
  canonicalExamples = return [0]

instance CanonicalExamples Word64 where
  canonicalExamples = return [0]

instance CanonicalExamples Natural where
  canonicalExamples = return [0]

instance (Ord a, Typeable a, CanonicalExamples a)
    => CanonicalExamples (Set a) where
  canonicalExamples = fmap Set.fromList <$> canonicalExamples

instance (Typeable a, CanonicalExamples a)
    => CanonicalExamples (Vector a) where
      canonicalExamples = fmap Vector.fromList <$> canonicalExamples

instance (Ord k, CanonicalExamples k, CanonicalExamples v)
    => CanonicalExamples (Map k v) where
    canonicalExamples = do
      ks <- canonicalExamples
      vs <- canonicalExamples
      return [Map.fromList [(k, v) | k <- ks, v <- vs]]

instance (Typeable a, CanonicalExamples a) => CanonicalExamples (Seq a) where
    canonicalExamples = fmap Seq.fromList <$> canonicalExamples

instance CanonicalExamples Text where
  canonicalExamples = fmap Text.pack <$> canonicalExamples

instance CanonicalExamples BS.ByteString where
  canonicalExamples = return ["\0\128\255"]

instance CanonicalExamples LBS.ByteString where
  canonicalExamples = return ["\0\128\255"]

instance CanonicalExamples SBS.ShortByteString where
  canonicalExamples = return ["\0\128\255"]

instance CanonicalExamples BA.ScrubbedBytes where
  canonicalExamples = return ["\0\128\255"]

instance CanonicalExamples Rational where
    canonicalExamples = return [2 :% 3]

instance CanonicalExamples NominalDiffTime where
    canonicalExamples = return [nominalDay]

instance CanonicalExamples UTCTime where
    canonicalExamples = fmap posixSecondsToUTCTime <$> canonicalExamples
