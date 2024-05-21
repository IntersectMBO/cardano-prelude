{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

-- This `ConvertText` class and instances in this module were cargo-culted from `Protolude`.

-- | Non-partial text conversion typeclass and functions.
-- For an alternative with partial conversions import 'Protolude.Conv'.
module Cardano.Prelude.ConvertText
  ( ConvertText (toS)
  , ToByteString (..)
  , ToLazyByteString (..)
  , ToLazyText (..)
  , ToString (..)
  , ToText (..)
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | Convert from one Unicode textual type to another. Not for serialization/deserialization,
-- so doesn't have instances for bytestrings.
class ConvertText a b where
  toS :: a -> b

instance ConvertText String String where toS = id
instance ConvertText String T.Text where toS = T.pack
instance ConvertText String LT.Text where toS = LT.pack

instance ConvertText T.Text String where toS = T.unpack
instance ConvertText T.Text LT.Text where toS = LT.fromStrict
instance ConvertText T.Text T.Text where toS = id

instance ConvertText LT.Text String where toS = LT.unpack
instance ConvertText LT.Text T.Text where toS = LT.toStrict
instance ConvertText LT.Text LT.Text where toS = id

instance ConvertText LB.ByteString B.ByteString where toS = LB.toStrict
instance ConvertText LB.ByteString LB.ByteString where toS = id

instance ConvertText B.ByteString B.ByteString where toS = id
instance ConvertText B.ByteString LB.ByteString where toS = LB.fromStrict

class ToByteString a where
  toBS :: a -> B.ByteString

instance ToByteString B.ByteString where toBS = id
instance ToByteString LB.ByteString where toBS = LB.toStrict
instance ToByteString String where toBS = B.pack
instance ToByteString T.Text where toBS = T.encodeUtf8
instance ToByteString LT.Text where toBS = LB.toStrict . LT.encodeUtf8

class ToLazyByteString a where
  toLBS :: a -> LB.ByteString

instance ToLazyByteString B.ByteString where toLBS = LB.fromStrict
instance ToLazyByteString LB.ByteString where toLBS = id
instance ToLazyByteString String where toLBS = LB.pack
instance ToLazyByteString T.Text where toLBS = LB.fromStrict . T.encodeUtf8
instance ToLazyByteString LT.Text where toLBS = LT.encodeUtf8

class ToString a where
  toString :: a -> String

instance ToString B.ByteString where toString = B.unpack
instance ToString LB.ByteString where toString = B.unpack . LB.toStrict
instance ToString String where toString = id
instance ToString T.Text where toString = T.unpack
instance ToString LT.Text where toString = LT.unpack
  
class ToText a where
  toText :: a -> T.Text

instance ToText B.ByteString where toText = T.decodeUtf8
instance ToText LB.ByteString where toText = T.decodeUtf8 . LB.toStrict
instance ToText LT.Text where toText = LT.toStrict
instance ToText String where toText = T.pack
instance ToText T.Text where toText = id

class ToLazyText a where
  toLazyText :: a -> LT.Text

instance ToLazyText B.ByteString where toLazyText = LT.fromStrict . T.decodeUtf8
instance ToLazyText LB.ByteString where toLazyText = LT.decodeUtf8
instance ToLazyText LT.Text where toLazyText = id
instance ToLazyText String where toLazyText = LT.pack
instance ToLazyText T.Text where toLazyText = LT.fromStrict
