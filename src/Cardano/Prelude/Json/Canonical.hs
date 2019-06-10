{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances used in the canonical JSON encoding of `GenesisData`

module Cardano.Prelude.Json.Canonical
  ( SchemaError(..)
  )
where

import Cardano.Prelude.Base

import Control.Monad.Except (MonadError(throwError))
import Data.Fixed (E12, resolution)
import qualified Data.Text.Lazy.Builder as Builder (fromText)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Formatting (bprint, builder)
import Formatting.Buildable (Buildable(build))
import Text.JSON.Canonical
  ( FromJSON(fromJSON)
  , Int54
  , JSValue(JSNum, JSString)
  , ReportSchemaErrors(expected)
  , ToJSON(toJSON)
  , expectedButGotValue
  , toJSString
  )

import Cardano.Prelude.Json.Parse (parseJSString)


data SchemaError = SchemaError
  { seExpected :: !Text
  , seActual   :: !(Maybe Text)
  } deriving (Show, Eq)

instance Buildable SchemaError where
  build se = mconcat
    [ bprint ("expected " . builder) $ Builder.fromText (seExpected se)
    , case seActual se of
      Nothing     -> mempty
      Just actual -> bprint (" but got " . builder) $ Builder.fromText actual
    ]

instance
    (Applicative m, Monad m, MonadError SchemaError m)
    => ReportSchemaErrors m
  where
  expected expec actual = throwError SchemaError
    { seExpected = toS expec
    , seActual   = fmap toS actual
    }

instance Monad m => ToJSON m Int32 where
  toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word16 where
  toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word32 where
  toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word64 where
  toJSON = pure . JSString . toJSString . show

instance Monad m => ToJSON m Integer where
  toJSON = pure . JSString . toJSString . show

instance Monad m => ToJSON m Natural where
  toJSON = pure . JSString . toJSString . show

-- | For backwards compatibility we convert this to seconds
instance Monad m => ToJSON m UTCTime where
  toJSON = pure . JSNum . round . utcTimeToPOSIXSeconds

-- | For backwards compatibility we convert this to microseconds
instance Monad m => ToJSON m NominalDiffTime where
  toJSON = toJSON . (`div` 1e6) . toPicoseconds
   where
    toPicoseconds :: NominalDiffTime -> Integer
    toPicoseconds t =
      numerator (toRational t * toRational (resolution $ Proxy @E12))

instance ReportSchemaErrors m => FromJSON m Int32 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Int32" val

instance ReportSchemaErrors m => FromJSON m Word16 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Word16" val

instance ReportSchemaErrors m => FromJSON m Word32 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Word32" val

instance ReportSchemaErrors m => FromJSON m Word64 where
  fromJSON = parseJSString (readEither . toS)

instance ReportSchemaErrors m => FromJSON m Integer where
  fromJSON = parseJSString (readEither . toS)

instance MonadError SchemaError m => FromJSON m Natural where
  fromJSON = parseJSString (readEither . toS)

instance MonadError SchemaError m => FromJSON m UTCTime where
  fromJSON = fmap (posixSecondsToUTCTime . fromIntegral) . fromJSON @_ @Int54

instance MonadError SchemaError m => FromJSON m NominalDiffTime where
  fromJSON = fmap (fromRational . (% 1e6)) . fromJSON
