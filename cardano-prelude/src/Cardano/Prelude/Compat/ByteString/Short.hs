{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Cardano.Prelude.Compat.ByteString.Short
  ( unsafeIndex
  ) where

-- GHC >= 9.0 does not export unsafeIndex for ShortByteString
-- GHC < 9.0 does not export the ShortByteString constructor SBS.
-- Coniditional compile to work around this.

#if __GLASGW_HASKELL__ >= 900
import Data.ByteString.Short (ShortByteString(..))
import Data.Int (Int)
import Data.Word (Word8)
import GHC.Exts (indexWord8Array#)

unsafeIndex :: ShortByteString -> Int -> Word8
unsafeIndex (SBS ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

#else

import Data.ByteString.Short (ShortByteString, index)
import Data.Int (Int)
import Data.Word (Word8)

unsafeIndex :: ShortByteString -> Int -> Word8
unsafeIndex = index

#endif
