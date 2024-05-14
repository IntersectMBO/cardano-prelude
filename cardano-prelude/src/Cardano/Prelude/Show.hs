{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

-- This module was cargo-culted from `Protolude`.

module Cardano.Prelude.Show
  ( bsShow
  , show
  , textShow
  ) where

import qualified Cardano.Prelude.Base as PBase
import qualified Cardano.Prelude.ConvertText as Conv
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Prelude 

{-# DEPRECATED show "Use Prelude.show, bsShow or textShow instead." #-}

show :: (PBase.Show a, Conv.ConvertText Prelude.String b) => a -> b
show a = Conv.toS (Prelude.show a)
{-# SPECIALIZE show :: PBase.Show  a => a -> PBase.Text  #-}
{-# SPECIALIZE show :: PBase.Show  a => a -> PBase.LText  #-}
{-# SPECIALIZE show :: PBase.Show  a => a -> Prelude.String  #-}


bsShow :: PBase.Show a => a -> BS.ByteString
bsShow a = BS.pack (Prelude.show a)

textShow :: PBase.Show a => a -> Text.Text
textShow a = Text.pack (Prelude.show a)



