{-# LANGUAGE FlexibleContexts #-}

module Cardano.Prelude.Read (
  readEither,
) where

import Data.Bifunctor (Bifunctor (first))

import qualified Cardano.Prelude.ConvertText as Conv
import qualified Text.Read as Read

-- A more general readEither than the one in Text.Read
readEither :: (Read a, Conv.ConvertText String e, Conv.ConvertText e String) => e -> Either e a
readEither = first Conv.toS . Read.readEither . Conv.toS
