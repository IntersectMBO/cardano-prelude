{-# LANGUAGE FlexibleContexts #-}

module Cardano.Prelude.Compat (
  readEither,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.Either (Either)
import Data.Function ((.))
import Data.String (String)
import Text.Read (Read)

import qualified Protolude.Conv as Conv
import qualified Text.Read as Read

-- TODO Delete this when all projects have updated protolude-0.3.2
readEither :: (Read a, Conv.StringConv String e, Conv.StringConv e String) => e -> Either e a
readEither = first Conv.toS . Read.readEither . Conv.toS
