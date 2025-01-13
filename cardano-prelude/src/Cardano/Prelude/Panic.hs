{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Safe #-}

-- This module cargo-culted from `Protolude`.
module Cardano.Prelude.Panic (
  FatalError(FatalError, fatalErrorMessage),
  panic,
) where

import Control.Exception (Exception, throw)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

-- | Uncatchable exceptions thrown and never caught.
newtype FatalError = FatalError { fatalErrorMessage :: Text }
  deriving Show

instance Exception FatalError

panic :: HasCallStack => Text -> a
panic a = throw (FatalError a)
