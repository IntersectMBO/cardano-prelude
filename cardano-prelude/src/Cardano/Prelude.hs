module Cardano.Prelude
  ( module X
  , readMaybe
  ) where

import Cardano.Prelude.Base as X
import Cardano.Prelude.Bool as X
import Cardano.Prelude.Compat.ByteString.Short as X
import Cardano.Prelude.ConvertText as X
import Cardano.Prelude.Either as X
import Cardano.Prelude.Error as X
import Cardano.Prelude.Formatting as X
import Cardano.Prelude.Functor as X
import Cardano.Prelude.GHC.Heap as X
import Cardano.Prelude.Json.Canonical as X
import Cardano.Prelude.Json.Parse as X
import Cardano.Prelude.Microlens as X ()
import Cardano.Prelude.Orphans ()
import Cardano.Prelude.Panic as X
import Cardano.Prelude.Read as X
import Cardano.Prelude.Safe as X
import Cardano.Prelude.Show as X
import Cardano.Prelude.Strict as X

import qualified Text.Read as Read

readMaybe :: (Read b, X.ToString a) => a -> Maybe b
readMaybe a = Read.readMaybe (toString a)

