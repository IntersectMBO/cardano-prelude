{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes.

module Cardano.Prelude.Orphans
  ()
where

import Cardano.Prelude.Base

#if !MIN_VERSION_nonempty_containers(0,3,4)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
#endif
import Data.Tagged (Tagged(Tagged))
import qualified Formatting as F
import Formatting.Buildable (Buildable(..))


--------------------------------------------------------------------------------
-- Aeson
--------------------------------------------------------------------------------

#if !MIN_VERSION_nonempty_containers(0,3,4)
instance (Ord a, FromJSON a) => FromJSON (NESet a) where
  parseJSON = fmap NES.fromList . parseJSON

instance (Ord a, ToJSON a) => ToJSON (NESet a) where
  toJSON = toJSON . toList
#endif


--------------------------------------------------------------------------------
-- Buildable
--------------------------------------------------------------------------------

-- | This orphan instance is sometimes useful and why not have it?
instance Buildable () where
  build _ = "()"

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
  build tt@(Tagged v) = F.bprint
    ("Tagged " F.% F.shown F.% " " F.% F.build)
    ts
    v
   where
    ts    = typeRep proxy
    proxy = (const Proxy :: Tagged s a -> Proxy s) tt
