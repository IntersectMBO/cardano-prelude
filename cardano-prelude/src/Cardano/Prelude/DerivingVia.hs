{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is intended for qualified import.
--
-- > import qualified Cardano.Prelude.DerivingVia as Via
module Cardano.Prelude.DerivingVia
  ( Generic (..)
  , Ord (..)
  )
where

import qualified Cardano.Prelude.Base as Base
import Cardano.Prelude.GHC.Generics.Lattices

import Algebra.Lattice
import qualified Algebra.Lattice.Ordered as Lattice
import qualified Data.Semigroup.Generic as SemigroupGeneric
import qualified GHC.Generics as G

--------------------------------------------------------------------------------
-- Deriving classes via GHC.Generics.Generic
--------------------------------------------------------------------------------

-- | Derive via the 'GHC.Generics.Generic' class
--
-- This is the same concept from
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/17147>.
newtype Generic a = Generic a

instance (G.Generic a, SemigroupGeneric.GSemigroup (G.Rep a))
      => Base.Semigroup (Generic a) where
  Generic l <> Generic r = Generic (SemigroupGeneric.gmappend l r)

instance (G.Generic a, SemigroupGeneric.GMonoid (G.Rep a))
      => Base.Monoid (Generic a) where
  mempty = Generic SemigroupGeneric.gmempty

instance (G.Generic a, GLattice (G.Rep a)) => Lattice (Generic a) where
  Generic x \/ Generic y = Generic (joinViaGeneric x y)
  Generic x /\ Generic y = Generic (meetViaGeneric x y)

instance (G.Generic a, GLattice (G.Rep a), GBoundedMeetSemiLattice (G.Rep a))
      => BoundedMeetSemiLattice (Generic a) where
  top = Generic topViaGeneric

instance (G.Generic a, GLattice (G.Rep a), GBoundedJoinSemiLattice (G.Rep a))
      => BoundedJoinSemiLattice (Generic a) where
  bottom = Generic bottomViaGeneric

--------------------------------------------------------------------------------
-- Deriving classes via Ord
--------------------------------------------------------------------------------

-- | Derive via the 'Ord' class
newtype Ord a = Ord a

deriving via Lattice.Ordered a
  instance (Base.Ord a                ) => Lattice                (Ord a)

deriving via Lattice.Ordered a
  instance (Base.Ord a, Base.Bounded a) => BoundedJoinSemiLattice (Ord a)

deriving via Lattice.Ordered a
  instance (Base.Ord a, Base.Bounded a) => BoundedMeetSemiLattice (Ord a)
