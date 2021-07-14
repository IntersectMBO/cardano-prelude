{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Generic definitions we use for the @lattices@ package
module Cardano.Prelude.GHC.Generics.Lattices
  ( -- * Methods
    bottomViaGeneric
  , joinViaGeneric
  , meetViaGeneric
  , topViaGeneric
    -- * Constraints
  , GLattice
  , GBoundedJoinSemiLattice
  , GBoundedMeetSemiLattice
  ) where

import Cardano.Prelude.Base

import Algebra.Lattice
import qualified GHC.TypeLits as GHC
import Prelude (error)

--------------------------------------------------------------------------------
-- Lattice instances
--------------------------------------------------------------------------------

meetViaGeneric :: (Generic a, GLattice (Rep a)) => a -> a -> a
meetViaGeneric l r = to $ gmeet (from l) (from r)

joinViaGeneric :: (Generic a, GLattice (Rep a)) => a -> a -> a
joinViaGeneric l r = to $ gjoin (from l) (from r)

class GLattice rep where
  gjoin :: rep x -> rep x -> rep x
  gmeet :: rep x -> rep x -> rep x

instance Lattice c => GLattice (K1 i c) where
  gjoin (K1 l) (K1 r) = K1 $ l \/ r
  gmeet (K1 l) (K1 r) = K1 $ l /\ r

instance GLattice f => GLattice (M1 i c f) where
  gjoin (M1 l) (M1 r) = M1 $ gjoin l r
  gmeet (M1 l) (M1 r) = M1 $ gmeet l r

instance GLattice V1 where
  gjoin = \case {}
  gmeet = \case {}

instance GLattice U1 where
  gjoin U1 U1 = U1
  gmeet U1 U1 = U1

instance (GLattice l, GLattice r) => GLattice (l :*: r) where
  gjoin (l1 :*: r1) (l2 :*: r2) = gjoin l1 l2 :*: gjoin r1 r2
  gmeet (l1 :*: r1) (l2 :*: r2) = gmeet l1 l2 :*: gmeet r1 r2

instance GHC.TypeError (         GHC.Text "No GHC.Generics definition of "
                        GHC.:<>: GHC.ShowType Lattice
                        GHC.:<>: GHC.Text " for types with multiple constructors "
                        GHC.:<>: GHC.ShowType l
                       )
      => GLattice (l :+: r) where
  gjoin = error "GLattice \\/ :+:"
  gmeet = error "GLattice /\\ :+:"

--------------------------------------------------------------------------------
-- BoundedMeetSemiLattice instances
--------------------------------------------------------------------------------

topViaGeneric :: (Generic a, GBoundedMeetSemiLattice (Rep a)) => a
topViaGeneric = to gtop

class GBoundedMeetSemiLattice rep where
  gtop :: rep x

instance BoundedMeetSemiLattice c => GBoundedMeetSemiLattice (K1 i c) where
  gtop = K1 top

instance GBoundedMeetSemiLattice f => GBoundedMeetSemiLattice (M1 i c f) where
  gtop = M1 gtop

instance GBoundedMeetSemiLattice V1 where
  gtop = error "GBoundedMeetSemiLattice V1"

instance GBoundedMeetSemiLattice U1 where
  gtop = U1

instance (GBoundedMeetSemiLattice l, GBoundedMeetSemiLattice r) => GBoundedMeetSemiLattice (l :*: r) where
  gtop = gtop :*: gtop

instance GHC.TypeError (         GHC.Text "No GHC.Generics definition of "
                        GHC.:<>: GHC.ShowType BoundedMeetSemiLattice
                        GHC.:<>: GHC.Text " for types with multiple constructors "
                        GHC.:<>: GHC.ShowType l
                       )
      => GBoundedMeetSemiLattice (l :+: r) where
  gtop = error "GBoundedMeetSemiLattice :+:"

--------------------------------------------------------------------------------
-- BoundedJoinSemiLattice instances
--------------------------------------------------------------------------------

bottomViaGeneric :: (Generic a, GBoundedJoinSemiLattice (Rep a)) => a
bottomViaGeneric = to gbottom

class GBoundedJoinSemiLattice rep where
  gbottom :: rep x

instance BoundedJoinSemiLattice c => GBoundedJoinSemiLattice (K1 i c) where
  gbottom = K1 bottom

instance GBoundedJoinSemiLattice f => GBoundedJoinSemiLattice (M1 i c f) where
  gbottom = M1 gbottom

instance GBoundedJoinSemiLattice V1 where
  gbottom = error "GBoundedJoinSemiLattice V1"

instance GBoundedJoinSemiLattice U1 where
  gbottom = U1

instance (GBoundedJoinSemiLattice l, GBoundedJoinSemiLattice r) => GBoundedJoinSemiLattice (l :*: r) where
  gbottom = gbottom :*: gbottom

instance GHC.TypeError (         GHC.Text "No GHC.Generics definition of "
                        GHC.:<>: GHC.ShowType BoundedJoinSemiLattice
                        GHC.:<>: GHC.Text " for types with multiple constructors "
                        GHC.:<>: GHC.ShowType l
                       )
      => GBoundedJoinSemiLattice (l :+: r) where
  gbottom = error "GBoundedJoinSemiLattice :+:"
