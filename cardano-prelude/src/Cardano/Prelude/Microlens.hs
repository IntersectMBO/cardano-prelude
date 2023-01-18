{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Prelude.Microlens () where

import Data.Functor ((<$>))
import Lens.Micro.Internal (Field1 (..), Field2 (..), Field3 (..), Field4 (..), Field5 (..))

instance Field1 (a, b, c, d, e, f) (a', b, c, d, e, f) a a' where
  _1 k ~(a, b, c, d, e, f) = (\a' -> (a', b, c, d, e, f)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c, d, e, f, g) (a', b, c, d, e, f, g) a a' where
  _1 k ~(a, b, c, d, e, f, g) = (\a' -> (a', b, c, d, e, f, g)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c, d, e, f, g, h) (a', b, c, d, e, f, g, h) a a' where
  _1 k ~(a, b, c, d, e, f, g, h) = (\a' -> (a', b, c, d, e, f, g, h)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c, d, e, f, g, h, i) (a', b, c, d, e, f, g, h, i) a a' where
  _1 k ~(a, b, c, d, e, f, g, h, i) = (\a' -> (a', b, c, d, e, f, g, h, i)) <$> k a
  {-# INLINE _1 #-}

instance Field2 (a, b, c, d, e, f) (a, b', c, d, e, f) b b' where
  _2 k ~(a, b, c, d, e, f) = (\b' -> (a, b', c, d, e, f)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c, d, e, f, g) (a, b', c, d, e, f, g) b b' where
  _2 k ~(a, b, c, d, e, f, g) = (\b' -> (a, b', c, d, e, f, g)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c, d, e, f, g, h) (a, b', c, d, e, f, g, h) b b' where
  _2 k ~(a, b, c, d, e, f, g, h) = (\b' -> (a, b', c, d, e, f, g, h)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c, d, e, f, g, h, i) (a, b', c, d, e, f, g, h, i) b b' where
  _2 k ~(a, b, c, d, e, f, g, h, i) = (\b' -> (a, b', c, d, e, f, g, h, i)) <$> k b
  {-# INLINE _2 #-}

instance Field3 (a, b, c, d, e, f) (a, b, c', d, e, f) c c' where
  _3 k ~(a, b, c, d, e, f) = (\c' -> (a, b, c', d, e, f)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a, b, c, d, e, f, g) (a, b, c', d, e, f, g) c c' where
  _3 k ~(a, b, c, d, e, f, g) = (\c' -> (a, b, c', d, e, f, g)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a, b, c, d, e, f, g, h) (a, b, c', d, e, f, g, h) c c' where
  _3 k ~(a, b, c, d, e, f, g, h) = (\c' -> (a, b, c', d, e, f, g, h)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a, b, c, d, e, f, g, h, i) (a, b, c', d, e, f, g, h, i) c c' where
  _3 k ~(a, b, c, d, e, f, g, h, i) = (\c' -> (a, b, c', d, e, f, g, h, i)) <$> k c
  {-# INLINE _3 #-}

instance Field4 (a, b, c, d, e, f) (a, b, c, d', e, f) d d' where
  _4 k ~(a, b, c, d, e, f) = (\d' -> (a, b, c, d', e, f)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a, b, c, d, e, f, g) (a, b, c, d', e, f, g) d d' where
  _4 k ~(a, b, c, d, e, f, g) = (\d' -> (a, b, c, d', e, f, g)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a, b, c, d, e, f, g, h) (a, b, c, d', e, f, g, h) d d' where
  _4 k ~(a, b, c, d, e, f, g, h) = (\d' -> (a, b, c, d', e, f, g, h)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a, b, c, d, e, f, g, h, i) (a, b, c, d', e, f, g, h, i) d d' where
  _4 k ~(a, b, c, d, e, f, g, h, i) = (\d' -> (a, b, c, d', e, f, g, h, i)) <$> k d
  {-# INLINE _4 #-}

instance Field5 (a, b, c, d, e, f) (a, b, c, d, e', f) e e' where
  _5 k ~(a, b, c, d, e, f) = (\e' -> (a, b, c, d, e', f)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a, b, c, d, e, f, g) (a, b, c, d, e', f, g) e e' where
  _5 k ~(a, b, c, d, e, f, g) = (\e' -> (a, b, c, d, e', f, g)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a, b, c, d, e, f, g, h) (a, b, c, d, e', f, g, h) e e' where
  _5 k ~(a, b, c, d, e, f, g, h) = (\e' -> (a, b, c, d, e', f, g, h)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e', f, g, h, i) e e' where
  _5 k ~(a, b, c, d, e, f, g, h, i) = (\e' -> (a, b, c, d, e', f, g, h, i)) <$> k e
  {-# INLINE _5 #-}
