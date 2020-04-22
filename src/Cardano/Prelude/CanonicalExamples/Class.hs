{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Prelude.CanonicalExamples.Class (
    CanonicalExamples (..)
  , Args (..)
  , Warnings
  , Warning (..)
  , getCanonicalExamples
  , getCanonicalExamplesWithWarnings
  , stdArgs
  ) where

import Cardano.Prelude.Base hiding (evalState)
import Prelude (String)

import Data.Dynamic
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

-- | The computation of 'CanonicalExamples' is done in this 'State'-like Monad.
newtype WithContext a = WithContext (Context -> (a, Context))
    deriving Functor

instance Applicative WithContext where
    pure a =  WithContext (a,)
    (<*>)  = ap

-- | A 'State' like Monad, but part of the state (the 'Args') is read only.
instance Monad WithContext where
    WithContext f >>= k = WithContext $ \s ->
      let (a, cntx) = f s
          WithContext f' = k a
          (a', cntx') = f' cntx
      in (a', cntx' {args = args cntx})

data Context = Context {
      args         :: Args -- ^ Read only part of the 'Context'
    , stackContext :: DepthContext -- ^ Gets restored when we backtrack.
    , globalCache  :: Cache -- ^ Stores the examples of each type, for future use.
    , warnings     :: Warnings
    } deriving (Show, Generic)

initContext :: Args -> Context
initContext args' = Context {
      args         = args'
    , stackContext = Map.empty
    , globalCache  = Map.empty
    , warnings     = Set.empty
    }

-- | The 'CanonicalExamples' created are a pure function of the related type
-- and the 'Args' type. These arguments are used as guards to keep the number
-- of examples and the stack memory under control.
--
-- If we visualise the represantation of a type as a graph then:
-- o 'maxOccurences' protects us against the loops of the graph, which can
--   otherwise result in infinite loops.
--
-- o 'typesLimit' protects against graphs that grow too much in depth.
--   This can in particular happen with nested types, in which case even
--   infinite graphs are possible (see prop_Perfect).
--
-- o 'productLimit' protects against graphs that grow too much in width.
--   Types which are represented as big products of types are an example of this
--   case, since the number of their examples grows exponentially.
data Args = Args {
      maxOccurences :: Int -- ^ max recursive occurenses of each type.
    , typesLimit    :: Int -- ^ max number of types that fit in the 'stackContext'.
    , productLimit  :: Int -- ^ limit on the number of examples of product types.
    }
  deriving (Eq, Show, Generic)

stdArgs :: Int -> Args
stdArgs s = Args {
      maxOccurences = s
    , typesLimit    = 10
    , productLimit  = 10
    }

data Warning =
    LoopDetected String
  | TypeLimitReached Int -- ^ the size limit of the stack was reached.
  deriving (Eq, Show, Ord)


-- | Some warnings may appear multiple times, that's why we use a 'Set' instead
-- of a 'List'. For examples, recursive types will go through the same type up
-- to 'maxOccurences' and will create up to ('maxOccurences' - 1) same warnings.
-- Using a 'Set' reduces all this unecessary complexity.
type Warnings = Set Warning

-- |The 'DepthContext' is used to detect loops in recursive types that we
-- derive. It is a mapping from a 'TypeRep' to the number of times we have seen
-- this type, dyring our path from the source.
type DepthContext = Map TypeRep Int

type Cache = Map TypeRep Dynamic

getContext :: WithContext Context
getContext = WithContext $
    \cntx -> (cntx, cntx)

putDepthContext :: DepthContext -> WithContext ()
putDepthContext dContext = WithContext $
    \cntx -> ((), cntx {stackContext = dContext})

modifyDepthContext :: (DepthContext -> DepthContext) -> WithContext ()
modifyDepthContext f = WithContext $
    \cntx -> ((), cntx {stackContext = f (stackContext cntx)})

insertDepthContext :: TypeRep -> Int -> DepthContext -> DepthContext
insertDepthContext = Map.insert

insertCache :: forall a. Typeable a
            => [a]
            -> WithContext ()
insertCache as = WithContext $
    \cntx -> ((), cntx {
        globalCache = Map.insert tpRep (toDyn as) (globalCache cntx)
      })
  where
    tpRep :: TypeRep
    tpRep = typeRep (Proxy :: Proxy a)

-- | Throws an error if the type representation doesn't match with the dynamic
-- list of cached examples.
getCached :: Typeable a
          => TypeRep
          -> Cache
          -> Maybe [a]
getCached str cache = do
    dyn <- Map.lookup str cache
    case fromDynamic dyn of
      Nothing -> panic $ show $ "canonical examples: unexpected type " ++ show str
      Just ls -> Just ls

addWarning :: Warning -> WithContext ()
addWarning warning = WithContext $
    \cntx -> ((), cntx { warnings = Set.insert warning (warnings cntx) })

evalState :: Context -> WithContext a -> a
evalState cntx (WithContext f) = fst $ f cntx

evalStateWithWarnings :: Context -> WithContext a -> (a, Warnings)
evalStateWithWarnings cntx (WithContext f) = second warnings (f cntx)

-- | Checks if the stack limit is reached and if the examples are already
-- cached, before computing the examples.
checkCacheAndCompute
    :: forall a. Typeable a
    => TypeRep
    -> WithContext [a]
    -> Context
    -> WithContext [a]
checkCacheAndCompute tpRep getExamples Context {..} =
    case ( Map.size stackContext > typesLimit args
         , getCached tpRep globalCache
         , Map.lookup tpRep stackContext) of
      (True, _, _)     -> do
        addWarning $ TypeLimitReached $ typesLimit args
        return []
      (_, Just cac, _) -> return cac
      (_, _, Nothing)  -> insertMaybe 0
      (_, _, Just n)   -> do
        addWarning $ LoopDetected $ show tpRep
        insertMaybe n
  where
    insertMaybe :: Int -> WithContext [a]
    insertMaybe n =
      if n >= maxOccurences args
      then return []
      else do
        modifyDepthContext (insertDepthContext tpRep $ n + 1)
        examples <- getExamples
        insertCache examples
        -- restore
        putDepthContext stackContext
        return examples

{-------------------------------------------------------------------------------
  CanonicalExamples
-------------------------------------------------------------------------------}

class CanonicalExamples a where
  canonicalExamples :: WithContext [a]
  default canonicalExamples
    :: (Typeable a, Generic a, GCanonicalExamples (Rep a))
    => WithContext [a]
  canonicalExamples = do
      cntx <- getContext
      checkCacheAndCompute tpRep getExamples cntx
    where
      getExamples :: WithContext [a]
      getExamples = fmap to <$> gCanonicalExamples

      tpRep :: TypeRep
      tpRep = typeRep (Proxy :: Proxy a)

class GCanonicalExamples f where
  gCanonicalExamples :: WithContext [f a]

instance (GCanonicalExamples V1) where
  gCanonicalExamples = return []

instance (GCanonicalExamples U1) where
  gCanonicalExamples = return [U1]

instance CanonicalExamples a
  => GCanonicalExamples (K1 i a) where
  gCanonicalExamples = fmap K1 <$> canonicalExamples

instance GCanonicalExamples a
  => GCanonicalExamples (D1 i a) where
  gCanonicalExamples = fmap M1 <$> gCanonicalExamples

instance (GCanonicalExamples a) => GCanonicalExamples (C1 i a) where
  gCanonicalExamples = fmap M1 <$> gCanonicalExamples

instance GCanonicalExamples a => GCanonicalExamples (S1 i a) where
  gCanonicalExamples = fmap M1 <$> gCanonicalExamples

instance (GCanonicalExamples a, GCanonicalExamples b)
  => GCanonicalExamples (a :+: b) where
  gCanonicalExamples = do
    l <- gCanonicalExamples
    r <- gCanonicalExamples
    return $ (L1 <$> l) ++ (R1 <$> r)

instance (GCanonicalExamples a, GCanonicalExamples b)
  => GCanonicalExamples (a :*: b) where
  gCanonicalExamples = do
    cntx <- getContext
    let limit = productLimit $ args cntx
    as <- gCanonicalExamples
    bs <- gCanonicalExamples
    return $ [a :*: b | a <- take limit as, b <- take limit bs]

getCanonicalExamples :: CanonicalExamples a => Args -> [a]
getCanonicalExamples args =
    evalState (initContext args) canonicalExamples

getCanonicalExamplesWithWarnings :: CanonicalExamples a
                                 => Args
                                 -> ([a], Warnings)
getCanonicalExamplesWithWarnings args =
    evalStateWithWarnings (initContext args) canonicalExamples

instance
     CanonicalExamples ()
instance (Typeable a, CanonicalExamples a, Typeable b, CanonicalExamples b)
  => CanonicalExamples (a, b)
instance ( Typeable a, CanonicalExamples a, Typeable b, CanonicalExamples b
         , Typeable c, CanonicalExamples c)
  => CanonicalExamples (a, b, c)
instance ( Typeable a, CanonicalExamples a, Typeable b, CanonicalExamples b
         , Typeable c, CanonicalExamples c, Typeable d, CanonicalExamples d)
  => CanonicalExamples (a, b, c, d)
instance ( Typeable a, CanonicalExamples a, Typeable b, CanonicalExamples b
         ,  Typeable c, CanonicalExamples c, Typeable d, CanonicalExamples d
         , Typeable e, CanonicalExamples e)
  => CanonicalExamples (a, b, c, d, e)
instance (Typeable a, CanonicalExamples a) => CanonicalExamples (Maybe a)
instance (Typeable a, Typeable b, CanonicalExamples a, CanonicalExamples b)
  => CanonicalExamples (Either a b)
-- TODO(kde) why we need to do this?
instance CanonicalExamples (Proxy a) where
    canonicalExamples = return [Proxy]
instance (CanonicalExamples a, Typeable a)
  => CanonicalExamples (Identity a)
instance CanonicalExamples Void
instance (Typeable a, CanonicalExamples a) => CanonicalExamples [a]
instance (Typeable a, CanonicalExamples a) => CanonicalExamples (NonEmpty a)
