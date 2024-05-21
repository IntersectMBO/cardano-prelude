{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

module Cardano.Prelude.Base (
  module X,
  HasLength (..),
  LByteString,
  LText,
  head,
  identity,
  length,
  liftM',
  liftM2',
  map,
  putTextLn,
  scanl',
  throwIO,
  trace,
  traceM,
  traceShow,
)
where

import Data.Map.Strict as X (Map)
import Data.List (scanl')
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Prelude qualified

import Control.Category qualified as Category
import Control.Category as X hiding (id)
import Numeric.Natural as X
import Control.Applicative as X (Applicative (..), Alternative (..), Const (..), ZipList (..),
        (<**>), liftA, liftA2, liftA3, optional, pure)
import Control.Concurrent as X (ThreadId, forkIO, forkFinally, forkIOWithUnmask,
        killThread, forkOn, forkOnWithUnmask, getNumCapabilities, setNumCapabilities,
        threadCapability, yield, threadDelay, threadWaitRead, threadWaitWrite,
-- https://www.snoyman.com/base/
#if MIN_VERSION_base(4,7,0)
        threadWaitReadSTM, threadWaitWriteSTM,
#endif
        rtsSupportsBoundThreads, forkOS,
#if MIN_VERSION_base(4,9,0)
        forkOSWithUnmask,
#endif
        isCurrentThreadBound, runInBoundThread, runInUnboundThread, mkWeakThreadId, myThreadId)
import Control.Concurrent.Async as X (Async(..), Concurrently(..), async, asyncBound, asyncOn,
        withAsync, withAsyncBound, withAsyncOn, wait, poll, waitCatch, cancel, cancelWith,
        asyncThreadId, waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel, waitEither,
        waitEitherCatch, waitEitherCancel, waitEitherCatchCancel, waitEither_, waitBoth, link,
        link2, race, race_, concurrently)
import Control.Concurrent.MVar as X (MVar, newEmptyMVar, newMVar, takeMVar, putMVar, readMVar,
        swapMVar, tryTakeMVar, tryPutMVar, isEmptyMVar, withMVar, withMVarMasked, modifyMVar_,
        modifyMVar, modifyMVarMasked_, modifyMVarMasked, tryReadMVar, mkWeakMVar, addMVarFinalizer)

import Control.DeepSeq as X (NFData (..), ($!!), force)
import Control.Exception as X (Exception, toException, fromException,
#if MIN_VERSION_base(4,8,0)
        displayException,
#endif
        SomeException (..), IOException, ArithException (..), ArrayException (..),
        AssertionFailed (..),
#if MIN_VERSION_base(4,7,0)
        SomeAsyncException (..), asyncExceptionToException, asyncExceptionFromException,
#endif
        AsyncException (..), NonTermination (..), NestedAtomically (..),
        BlockedIndefinitelyOnMVar (..), BlockedIndefinitelyOnSTM (..),
#if MIN_VERSION_base(4,8,0)
        AllocationLimitExceeded (..),
#endif
#if MIN_VERSION_base(4,10,0)
        CompactionFailed (..),
#endif
        Deadlock (..), NoMethodError (..), PatternMatchFail (..), RecConError (..),
        RecSelError (..), RecUpdError (..),
#if MIN_VERSION_base(4,9,0)
        ErrorCall (..),
#else
        ErrorCall (..),
#endif
#if MIN_VERSION_base(4,9,0)
        TypeError (..),
#endif
        ioError, catch, catches, Handler (..), catchJust, handle, handleJust, try, tryJust,
        evaluate, mapException, mask, mask_, uninterruptibleMask, uninterruptibleMask_,
        MaskingState (..), getMaskingState,
#if MIN_VERSION_base(4,9,0)
        interruptible,
#endif
        allowInterrupt, bracket, bracket_, bracketOnError, finally, onException)
import Control.Monad as X (Monad, MonadPlus, (=<<), (>>=), (<=<), (>=>), (>>), (<$!>), ap, filterM,
        foldM, foldM_, forever, join, guard, join, liftM, liftM2, liftM3, liftM4, liftM5, 
        mapAndUnzipM, mfilter, mplus, mzero, replicateM, replicateM_, return, void, when,
        unless, zipWithM, zipWithM_)
import Control.Monad.Extra as X (concatMapM)
import Control.Monad.Except as X (MonadError, Except, ExceptT (..), throwError, catchError,
        runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT)
import Control.Monad.Fail as X (MonadFail)
import Control.Monad.Reader as X (MonadReader, Reader, ReaderT (..), ask, asks, local, reader,
        runReader, runReaderT)
import Control.Monad.STM as X (STM, atomically, retry, orElse, check, throwSTM, catchSTM)
import Control.Monad.State as X (MonadState, State, StateT(StateT), put, get, gets, modify,
        state, withState, runState, execState, evalState, runStateT, execStateT, evalStateT)
import Control.Monad.Trans as X (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except as X (throwE, catchE)
import Data.ByteString as X (ByteString)
import Data.Bifunctor as X (Bifunctor, bimap, first, second)
import Data.Bits as X
import Data.Char as X
import Data.Either as X (Either (..), either, fromLeft, fromRight, lefts, rights, partitionEithers,
    isLeft, isRight)
import Data.Foldable as X (Foldable, fold, foldMap, foldr, foldr', foldl, foldl', toList,
#if MIN_VERSION_base(4,8,0)
        null, 
#endif
        elem, maximum, minimum, foldrM, foldlM, traverse_, for_, mapM_, forM_, sequence_,
        sequenceA_, asum, msum, concat, concatMap, and, or, any, all, maximumBy, minimumBy,
        notElem, find)
import Data.Function as X ((&), on)
import Data.Functor as X (Functor (..), (<$>), ($>), (<&>))
import Data.Functor.Identity as X (Identity (..), runIdentity)
import Data.Int as X (Int, Int8, Int16, Int32, Int64)
import Data.Kind as X (Type)
import Data.Ord as X (Ord (..), Ordering (..), Down (..), comparing)
import Data.List as X (splitAt, break, intercalate, isPrefixOf, isInfixOf, isSuffixOf, drop, filter,
        groupBy, reverse, replicate, take, sortBy, sort, intersperse, transpose, subsequences,
        permutations, scanl, scanr, sortBy, iterate, repeat, cycle, unfoldr, takeWhile, dropWhile,
        group, inits, tails, zipWith, zip, unzip, genericLength, genericTake, genericDrop,
        genericSplitAt, genericReplicate)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Maybe as X (Maybe (..), maybe, isJust, isNothing, fromMaybe, listToMaybe, maybeToList,
        catMaybes, mapMaybe)
import Data.Monoid as X (Monoid (..), First (..), Last (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..), All (..), Any (..), diff)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.String as X (IsString)
import Data.Text as X (Text, lines, words, unlines, unwords)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Text.Encoding.Error as X (OnDecodeError, OnError, UnicodeException, lenientDecode,
        strictDecode, ignore, replace)
import Data.Traversable as X
import Data.Tuple as X (fst, snd, curry, uncurry, swap)
import Data.Type.Coercion as X (Coercion(..), coerceWith, repr)
import Data.Type.Equality as X ((:~:)(..), type (==), sym, trans, castWith, gcastWith)
import Data.Typeable as X (Typeable, typeOf, typeRep)
import Data.Void as X (Void, absurd, vacuous)
import Data.Word as X (Word, Word8, Word16, Word32, Word64)
import Foreign.Ptr as X (Ptr)
import GHC.Generics as X (Generic (..), Generic1, Rep, K1 (..), M1 (..), U1 (..), V1, D1, C1, S1,
        (:+:)(..), (:*:)(..), (:.:)(..), Rec0, Constructor (..), Datatype (..), Selector (..),
        Fixity (..), Associativity (..),
#if ( __GLASGOW_HASKELL__ >= 800 )
        Meta (..), FixityI (..), URec
#endif
        )
import GHC.Real as X ((%), (/), Fractional, Integral, Ratio (..), Rational, Real, RealFrac, (^), (^%^),
        (^^), (^^%^^), ceiling, denominator, div, divMod,
#if MIN_VERSION_base(4,7,0)
        divZeroError,
#endif
        even, floor, fromIntegral, fromRational, gcd,
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,15,0)
#if defined(MIN_VERSION_integer_gmp)
        gcdInt', gcdWord',
#endif
#endif
        infinity, integralEnumFrom, integralEnumFromThen, integralEnumFromThenTo,
        integralEnumFromTo, lcm, mod, notANumber, numerator, numericEnumFrom, numericEnumFromThen,
        numericEnumFromThenTo, numericEnumFromTo, odd,
#if MIN_VERSION_base(4,7,0)
        overflowError,
#endif
        properFraction, quot, quotRem, ratioPrec, ratioPrec1,
#if MIN_VERSION_base(4,7,0)
        ratioZeroDenominatorError,
#endif
        realToFrac, recip, reduce, rem, round, showSigned, toInteger, truncate,
#if MIN_VERSION_base(4,12,0)
        underflowError
#endif
        )
import GHC.Records as X (HasField, getField)
import GHC.Stack as X
import GHC.TypeLits as X (Symbol, SomeSymbol (..), Nat, SomeNat (..), CmpNat, KnownSymbol, KnownNat,
        natVal, someNatVal, symbolVal, someSymbolVal)
import Prelude as X (Bool (..), Bounded (..), Double, Enum (..), Eq (..), Float,
        Integer, Num (..), Read, Real (..), Show,
#if __GLASGOW_HASKELL__ >= 906
        type (~),
#endif
        ($), ($!), (++), (||), (*), (&&),
        const, flip, isNaN, not, otherwise, sqrt, sum)
import System.Exit as X (ExitCode(..), exitWith, exitFailure, exitSuccess)
import System.IO as X (Handle, IO, FilePath, IOMode(..), stdin, stdout, stderr,
         withFile, openFile)
import Control.Exception qualified as Exception
import Data.ByteString.Lazy.Char8 qualified as LBS
-- Need to import this qualifed so we can redefine `length` below.
import Data.Foldable qualified  as Foldable
import Data.Functor qualified  as Functor
import Data.Text.Lazy qualified as LT
import Debug.Trace qualified  as Debug

type LByteString = LBS.ByteString
type LText = LT.Text

-- | Rename `id` to `identity` to allow `id` as a variable name
identity :: Category cat => cat a a
identity = Category.id

-- | Explicit output with @Text@ since that is what we want most of the time.
-- We don't want to look at the type errors or warnings arising.
putTextLn :: Text -> IO ()
putTextLn = Text.putStrLn

-- Length which includes @Text@ as well as @Foldable@.
class HasLength a where
  length' :: a -> Int

instance HasLength Text where
  length' = Text.length

instance Foldable.Foldable t => HasLength (t a) where
  length' = Foldable.length

-- Need this `head` to be the same as the `head` provided by `protolude`.
head :: Foldable.Foldable f => f a -> Maybe a
head = Foldable.foldr (\x _ -> pure x) Nothing

-- | We can pass several things here, as long as they have length.
length :: HasLength a => a -> Int
length = length'

map :: Functor.Functor f => (a -> b) -> f a -> f b
map = Functor.fmap

throwIO :: (X.MonadIO m, Exception.Exception e) => e -> m a
throwIO = X.liftIO . Exception.throwIO

{-# WARNING trace "'trace' remains in code" #-}
trace :: Prelude.String -> a -> a
trace = Debug.trace

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: Prelude.Applicative f => Prelude.String -> f () 
traceM = Debug.traceM

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: Prelude.String -> a -> a
traceShow = Debug.traceShow

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = (<$!>)
{-# INLINE liftM' #-}

liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  x <- a
  y <- b
  let z = f x y
  z `Prelude.seq` return z
{-# INLINE liftM2' #-}

