{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Cardano.Prelude.Base (
  module X,
  HasLength (..),
  LByteString,
  LText,
  identity,
  length,
  putTextLn,
  scanl',
)
where

import Data.Map.Strict as X (Map)
import Data.List (scanl')
import Data.Text as X (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Control.Category qualified as Category
import Control.Category as X hiding (id)
import Numeric.Natural as X

import Control.Applicative as X (Applicative (..), many)
import Control.Concurrent.MVar as X (MVar, newMVar)
import Control.DeepSeq as X (NFData (..), ($!!), force)
import Control.Exception as X (Exception, bracket)
import Control.Monad as X (Monad, (=<<), (>>=), (<=<), (>=>), liftM, return, when, unless)
import Control.Monad.Except as X (MonadError, throwError)
import Control.Monad.IO.Class as X (MonadIO (..))
import Data.ByteString as X (ByteString)
import Data.Bifunctor as X (bimap, first, second)
import Data.Either as X (Either (..), either)
import Data.Foldable as X (Foldable)
import Data.Functor as X (Functor (..), (<$>))
import Data.Functor.Identity as X (Identity, runIdentity)
import Data.Int as X (Int, Int8, Int16, Int32, Int64)
import Data.Kind as X (Type)
import Data.Ord as X (Ord (..), comparing)
import Data.List as X (sortBy)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Maybe as X (Maybe (..), catMaybes, maybe, fromMaybe)
import Data.Monoid as X (Monoid (..))
import Data.Proxy as X (Proxy (..))
import Data.Ratio as X ((%), denominator, numerator)
import Data.Semigroup as X (Semigroup (..), Any, diff)
import Data.Typeable as X (Typeable, typeRep)
import Data.Word as X (Word, Word8, Word16, Word32, Word64)
import Foreign.Ptr as X (Ptr)
import GHC.Generics as X (Generic)
import GHC.Stack as X
import Prelude as X (Bool (..), Eq (..), Integer, Num (..), Read, String, Show, type (~),
        ($), (++), (||), (*), (^),
        const, fromIntegral, fst, otherwise, rem, snd, toInteger)
import System.Exit as X
import System.IO as X (Handle, IO, stderr, stdout)
import Text.Read as X (readEither)

import Data.ByteString.Lazy.Char8 qualified as LBS
-- Need to import this qualifed so we can redefine `length` below.
import Data.Foldable qualified  as Foldable
import Data.Text.Lazy qualified as LT

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

instance Foldable t => HasLength (t a) where
  length' = Foldable.length

-- | We can pass several things here, as long as they have length.
length :: HasLength a => a -> Int
length = length'
