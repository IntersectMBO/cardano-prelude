{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

-- This module was cargo-culted from `Protolude`.

module Cardano.Prelude.Show 
  ( Print
  , bsShow
  , hPutStr
  , putStr
  , hPutStrLn
  , putStrLn
  , putErrLn
  , putText
  , putErrText
  , putLText
  , putByteString
  , putLByteString
  , show
  , textShow
  ) where

import qualified Cardano.Prelude.Base as PBase
import qualified Cardano.Prelude.ConvertText as Conv
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ((.))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Prelude 
import qualified System.IO as IO

-- Much of this stolen from `protolude`.

class Print a where
  hPutStr :: PBase.MonadIO m => IO.Handle -> a -> m ()
  putStr :: PBase.MonadIO m => a -> m ()
  putStr = hPutStr IO.stdout
  hPutStrLn :: PBase.MonadIO m => IO.Handle -> a -> m ()
  putStrLn :: PBase.MonadIO m => a -> m ()
  putStrLn = hPutStrLn IO.stdout
  putErrLn :: PBase.MonadIO m => a -> m ()
  putErrLn = hPutStrLn IO.stderr

instance Print Text.Text where
  hPutStr = \h -> PBase.liftIO . Text.hPutStr h
  hPutStrLn = \h -> PBase.liftIO . Text.hPutStrLn h

instance Print LazyText.Text where
  hPutStr = \h -> PBase.liftIO . LazyText.hPutStr h
  hPutStrLn = \h -> PBase.liftIO . LazyText.hPutStrLn h

instance Print BS.ByteString where
  hPutStr = \h -> PBase.liftIO . BS.hPutStr h
  hPutStrLn = \h -> PBase.liftIO . BS.hPutStrLn h

instance Print LBS.ByteString where
  hPutStr = \h -> PBase.liftIO . LBS.hPutStr h
  hPutStrLn = \h -> PBase.liftIO . LBS.hPutStrLn h

instance Print [PBase.Char] where
  hPutStr = \h -> PBase.liftIO . hPutStr h
  hPutStrLn = \h -> PBase.liftIO . hPutStrLn h

-- Will do this once everything builds with this version.
-- {-# DEPRECATED show "Use Prelude.show, bsShow or textShow instead." #-}

show :: (PBase.Show a, Conv.ConvertText Prelude.String b) => a -> b
show a = Conv.toS (Prelude.show a)
{-# SPECIALIZE show :: PBase.Show  a => a -> PBase.Text  #-}
{-# SPECIALIZE show :: PBase.Show  a => a -> PBase.LText  #-}
{-# SPECIALIZE show :: PBase.Show  a => a -> Prelude.String  #-}


bsShow :: PBase.Show a => a -> BS.ByteString
bsShow a = BS.pack (Prelude.show a)

textShow :: PBase.Show a => a -> Text.Text
textShow a = Text.pack (Prelude.show a)

-- For forcing type inference
putText :: PBase.MonadIO m => Text.Text -> m ()
putText = putStrLn
{-# SPECIALIZE putText :: Text.Text -> PBase.IO () #-}

putLText :: PBase.MonadIO m => LazyText.Text -> m ()
putLText = putStrLn
{-# SPECIALIZE putLText :: LazyText.Text -> PBase.IO () #-}

putByteString :: PBase.MonadIO m => BS.ByteString -> m ()
putByteString = putStrLn
{-# SPECIALIZE putByteString :: BS.ByteString -> PBase.IO () #-}

putLByteString :: PBase.MonadIO m => LBS.ByteString -> m ()
putLByteString = putStrLn
{-# SPECIALIZE putLByteString :: LBS.ByteString -> PBase.IO () #-}

putErrText :: PBase.MonadIO m => Text.Text -> m ()
putErrText = putErrLn
{-# SPECIALIZE putErrText :: Text.Text -> PBase.IO () #-}


