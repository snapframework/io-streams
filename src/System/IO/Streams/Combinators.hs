-- | Generic stream manipulations

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Streams.Combinators
 ( -- * Folds
   inputFoldM
 , outputFoldM
   -- * Maps
 , mapM
 , contramapM
   -- * Utility
 , skipToEof
 ) where

------------------------------------------------------------------------------
import Control.Monad              ( liftM )
import Data.IORef                 ( atomicModifyIORef
                                  , newIORef
                                  , readIORef
                                  , writeIORef
                                  )
import Prelude             hiding ( mapM, read )
------------------------------------------------------------------------------
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , makeInputStream
                                  , makeOutputStream
                                  , read
                                  , write
                                  )


------------------------------------------------------------------------------
outputFoldM :: (a -> b -> IO a)
            -> a
            -> OutputStream b
            -> IO (OutputStream b, IO a)
outputFoldM f initial stream = do
    ref <- newIORef initial
    os  <- makeOutputStream (wr ref)
    return (os, fetch ref)

  where
    wr _ Nothing       = write Nothing stream
    wr ref mb@(Just x) = do
        !z  <- readIORef ref
        !z' <- f z x
        writeIORef ref z'
        write mb stream

    fetch ref = atomicModifyIORef ref $ \x -> (initial, x)


------------------------------------------------------------------------------
inputFoldM :: (a -> b -> IO a)
           -> a
           -> InputStream b
           -> IO (InputStream b, IO a)
inputFoldM f initial stream = do
    ref <- newIORef initial
    is  <- makeInputStream (rd ref)
    return (is, fetch ref)

  where
    twiddle _ Nothing = return Nothing

    twiddle ref mb@(Just x) = do
        !z  <- readIORef ref
        !z' <- f z x
        writeIORef ref z'
        return mb

    rd ref = read stream >>= twiddle ref

    fetch ref = atomicModifyIORef ref $ \x -> (initial, x)


------------------------------------------------------------------------------
-- | Maps an impure function over an 'InputStream'.
--
-- @mapM f s@ passes all output from @s@ through the impure function @f@.
--
-- Satisfies the following laws:
--
-- > mapM (f >=> g) = mapM f >=> mapM g
-- >
-- > mapM return = return
mapM :: (a -> IO b) -> InputStream a -> IO (InputStream b)
mapM f s = makeInputStream g
  where
    g = do
        mb <- read s >>= maybe (return Nothing)
                               (\x -> liftM Just $ f x)

        return mb


------------------------------------------------------------------------------
-- | Contravariant counterpart to 'mapM'.
--
-- (@contramapM f s@) passes all input to @s@ through the impure function @f@
--
-- Satisfies the following laws:
--
-- > contramapM (f >=> g) = contramapM g >=> contramapM f
-- >
-- > contramapM return = return
contramapM :: (a -> IO b) -> OutputStream b -> IO (OutputStream a)
contramapM f s = makeOutputStream g
  where
    g Nothing = write Nothing s

    g (Just x) = do
        !y <- f x
        write (Just y) s


------------------------------------------------------------------------------
-- | Drives an 'InputStream' to end-of-stream, discarding all of the yielded
-- values.
skipToEof :: InputStream a -> IO ()
skipToEof str = go
  where
    go = read str >>= maybe (return ()) (const go)
{-# INLINE skipToEof #-}
