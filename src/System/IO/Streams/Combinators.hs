{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Streams.Combinators
 ( inputFoldM
 , outputFoldM
 , mapM
 , contramapM
 , RateTooSlowException
 , killIfTooSlow
 ) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad              (liftM, when)
import qualified Data.ByteString.Char8      as S
import           Data.ByteString.Char8      (ByteString)
import           Data.IORef
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Typeable
import           Prelude                    hiding (mapM, read)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal


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
mapM :: (a -> IO b) -> InputStream a -> IO (InputStream b)
mapM f s = makeInputStream g
  where
    g = do
        mb <- read s >>= maybe (return Nothing)
                               (\x -> liftM Just $ f x)

        return mb


------------------------------------------------------------------------------
contramapM :: (a -> IO b) -> OutputStream b -> IO (OutputStream a)
contramapM f s = makeOutputStream g
  where
    g Nothing = write Nothing s

    g (Just x) = do
        !y <- f x
        write (Just y) s


------------------------------------------------------------------------------
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime


------------------------------------------------------------------------------
data RateTooSlowException = RateTooSlowException deriving (Typeable)
instance Show RateTooSlowException where
    show RateTooSlowException = "Input rate too slow"
instance Exception RateTooSlowException


------------------------------------------------------------------------------
-- | Rate-limit an input stream. If the input stream does not produce input
-- faster than the given rate, reading from the wrapped stream will throw a
-- 'RateTooSlowException'.
killIfTooSlow
    :: IO ()                   -- ^ action to bump timeout
    -> Double                  -- ^ minimum data rate, in bytes per second
    -> Int                     -- ^ amount of time to wait before data rate
                               --   calculation takes effect
    -> InputStream ByteString  -- ^ input stream
    -> IO (InputStream ByteString)
killIfTooSlow !bump !minRate !minSeconds' !stream = do
    !_        <- bump
    startTime <- getTime

    sourceToStream $ source startTime 0

  where
    minSeconds = fromIntegral minSeconds'

    source !startTime = proc
      where
        proc !nb = Source $ do
          mb <- read stream
          maybe (return (nullSource, Nothing))
                (\s -> do
                   let slen = S.length s
                   now <- getTime
                   let !delta = now - startTime
                   let !newBytes = nb + slen
                   when (delta > minSeconds + 1 &&
                         fromIntegral newBytes /
                            (delta-minSeconds) < minRate) $
                       throwIO RateTooSlowException

                   -- otherwise, bump the timeout and return the input
                   !_ <- bump
                   return (proc newBytes, Just s))
                mb
