{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Streams.ByteString
 ( countInput
 , countOutput
 , readNoMoreThan
 , TooManyBytesReadException
 , takeNoMoreThan
 , writeNoMoreThan
 , MatchInfo(..)
 , boyerMooreHorspool
 , RateTooSlowException
 , killIfTooSlow
 ) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad                                 (when)
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as S
import           Data.Int
import           Data.Time.Clock.POSIX                         (getPOSIXTime)
import           Data.Typeable
import           Prelude                                       hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Combinators
import           System.IO.Streams.Internal
import           System.IO.Streams.Internal.BoyerMooreHorspool


------------------------------------------------------------------------------
countInput :: InputStream ByteString -> IO (InputStream ByteString, IO Int64)
countInput = inputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
countOutput :: OutputStream ByteString
            -> IO (OutputStream ByteString, IO Int64)
countOutput = outputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
readNoMoreThan :: Int64
               -> InputStream ByteString
               -> IO (InputStream ByteString)
readNoMoreThan k0 src = sourceToStream $ source k0
  where
    source !k =
        Source $ read src >>= maybe (return (nullSource, Nothing)) chunk

      where
        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' < 0
                       then let (a,b) = S.splitAt (fromEnum k) s
                            in do
                                unRead b src
                                return (nullSource, Just a)
                       else return (source k', Just s)


------------------------------------------------------------------------------
data TooManyBytesReadException = TooManyBytesReadException deriving (Typeable)

instance Show TooManyBytesReadException where
    show TooManyBytesReadException = "Too many bytes read"

instance Exception TooManyBytesReadException


------------------------------------------------------------------------------
-- | Like 'readNoMoreThan', but throws an exception if the input stream
-- produces more bytes than the limit.
takeNoMoreThan :: Int64                    -- ^ maximum number of bytes to read
               -> InputStream ByteString   -- ^ input stream
               -> IO (InputStream ByteString)
takeNoMoreThan k0 src = sourceToStream $ source k0
  where
    source !k =
        Source $ read src >>= maybe (return (nullSource, Nothing)) chunk

      where
        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' < 0
                       then throwIO TooManyBytesReadException
                       else return (source k', Just s)


------------------------------------------------------------------------------
writeNoMoreThan :: Int64
                -> OutputStream ByteString
                -> IO (OutputStream ByteString)
writeNoMoreThan k0 str = sinkToStream $ sink k0
  where
    sink !k = Sink g
      where
        g Nothing     = write Nothing str >> return nullSink

        g mb@(Just x) = let l  = toEnum $ S.length x
                            k' = k - l
                        in if k' < 0
                             then let a = S.take (fromEnum k) x
                                  in write (Just a) str >> return nullSink
                             else write mb str >> return (sink k')



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
