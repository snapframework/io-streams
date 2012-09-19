{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Stream operations on 'ByteString'.
module System.IO.Streams.ByteString
 ( -- * Counting bytes
   countInput
 , countOutput

   -- * Input and output
 , readExactly
 , writeLazyByteString

   -- * Stream transformers
 , giveBytes
 , takeBytes
 , throwIfConsumesMoreThan
 , throwIfProducesMoreThan

   -- * Rate limiting
 , throwIfTooSlow

   -- * String search
 , MatchInfo(..)
 , search

   -- * Exception types
 , RateTooSlowException
 , ReadTooShortException
 , TooManyBytesReadException
 , TooManyBytesWrittenException

 ) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad                                 (when)
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as S
import qualified Data.ByteString.Lazy.Char8                    as L
import           Data.Int
import           Data.Time.Clock.POSIX                         (getPOSIXTime)
import           Data.Typeable
import           Prelude                                       hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Combinators
import           System.IO.Streams.Internal
import           System.IO.Streams.Internal.BoyerMooreHorspool
import           System.IO.Streams.List


------------------------------------------------------------------------------
-- | Write a lazy 'ByteString' to an 'OutputStream'.
writeLazyByteString :: L.ByteString             -- ^ string to write to output
                    -> OutputStream ByteString  -- ^ output stream
                    -> IO ()
writeLazyByteString = writeList . L.toChunks
{-# INLINE writeLazyByteString #-}


------------------------------------------------------------------------------
-- | Wraps an 'InputStream', counting the number of bytes produced by the
-- stream as a side effect. Produces a new 'InputStream' as well as an IO
-- action to retrieve the count of bytes produced.
countInput :: InputStream ByteString -> IO (InputStream ByteString, IO Int64)
countInput = inputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', counting the number of bytes consumed by the
-- stream as a side effect. Produces a new 'OutputStream' as well as an IO
-- action to retrieve the count of bytes consumed.
countOutput :: OutputStream ByteString
            -> IO (OutputStream ByteString, IO Int64)
countOutput = outputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
-- | Wrap an 'InputStream', producing a new 'InputStream' that will produce at
-- most n, then yielding EOF forever.
takeBytes :: Int64                        -- ^ maximum number of bytes to read
          -> InputStream ByteString       -- ^ input stream to wrap
          -> IO (InputStream ByteString)
takeBytes k0 src = sourceToStream $ source k0
  where
    fromBS s = if S.null s then Nothing else Just s

    eof !n = return (eofSrc n, Nothing)

    eofSrc !n = Source {
               produce = eof n
             , pushback = pb n
             }

    pb !n s = do
        unRead s src
        return $! source $! n + toEnum (S.length s)

    source !k = Source {
                  produce  = read src >>= maybe (eof k) chunk
                , pushback = pb k
                }

      where
        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' <=  0
                       then let (a,b) = S.splitAt (fromEnum k) s
                            in do
                                when (not $ S.null b) $ unRead b src
                                return (eofSrc 0, fromBS a)
                       else return (source k', Just s)


------------------------------------------------------------------------------
data TooManyBytesReadException = TooManyBytesReadException deriving (Typeable)

instance Show TooManyBytesReadException where
    show TooManyBytesReadException = "Too many bytes read"

instance Exception TooManyBytesReadException


------------------------------------------------------------------------------
data TooManyBytesWrittenException =
    TooManyBytesWrittenException deriving (Typeable)

instance Show TooManyBytesWrittenException where
    show TooManyBytesWrittenException = "Too many bytes written"

instance Exception TooManyBytesWrittenException


------------------------------------------------------------------------------
data ReadTooShortException = ReadTooShortException Int deriving (Typeable)

instance Show ReadTooShortException where
    show (ReadTooShortException x) = "Short read, expected " ++ show x
                                     ++ " bytes"

instance Exception ReadTooShortException


------------------------------------------------------------------------------
-- | Wraps an 'InputStream'. If more than @n@ bytes are produced by this
-- stream, 'read' will throw a 'TooManyBytesReadException'.
throwIfProducesMoreThan
    :: Int64                    -- ^ maximum number of bytes to read
    -> InputStream ByteString   -- ^ input stream
    -> IO (InputStream ByteString)
throwIfProducesMoreThan k0 src = sourceToStream $ source k0
  where
    eofSrc n = Source {
                 produce  = eof n
               , pushback = pb n
               }

    eof n = return (eofSrc n, Nothing)

    pb n s = do
        unRead s src
        return $! source $! n + toEnum (S.length s)

    source !k = Source prod (pb k)
      where
        prod = read src >>= maybe (eof k) chunk

        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' < 0
                       then throwIO TooManyBytesReadException
                       else return (source k', Just s)


------------------------------------------------------------------------------
-- | Read an @n@-byte ByteString from an input stream. Throws a
-- 'ReadTooShortException' if fewer than @n@ bytes were available.
--
readExactly :: Int                     -- ^ number of bytes to read
            -> InputStream ByteString  -- ^ input stream
            -> IO ByteString
readExactly n input = go id n
  where
    go !dl 0  = return $! S.concat $! dl []
    go !dl k  =
        read input >>=
        maybe (throwIO $ ReadTooShortException n)
              (\s -> do
                 let l = S.length s
                 if l >= k
                   then do
                     let (a,b) = S.splitAt k s
                     when (not $ S.null b) $ unRead b input
                     return $! S.concat $! dl [a]
                   else go (dl . (s:)) (k - l))


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', producing a new stream that will pass along at
-- most @n@ bytes to the wrapped stream, throwing any subsequent input away.
--
giveBytes :: Int64                        -- ^ maximum number of bytes to send
                                          -- to the wrapped stream
          -> OutputStream ByteString      -- ^ output stream to wrap
          -> IO (OutputStream ByteString)
giveBytes k0 str = sinkToStream $ sink k0
  where
    sink !k = Sink g
      where
        g Nothing     = write Nothing str >> return nullSink

        g mb@(Just x) = let l  = toEnum $ S.length x
                            k' = k - l
                        in if k' < 0
                             then do
                                 let a = S.take (fromEnum k) x
                                 when (not $ S.null a) $ write (Just a) str
                                 return nullStr
                             else write mb str >> return (sink k')

    nullStr = Sink h

    h Nothing = write Nothing str >> return nullSink
    h _       = return nullSink


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', producing a new stream that will pass along at
-- most @n@ bytes to the wrapped stream. If more than @n@ bytes are sent to the
-- outer stream, a 'TooManyBytesWrittenException' will be thrown.
--
-- Note that if more than @n@ bytes are sent to the outer stream,
-- 'throwIfConsumesMoreThan' will not necessarily send the first @n@ bytes
-- through to the wrapped stream before throwing the exception.
throwIfConsumesMoreThan
    :: Int64                    -- ^ maximum number of bytes to send to the
                                --   wrapped stream
    -> OutputStream ByteString  -- ^ output stream to wrap
    -> IO (OutputStream ByteString)
throwIfConsumesMoreThan k0 str = sinkToStream $ sink k0
  where
    sink !k = Sink g
      where
        g Nothing     = write Nothing str >> return nullSink

        g mb@(Just x) = let l  = toEnum $ S.length x
                            k' = k - l
                        in if k' < 0
                             then throwIO TooManyBytesWrittenException
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
-- | Rate-limit an input stream. If the input stream is not read from faster
-- than the given rate, reading from the wrapped stream will throw a
-- 'RateTooSlowException'.
throwIfTooSlow
    :: IO ()                   -- ^ action to bump timeout
    -> Double                  -- ^ minimum data rate, in bytes per second
    -> Int                     -- ^ amount of time to wait before data rate
                               --   calculation takes effect
    -> InputStream ByteString  -- ^ input stream
    -> IO (InputStream ByteString)
throwIfTooSlow !bump !minRate !minSeconds' !stream = do
    !_        <- bump
    startTime <- getTime

    sourceToStream $ source startTime 0

  where
    minSeconds = fromIntegral minSeconds'

    source !startTime = proc
      where
        eof !nb = return (eofSrc nb, Nothing)
        eofSrc !nb = Source { produce = eof nb
                            , pushback = pb nb }

        pb !nb s = do
            unRead s stream
            return $ proc $ nb - S.length s

        proc !nb = Source prod (pb nb)
          where
            prod = do
                mb <- read stream
                maybe (eof nb)
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
