{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Stream operations on 'ByteString'.
module System.IO.Streams.ByteString
 ( -- * Counting bytes
   countInput
 , countOutput

   -- * Input and output
 , readExactly
 , takeBytesWhile
 , writeLazyByteString

   -- * Stream transformers
   -- ** Splitting/Joining
 , splitOn
 , lines
 , unlines
 , words
 , unwords

   -- ** Other
 , giveBytes
 , takeBytes
 , throwIfConsumesMoreThan
 , throwIfProducesMoreThan

   -- ** Rate limiting
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
import           Control.Exception                             (Exception,
                                                                throwIO)
import           Control.Monad                                 (when, (>=>))
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as S
import qualified Data.ByteString.Lazy.Char8                    as L
import           Data.Char                                     (isSpace)
import           Data.Int                                      (Int64)
import           Data.IORef                                    (newIORef,
                                                                readIORef,
                                                                writeIORef)
import           Data.Time.Clock.POSIX                         (getPOSIXTime)
import           Data.Typeable                                 (Typeable)

import           Prelude                                       hiding (lines,
                                                                read, takeWhile,
                                                                unlines,
                                                                unwords, words)
------------------------------------------------------------------------------
import           System.IO.Streams.Combinators                 (filterM,
                                                                intersperse,
                                                                outputFoldM)
import           System.IO.Streams.Internal                    (InputStream,
                                                                OutputStream,
                                                                SP (..),
                                                                Sink (..),
                                                                Source (..),
                                                                nullSink,
                                                                nullSource,
                                                                pushback, read,
                                                                sinkToStream,
                                                                sourceToStream,
                                                                unRead,
                                                                withDefaultPushback,
                                                                write)
import           System.IO.Streams.Internal.BoyerMooreHorspool (MatchInfo (..),
                                                                search)
import           System.IO.Streams.List                        (writeList)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Writes a lazy 'ByteString' to an 'OutputStream'.
--
-- Example:
--
-- > ghci> Streams.handleToOutputStream stdout >>= Streams.writeLazyByteString "Test\n"
-- > Test
writeLazyByteString :: L.ByteString             -- ^ string to write to output
                    -> OutputStream ByteString  -- ^ output stream
                    -> IO ()
writeLazyByteString = writeList . L.toChunks
{-# INLINE writeLazyByteString #-}


------------------------------------------------------------------------------
-- | Wraps an 'InputStream', counting the number of bytes produced by the
-- stream as a side effect. Produces a new 'InputStream' as well as an IO
-- action to retrieve the count of bytes produced.
--
-- Strings pushed back to the returned 'InputStream' will be pushed back to the
-- original stream, and the count of produced bytes will be subtracted
-- accordingly.
--
-- Example:
--
-- @
-- ghci> is <- Streams.'System.IO.Streams.fromList' [\"abc\", \"def\", \"ghi\"::ByteString]
-- ghci> (is', getCount) <- Streams.'countInput' is
-- ghci> Streams.'read' is'
-- Just \"abc\"
-- ghci> getCount
-- 3
-- ghci> Streams.'unRead' \"bc\" is'
-- ghci> getCount
-- 1
-- ghci> Streams.'System.IO.Streams.peek' is
-- Just \"bc\"
-- ghci> Streams.'System.IO.Streams.toList' is'
-- [\"bc\",\"def\",\"ghi\"]
-- ghci> getCount
-- 9
-- @
--
countInput :: InputStream ByteString -> IO (InputStream ByteString, IO Int64)
countInput src = do
    ref    <- newIORef 0
    stream <- sourceToStream $ source ref
    return $! (stream, readIORef ref)

  where
    -- strict modify necessary here, also don't care about CAS overhead in this
    -- case.
    modify ref f = do
        !c <- readIORef ref
        writeIORef ref $! f c

    eof !ref = return $! SP (eofSrc ref) Nothing

    eofSrc !ref = Source {
                    produce  = eof ref
                  , pushback = pb ref
                  }

    pb !ref !s = do
        unRead s src
        modify ref $ \x -> x - (toEnum $ S.length s)
        return $! source ref

    source ref = Source {
                   produce  = read src >>= maybe (eof ref) chunk
                 , pushback = pb ref
                 }
      where
        chunk s = let !l = toEnum $ S.length s
                  in do
                      modify ref (+ l)
                      return $! SP (source ref) (Just s)


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', counting the number of bytes consumed by the
-- stream as a side effect. Produces a new 'OutputStream' as well as an IO
-- action to retrieve the count of bytes consumed.
--
-- Example:
--
-- @
-- ghci> (os :: OutputStream ByteString, getList) <- Streams.'System.IO.Streams.listOutputStream'
-- ghci> (os', getCount) <- Streams.'countOutput' os
-- ghci> Streams.'System.IO.Streams.fromList' [\"abc\", \"def\", \"ghi\"] >>= Streams.'System.IO.Streams.connectTo' os'
-- ghci> getList
-- [\"abc\",\"def\",\"ghi\"]
-- ghci> getCount
-- 9
-- @
countOutput :: OutputStream ByteString
            -> IO (OutputStream ByteString, IO Int64)
countOutput = outputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
-- | Wraps an 'InputStream', producing a new 'InputStream' that will produce at
-- most @n@ bytes, subsequently yielding end-of-stream forever.
--
-- Strings pushed back to the returned 'InputStream' will be propagated
-- upstream, modifying the count of taken bytes accordingly.
--
-- Example:
--
-- @
-- ghci> is <- Streams.'System.IO.Streams.fromList' [\"truncated\", \" string\"::ByteString]
-- ghci> is' <- Streams.'takeBytes' 9 is
-- ghci> Streams.'read' is'
-- Just \"truncated\"
-- ghci> Streams.'read' is'
-- Nothing
-- ghci> Streams.'System.IO.Streams.peek' is
-- Just \" string\"
-- ghci> Streams.'unRead' \"cated\" is'
-- ghci> Streams.'System.IO.Streams.peek' is
-- Just \"cated\"
-- ghci> Streams.'System.IO.Streams.peek' is'
-- Just \"cated\"
-- ghci> Streams.'read' is'
-- Just \"cated\"
-- ghci> Streams.'read' is'
-- Nothing
-- ghci> Streams.'read' is
-- Just \" string\"
-- @
takeBytes :: Int64                        -- ^ maximum number of bytes to read
          -> InputStream ByteString       -- ^ input stream to wrap
          -> IO (InputStream ByteString)
takeBytes k0 src = sourceToStream $ source k0
  where
    fromBS s = if S.null s then Nothing else Just s

    eof !n = return $! SP (eofSrc n) Nothing

    eofSrc !n = Source (eof n) (pb n)

    pb !n s = do
        unRead s src
        return $! source $! n + toEnum (S.length s)

    source !k = Source (read src >>= maybe (eof k) chunk) (pb k)
      where
        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' <=  0
                       then let (a,b) = S.splitAt (fromEnum k) s
                            in do
                                when (not $ S.null b) $ unRead b src
                                return $! SP (eofSrc 0) (fromBS a)
                       else return $! SP (source k') (Just s)


------------------------------------------------------------------------------
-- | Splits an 'InputStream' over 'ByteString's using a delimiter predicate.
--
-- Note that:
--
--   * data pushed back with 'unRead' is *not* propagated upstream here.
--
--   * the resulting 'InputStream' may hold an unbounded amount of the
--     bytestring in memory waiting for the function to return true, so this
--     function should not be used in unsafe contexts.
--
--   * the delimiter is NOT included in the output.
--
--   * consecutive delimiters are not merged.
--
-- Example:
--
-- @
-- ghci> Streams.'System.IO.Streams.fromList' [\"the quick br\", \"own  fox\"::'ByteString'] >>=
--       Streams.'splitOn' (== \' \') >>= Streams.'System.IO.Streams.toList'
-- [\"the\",\"quick\",\"brown\",\"\",\"fox\"]
-- @
--
splitOn :: (Char -> Bool)               -- ^ predicate used to break the input
                                        -- stream into chunks
        -> InputStream ByteString       -- ^ input stream
        -> IO (InputStream ByteString)
splitOn p is = sourceToStream $ withDefaultPushback newChunk
  where
    newChunk = read is >>=
               maybe (return (SP nullSource Nothing)) (go id . Just)

    go !dl Nothing = return $! SP nullSource (Just $! S.concat $! dl [])

    go !dl (Just s) = do
        let (a,b) = S.break p s
        if S.null b
          then read is >>= go (dl . (a:))
          else do
              let b'   = S.drop 1 b
              let rest = withDefaultPushback $ go id (Just b')
              return $! SP rest $! Just $! S.concat $! dl [a]


------------------------------------------------------------------------------
-- | Splits a bytestring 'InputStream' into lines. See 'splitOn' and
-- 'Prelude.lines'.
--
-- Example:
--
-- > ghci> is <- Streams.fromList ["Hello,\n world!"] >>= Streams.lines
-- > ghci> replicateM 3 (Streams.read is)
-- > [Just "Hello", Just ", world!", Nothing]
lines :: InputStream ByteString -> IO (InputStream ByteString)
lines = splitOn (== '\n')


------------------------------------------------------------------------------
-- | Splits a bytestring 'InputStream' into words. See 'splitOn' and
-- 'Prelude.words'.
--
-- Example:
--
-- > ghci> is <- Streams.fromList ["Hello, world!"] >>= Streams.words
-- > ghci> replicateM 3 (Streams.read is)
-- > [Just "Hello,", Just "world!", Nothing]
words :: InputStream ByteString -> IO (InputStream ByteString)
words = splitOn isSpace >=> filterM (return . not . S.all isSpace)


------------------------------------------------------------------------------
-- | Intersperses string chunks sent to the given 'OutputStream' with newlines.
-- See 'intersperse' and 'Prelude.unlines'.
unlines :: OutputStream ByteString -> IO (OutputStream ByteString)
unlines = intersperse "\n"


------------------------------------------------------------------------------
-- | Intersperses string chunks sent to the given 'OutputStream' with spaces.
-- See 'intersperse' and 'Prelude.unwords'.
unwords :: OutputStream ByteString -> IO (OutputStream ByteString)
unwords = intersperse " "


------------------------------------------------------------------------------
-- | Thrown by 'throwIfProducesMoreThan' when too many bytes were read from the
-- original 'InputStream'.
data TooManyBytesReadException = TooManyBytesReadException deriving (Typeable)

instance Show TooManyBytesReadException where
    show TooManyBytesReadException = "Too many bytes read"

instance Exception TooManyBytesReadException


------------------------------------------------------------------------------
-- | Thrown by 'throwIfConsumesMoreThan' when too many bytes were sent to the
-- produced 'OutputStream'.
data TooManyBytesWrittenException =
    TooManyBytesWrittenException deriving (Typeable)

instance Show TooManyBytesWrittenException where
    show TooManyBytesWrittenException = "Too many bytes written"

instance Exception TooManyBytesWrittenException


------------------------------------------------------------------------------
-- | Thrown by 'readExactly' when not enough bytes were available on the input.
data ReadTooShortException = ReadTooShortException Int deriving (Typeable)

instance Show ReadTooShortException where
    show (ReadTooShortException x) = "Short read, expected " ++ show x
                                     ++ " bytes"

instance Exception ReadTooShortException


------------------------------------------------------------------------------
-- | Wraps an 'InputStream'. If more than @n@ bytes are produced by this
-- stream, 'read' will throw a 'TooManyBytesReadException'.
--
-- If a chunk yielded by the input stream would result in more than @n@ bytes
-- being produced, 'throwIfProducesMoreThan' will cut the generated string such
-- that exactly @n@ bytes are yielded by the returned stream, and the
-- /subsequent/ read will throw an exception. Example:
--
-- @
-- ghci> is \<- Streams.'System.IO.Streams.fromList' [\"abc\", \"def\", \"ghi\"] >>=
--             Streams.'throwIfProducesMoreThan' 5
-- ghci> 'Control.Monad.replicateM' 2 ('read' is)
-- [Just \"abc\",Just \"de\"]
-- ghci> Streams.'read' is
-- *** Exception: Too many bytes read
-- @
--
-- Strings pushed back to the returned 'InputStream' will be propagated
-- upstream, modifying the count of taken bytes accordingly. Example:
--
-- @
-- ghci> is  <- Streams.'System.IO.Streams.fromList' [\"abc\", \"def\", \"ghi\"]
-- ghci> is' <- Streams.'throwIfProducesMoreThan' 5 is
-- ghci> Streams.'read' is'
-- Just \"abc\"
-- ghci> Streams.'unRead' \"xyz\" is'
-- ghci> Streams.'System.IO.Streams.peek' is
-- Just \"xyz\"
-- ghci> Streams.'read' is
-- Just \"xyz\"
-- ghci> Streams.'read' is
-- Just \"de\"
-- ghci> Streams.'read' is
-- *** Exception: Too many bytes read
-- @
--
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

    eof n = return $! SP (eofSrc n) Nothing

    pb n s = do
        unRead s src
        return $! source $! n + toEnum (S.length s)

    source !k = Source prod (pb k)
      where
        prod = read src >>= maybe (eof k) chunk

        chunk s | l == 0    = return $! SP (source k) (Just s)
                | k == 0    = throwIO TooManyBytesReadException
                | k' >= 0   = return $! SP (source k') (Just s)
                | otherwise = do
                     unRead b src
                     return $! SP (source 0) (Just a)
          where
            l     = toEnum $ S.length s
            k'    = k - l
            (a,b) = S.splitAt (fromEnum k) s


------------------------------------------------------------------------------
-- | Reads an @n@-byte ByteString from an input stream. Throws a
-- 'ReadTooShortException' if fewer than @n@ bytes were available.
--
-- Example:
--
-- @
-- ghci> Streams.'System.IO.Streams.fromList' [\"long string\"] >>= Streams.'readExactly' 6
-- \"long s\"
-- ghci> Streams.'System.IO.Streams.fromList' [\"short\"] >>= Streams.'readExactly' 6
-- *** Exception: Short read, expected 6 bytes
-- @
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
-- | Takes from a stream until the given predicate is no longer satisfied.
-- Returns Nothing on end-of-stream, or @Just \"\"@ if the predicate is never
-- satisfied. See 'Prelude.takeWhile' and 'Data.ByteString.Char8.takeWhile'.
--
-- Example:
--
-- > ghci> Streams.fromList ["Hello, world!"] >>= Streams.takeBytesWhile (/= ',')
-- > Just "Hello"
-- > ghci> import Data.Char
-- > ghci> Streams.fromList ["7 Samurai"] >>= Streams.takeBytesWhile isAlpha
-- > Just ""
-- > ghci> Streams.fromList [] >>= Streams.takeBytesWhile isAlpha
-- > Nothing
takeBytesWhile :: (Char -> Bool)          -- ^ predicate
               -> InputStream ByteString  -- ^ input stream
               -> IO (Maybe ByteString)
takeBytesWhile p input = read input >>= maybe (return Nothing) (go id)
  where
    go dl !s | S.null b  = read input >>= maybe finish (go dl')
             | otherwise = unRead b input >> finish
      where
        (a, b) = S.span p s
        dl'    = dl . (a:)
        finish = return $! Just $! S.concat $! dl [a]


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', producing a new stream that will pass along at
-- most @n@ bytes to the wrapped stream, throwing any subsequent input away.
--
-- Example:
--
-- @
-- ghci> (os :: OutputStream ByteString, getList) <- Streams.'System.IO.Streams.listOutputStream'
-- ghci> os' <- Streams.'giveBytes' 6 os
-- ghci> Streams.'System.IO.Streams.fromList' [\"long \", \"string\"] >>= Streams.'System.IO.Streams.connectTo' os'
-- ghci> getList
-- [\"long \",\"s\"]
-- @
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
-- /Note/: if more than @n@ bytes are sent to the outer stream,
-- 'throwIfConsumesMoreThan' will not necessarily send the first @n@ bytes
-- through to the wrapped stream before throwing the exception.
--
-- Example:
--
-- @
-- ghci> (os :: OutputStream ByteString, getList) <- Streams.'System.IO.Streams.listOutputStream'
-- ghci> os' <- Streams.'throwIfConsumesMoreThan' 5 os
-- ghci> Streams.'System.IO.Streams.fromList' [\"short\"] >>= Streams.'System.IO.Streams.connectTo' os'
-- ghci> getList
-- [\"short\"]
-- ghci> os'' <- Streams.'throwIfConsumesMoreThan' 5 os
-- ghci> Streams.'System.IO.Streams.fromList' [\"long\", \"string\"] >>= Streams.'System.IO.Streams.connectTo' os''
-- *** Exception: Too many bytes written
-- @
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
-- | Gets the current posix time
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime


------------------------------------------------------------------------------
-- | Thrown by 'throwIfTooSlow' if input is not being produced fast enough by
-- the given 'InputStream'.
--
data RateTooSlowException = RateTooSlowException deriving (Typeable)
instance Show RateTooSlowException where
    show RateTooSlowException = "Input rate too slow"
instance Exception RateTooSlowException


------------------------------------------------------------------------------
-- | Rate-limits an input stream. If the input stream is not read from faster
-- than the given rate, reading from the wrapped stream will throw a
-- 'RateTooSlowException'.
--
-- Strings pushed back to the returned 'InputStream' will be propagated up to
-- the original stream.
throwIfTooSlow
    :: IO ()                   -- ^ action to bump timeout
    -> Double                  -- ^ minimum data rate, in bytes per second
    -> Int                     -- ^ amount of time in seconds to wait before
                               --   data rate calculation takes effect
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
        eof !nb = return $! SP (eofSrc nb) Nothing
        eofSrc !nb = Source { produce = eof nb
                            , pushback = pb nb }

        pb !nb s = do
            unRead s stream
            return $ proc $ nb - S.length s

        proc !nb = Source prod (pb nb)
          where
            prod = read stream >>=
                   maybe (eof nb)
                         (\s -> do
                            let slen = S.length s
                            now <- getTime
                            let !delta = now - startTime
                            let !newBytes = nb + slen
                            when (delta > minSeconds + 1 &&
                                  (fromIntegral newBytes /
                                    (delta - minSeconds)) < minRate) $
                                throwIO RateTooSlowException

                            -- otherwise, bump the timeout and return the input
                            !_ <- bump
                            return $! SP (proc newBytes) (Just s))
