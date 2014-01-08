{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Stream operations on 'ByteString'.
module System.IO.Streams.ByteString
 ( -- * Counting bytes
   countInput
 , countOutput

   -- * Treating strings as streams
 , fromByteString
 , fromLazyByteString

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
 , giveExactly
 , takeBytes
 , takeExactly
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
 , TooFewBytesWrittenException

 ) where

------------------------------------------------------------------------------
import           Control.Exception                 (Exception, throwIO)
import           Control.Monad                     (when, (>=>))
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as S
import qualified Data.ByteString.Lazy.Char8        as L
import qualified Data.ByteString.Unsafe            as S
import           Data.Char                         (isSpace)
import           Data.Int                          (Int64)
import           Data.IORef                        (IORef, newIORef,
                                                    readIORef, writeIORef)
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Typeable                     (Typeable)

import           Prelude                           hiding (lines, read,
                                                    takeWhile, unlines,
                                                    unwords, words)
------------------------------------------------------------------------------
import           System.IO.Streams.Combinators     (filterM, intersperse,
                                                    outputFoldM)
import           System.IO.Streams.Internal        (InputStream (..),
                                                    OutputStream,
                                                    makeInputStream,
                                                    makeOutputStream, read,
                                                    unRead, write)
import           System.IO.Streams.Internal.Search (MatchInfo (..), search)
import           System.IO.Streams.List            (fromList, writeList)
------------------------------------------------------------------------------

{-# INLINE modifyRef #-}
modifyRef :: IORef a -> (a -> a) -> IO ()
modifyRef ref f = do
    x <- readIORef ref
    writeIORef ref $! f x


------------------------------------------------------------------------------
-- | Writes a lazy 'ByteString' to an 'OutputStream'.
--
-- Example:
--
-- @
-- ghci> Streams.'writeLazyByteString' \"Test\\n\" Streams.'System.IO.Streams.stdout'
-- Test
-- @
writeLazyByteString :: L.ByteString             -- ^ string to write to output
                    -> OutputStream ByteString  -- ^ output stream
                    -> IO ()
writeLazyByteString = writeList . L.toChunks
{-# INLINE writeLazyByteString #-}


------------------------------------------------------------------------------
-- | Creates an 'InputStream' from a 'ByteString'.
fromByteString :: ByteString -> IO (InputStream ByteString)
fromByteString = fromList . (:[])


------------------------------------------------------------------------------
-- | Creates an 'InputStream' from a lazy 'ByteString'.
fromLazyByteString :: L.ByteString -> IO (InputStream ByteString)
fromLazyByteString = fromList . L.toChunks


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
    ref    <- newIORef (0 :: Int64)
    return $! (InputStream (prod ref) (pb ref), readIORef ref)

  where
    prod ref = read src >>= maybe (return Nothing) (\x -> do
        modifyRef ref (+ (fromIntegral $ S.length x))
        return $! Just x)

    pb ref s = do
        modifyRef ref (\x -> x - (fromIntegral $ S.length s))
        unRead s src


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
takeBytes k0 = takeBytes' k0 (return Nothing)
{-# INLINE takeBytes #-}


------------------------------------------------------------------------------
-- | Like @Streams.'takeBytes'@, but throws 'ReadTooShortException' when
-- there aren't enough bytes present on the source.
takeExactly :: Int64                        -- ^ number of bytes to read
            -> InputStream ByteString       -- ^ input stream to wrap
            -> IO (InputStream ByteString)
takeExactly k0 = takeBytes' k0 (throwIO $ ReadTooShortException k0)
{-# INLINE takeExactly #-}


------------------------------------------------------------------------------
-- Helper for the two above.
takeBytes' :: Int64
           -> IO (Maybe ByteString)
           -- ^ What to do if the input ends before having consumed the
           -- right amount of bytes.
           -> InputStream ByteString
           -> IO (InputStream ByteString)
takeBytes' k0 h src = do
    kref <- newIORef k0
    return $! InputStream (prod kref) (pb kref)
  where
    prod kref = do
        k <- readIORef kref
        if k <= 0
           then return Nothing
           else read src >>= maybe h (chunk k)
      where
        chunk k s = do
            let l  = fromIntegral $ S.length s
            let k' = k - l
            if k' <= 0
              then let (a,b) = S.splitAt (fromIntegral k) s
                   in do
                       when (not $ S.null b) $ unRead b src
                       writeIORef kref 0
                       return $! Just a
              else writeIORef kref k' >> return (Just s)

    pb kref s = do
        modifyRef kref (+ (fromIntegral $ S.length s))
        unRead s src
{-# INLINE takeBytes' #-}


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
splitOn p is = do
    ref <- newIORef id
    makeInputStream $ start ref
  where
    start ref = go
      where
        go  = read is >>= maybe end chunk

        end = do
            dl <- readIORef ref
            case dl [] of
              [] -> return Nothing
              xs -> writeIORef ref id >>
                    (return $! Just $! S.concat xs)

        chunk s = let (a, b) = S.break p s
                  in if S.null b
                       then modifyRef ref (\f -> f . (a:)) >> go
                       else do
                         let !b' = S.unsafeDrop 1 b
                         dl <- readIORef ref

                         if S.null b'
                           then do
                             writeIORef ref ("" :)
                             return $ Just $! S.concat $ dl [a]
                           else do
                             writeIORef ref id
                             unRead b' is
                             return $ Just $! S.concat $ dl [a]


------------------------------------------------------------------------------
-- | Splits a bytestring 'InputStream' into lines. See 'splitOn' and
-- 'Prelude.lines'.
--
-- Example:
--
-- @
-- ghci> is \<- Streams.'System.IO.Streams.fromList' [\"Hello,\\n world!\"] >>= Streams.'lines'
-- ghci> replicateM 3 (Streams.'read' is)
-- [Just \"Hello\", Just \", world!\", Nothing]
-- @
--
-- Note that this may increase the chunk size if the input contains extremely
-- long lines.
lines :: InputStream ByteString -> IO (InputStream ByteString)
lines = splitOn (== '\n')


------------------------------------------------------------------------------
-- | Splits a bytestring 'InputStream' into words. See 'splitOn' and
-- 'Prelude.words'.
--
-- Example:
--
-- @
-- ghci> is \<- Streams.'System.IO.Streams.fromList' [\"Hello, world!\"] >>= Streams.'words'
-- ghci> replicateM 3 (Streams.'read' is)
-- [Just \"Hello,\", Just \"world!\", Nothing]
-- @
--
-- Note that this may increase the chunk size if the input contains extremely
-- long words.
words :: InputStream ByteString -> IO (InputStream ByteString)
words = splitOn isSpace >=> filterM (return . not . S.all isSpace)


------------------------------------------------------------------------------
-- | Intersperses string chunks sent to the given 'OutputStream' with newlines.
-- See 'intersperse' and 'Prelude.unlines'.
--
-- @
-- ghci> os <- Streams.'unlines' Streams.'System.IO.Streams.stdout'
-- ghci> Streams.'write' (Just \"Hello,\") os
-- Hello
-- ghci> Streams.'write' Nothing os
-- ghci> Streams.'write' (Just \"world!\") os
-- world!
-- @
unlines :: OutputStream ByteString -> IO (OutputStream ByteString)
unlines os = makeOutputStream $ \m -> do
    write m os
    case m of
        Nothing -> return $! ()
        Just _  -> write (Just "\n") os


------------------------------------------------------------------------------
-- | Intersperses string chunks sent to the given 'OutputStream' with spaces.
-- See 'intersperse' and 'Prelude.unwords'.
--
-- @
-- ghci> os <- Streams.'unwords' Streams.'System.IO.Streams.stdout'
-- ghci> forM_ [Just \"Hello,\", Nothing, Just \"world!\\n\"] $ \w -> Streams.'write' w os
-- Hello, world!
-- @
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
-- | Thrown by 'giveExactly' when too few bytes were written to the produced
-- 'OutputStream'.
data TooFewBytesWrittenException = TooFewBytesWrittenException deriving (Typeable)

instance Show TooFewBytesWrittenException where
    show TooFewBytesWrittenException = "Too few bytes written"

instance Exception TooFewBytesWrittenException


------------------------------------------------------------------------------
-- | Thrown by 'throwIfConsumesMoreThan' when too many bytes were sent to the
-- produced 'OutputStream'.
data TooManyBytesWrittenException =
    TooManyBytesWrittenException deriving (Typeable)

instance Show TooManyBytesWrittenException where
    show TooManyBytesWrittenException = "Too many bytes written"

instance Exception TooManyBytesWrittenException


------------------------------------------------------------------------------
-- | Thrown by 'readExactly' and 'takeExactly' when not enough bytes were
-- available on the input.
data ReadTooShortException = ReadTooShortException Int64 deriving (Typeable)

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
throwIfProducesMoreThan k0 src = do
    kref <- newIORef k0
    return $! InputStream (prod kref) (pb kref)
  where
    prod kref = read src >>= maybe (return Nothing) chunk
      where
        chunk s = do
            k <- readIORef kref
            let k'    = k - l
            case () of !_ | l == 0  -> return (Just s)
                          | k == 0  -> throwIO TooManyBytesReadException
                          | k' >= 0 -> writeIORef kref k' >> return (Just s)
                          | otherwise -> do
                                let (!a,!b) = S.splitAt (fromEnum k) s
                                writeIORef kref 0
                                unRead b src
                                return $! Just a
          where
            l     = toEnum $ S.length s

    pb kref s = do
        unRead s src
        modifyRef kref (+ (fromIntegral $ S.length s))


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
        maybe (throwIO $ ReadTooShortException (fromIntegral n))
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
-- @
-- ghci> Streams.'System.IO.Streams.fromList' [\"Hello, world!\"] >>= Streams.'takeBytesWhile' (/= ',')
-- Just \"Hello\"
-- ghci> import Data.Char
-- ghci> Streams.'System.IO.Streams.fromList' [\"7 Samurai\"] >>= Streams.'takeBytesWhile' isAlpha
-- Just \"\"
-- ghci> Streams.'System.IO.Streams.fromList' [] >>= Streams.'takeBytesWhile' isAlpha
-- Nothing
-- @
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
giveBytes k0 str = do
    kref <- newIORef k0
    makeOutputStream $ sink kref
  where
    sink _ Nothing        = write Nothing str
    sink kref mb@(Just x) = do
        k <- readIORef kref
        let l  = fromIntegral $ S.length x
        let k' = k - l
        if k' < 0
          then do let a = S.take (fromIntegral k) x
                  when (not $ S.null a) $ write (Just a) str
                  writeIORef kref 0
          else writeIORef kref k' >> write mb str


------------------------------------------------------------------------------
-- | Wraps an 'OutputStream', producing a new stream that will pass along
-- exactly @n@ bytes to the wrapped stream. If the stream is sent more or fewer
-- than the given number of bytes, the resulting stream will throw an exception
-- (either 'TooFewBytesWrittenException' or 'TooManyBytesWrittenException')
-- during a call to 'write'.
--
-- Example:
--
-- @
-- ghci> is <- Streams.'System.IO.Streams.fromList' [\"ok\"]
-- ghci> Streams.'System.IO.Streams.outputToList' (Streams.'giveExactly' 2 >=> Streams.'System.IO.Streams.connect' is)
-- [\"ok\"]
-- ghci> is <- Streams.'System.IO.Streams.fromList' [\"ok\"]
-- ghci> Streams.'System.IO.Streams.outputToList' (Streams.'giveExactly' 1 >=> Streams.'System.IO.Streams.connect' is)
-- *** Exception: Too many bytes written
-- ghci> is <- Streams.'System.IO.Streams.fromList' [\"ok\"]
-- ghci> Streams.'System.IO.Streams.outputToList' (Streams.'giveExactly' 3 >=> Streams.'System.IO.Streams.connect' is)
-- *** Exception: Too few bytes written
-- @
giveExactly :: Int64
            -> OutputStream ByteString
            -> IO (OutputStream ByteString)
giveExactly k0 os = do
    ref <- newIORef k0
    makeOutputStream $ go ref
  where
    go ref chunk = do
        !n <- readIORef ref
        case chunk of
          Nothing -> if n /= 0
                       then throwIO TooFewBytesWrittenException
                       else return $! ()
          Just s  -> let n' = n - fromIntegral (S.length s)
                     in if n' < 0
                          then throwIO TooManyBytesWrittenException
                          else do writeIORef ref n'
        write chunk os


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
throwIfConsumesMoreThan k0 str = do
    kref   <- newIORef k0
    makeOutputStream $ sink kref
  where
    sink _ Nothing        = write Nothing str

    sink kref mb@(Just x) = do
        k <- readIORef kref
        let l  = toEnum $ S.length x
        let k' = k - l
        if k' < 0
          then throwIO TooManyBytesWrittenException
          else writeIORef kref k' >> write mb str


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
    bytesRead <- newIORef (0 :: Int64)
    return $! InputStream (prod startTime bytesRead) (pb bytesRead)

  where
    prod startTime bytesReadRef = read stream >>= maybe (return Nothing) chunk
      where
        chunk s = do
            let slen = S.length s
            now <- getTime
            let !delta = now - startTime
            nb <- readIORef bytesReadRef
            let newBytes = nb + fromIntegral slen
            when (delta > minSeconds + 1 &&
                  (fromIntegral newBytes /
                    (delta - minSeconds)) < minRate) $
                throwIO RateTooSlowException
            -- otherwise, bump the timeout and return the input
            !_ <- bump
            writeIORef bytesReadRef newBytes
            return $! Just s

    pb bytesReadRef s = do
        modifyRef bytesReadRef $ \x -> x - (fromIntegral $ S.length s)
        unRead s stream

    minSeconds = fromIntegral minSeconds'
