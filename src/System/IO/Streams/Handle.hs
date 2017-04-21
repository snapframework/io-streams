{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy       #-}
#endif

-- | Input and output streams for file 'Handle's.
module System.IO.Streams.Handle
 ( -- * Handle conversions
   handleToInputStream
 , handleToOutputStream
 , handleToStreams
 , inputStreamToHandle
 , outputStreamToHandle
 , streamPairToHandle

    -- * Flushing
 , flushOutputStream

   -- * Standard system handles
 , stdin
 , stdout
 , stderr
 ) where

------------------------------------------------------------------------------
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as S
import qualified GHC.IO.Handle              as H
import           System.IO                  (Handle, hFlush)
import qualified System.IO                  as IO
import           System.IO.Unsafe           (unsafePerformIO)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal (InputStream, OutputStream, SP (..), lockingInputStream, lockingOutputStream, makeInputStream, makeOutputStream, write)


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 32752


------------------------------------------------------------------------------
-- | Converts a read-only handle into an 'InputStream' of strict 'ByteString's.
--
-- Note that the wrapped handle is /not/ closed when it yields end-of-stream;
-- you can use 'System.IO.Streams.Combinators.atEndOfInput' to close the handle
-- if you would like this behaviour.
handleToInputStream :: Handle -> IO (InputStream ByteString)
handleToInputStream h = makeInputStream f
  where
    f = do
        x <- S.hGetSome h bUFSIZ
        return $! if S.null x then Nothing else Just x


------------------------------------------------------------------------------
-- | Converts a writable handle into an 'OutputStream' of strict 'ByteString's.
--
-- Note that the wrapped handle is /not/ closed when it receives end-of-stream;
-- you can use 'System.IO.Streams.Combinators.atEndOfOutput' to close the
-- handle if you would like this behaviour.
--
-- /Note/: to force the 'Handle' to be flushed, you can write a null string to
-- the returned 'OutputStream':
--
-- > Streams.write (Just "") os
handleToOutputStream :: Handle -> IO (OutputStream ByteString)
handleToOutputStream h = makeOutputStream f
  where
    f Nothing  = hFlush h
    f (Just x) = if S.null x
                   then hFlush h
                   else S.hPut h x


------------------------------------------------------------------------------
-- | Converts a readable and writable handle into an 'InputStream'/'OutputStream'
-- of strict 'ByteString's.
--
-- Note that the wrapped handle is /not/ closed when it receives
-- end-of-stream; you can use
-- 'System.IO.Streams.Combinators.atEndOfOutput' to close the handle
-- if you would like this behaviour.
--
-- /Note/: to force the 'Handle' to be flushed, you can write a null string to
-- the returned 'OutputStream':
--
-- > Streams.write (Just "") os
--
-- /Since: 1.3.4.0./
handleToStreams :: Handle
                -> IO (InputStream ByteString, OutputStream ByteString)
handleToStreams h = do
    is <- handleToInputStream h
    os <- handleToOutputStream h
    return $! (is, os)


------------------------------------------------------------------------------
-- | Converts an 'InputStream' over bytestrings to a read-only 'Handle'. Note
-- that the generated handle is opened unbuffered in binary mode (i.e. no
-- newline translation is performed).
--
-- Note: the 'InputStream' passed into this function is wrapped in
-- 'lockingInputStream' to make it thread-safe.
--
-- /Since: 1.0.2.0./
inputStreamToHandle :: InputStream ByteString -> IO Handle
inputStreamToHandle is0 = do
    is <- lockingInputStream is0
    h <- H.mkDuplexHandle is "*input-stream*" Nothing $! H.noNewlineTranslation
    H.hSetBuffering h H.NoBuffering
    return h


------------------------------------------------------------------------------
-- | Converts an 'OutputStream' over bytestrings to a write-only 'Handle'. Note
-- that the 'Handle' will be opened in non-buffering mode; if you buffer the
-- 'OutputStream' using the 'Handle' buffering then @io-streams@ will copy the
-- 'Handle' buffer when sending 'ByteString' values to the output, which might
-- not be what you want.
--
-- When the output buffer, if used, is flushed (using 'System.IO.hFlush'), an
-- empty string is written to the provided 'OutputStream'.
--
-- /Note/: the 'OutputStream' passed into this function is wrapped in
-- 'lockingOutputStream' to make it thread-safe.
--
-- /Since: 1.0.2.0./
outputStreamToHandle :: OutputStream ByteString -> IO Handle
outputStreamToHandle os0 = do
    os <- lockingOutputStream os0
    h <- H.mkDuplexHandle os "*output-stream*" Nothing $! H.noNewlineTranslation
    H.hSetBuffering h H.NoBuffering
    return $! h


------------------------------------------------------------------------------
-- | Converts a pair of 'InputStream' and 'OutputStream' over bytestrings to a
-- read-write 'Handle'.
--
-- Note: the streams passed into this function are wrapped in
-- locking primitives to make them thread-safe.
--
-- /Since: 1.0.2.0./
streamPairToHandle :: InputStream ByteString -> OutputStream ByteString -> IO Handle
streamPairToHandle is0 os0 = do
    is <- lockingInputStream is0
    os <- lockingOutputStream os0
    h <- H.mkDuplexHandle (SP is os) "*stream*" Nothing $! H.noNewlineTranslation
    H.hSetBuffering h H.NoBuffering
    return $! h

------------------------------------------------------------------------------
-- | Flushes buffered data down an 'OutputStream' of 'ByteString'.
--
-- /Since: ????/
flushOutputStream :: OutputStream ByteString -> IO ()
flushOutputStream = write (Just S.empty)


------------------------------------------------------------------------------
-- | An 'InputStream' for 'IO.stdin'.
stdin :: InputStream ByteString
stdin = unsafePerformIO (handleToInputStream IO.stdin >>= lockingInputStream)
{-# NOINLINE stdin #-}


------------------------------------------------------------------------------
-- | An 'OutputStream' for 'IO.stdout'.
stdout :: OutputStream ByteString
stdout = unsafePerformIO (handleToOutputStream IO.stdout >>=
                          lockingOutputStream)
{-# NOINLINE stdout #-}


------------------------------------------------------------------------------
-- | An 'OutputStream' for 'IO.stderr'.
stderr :: OutputStream ByteString
stderr = unsafePerformIO (handleToOutputStream IO.stderr >>=
                          lockingOutputStream)
{-# NOINLINE stderr #-}
