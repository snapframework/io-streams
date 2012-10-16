{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Buffering for output streams based on @blaze-builder@.
--
-- Buffering an output stream can often improve throughput by reducing the
-- number of system calls made through the file descriptor. The @blaze-builder@
-- package provides an efficient set of primitives for serializing values
-- directly to an output buffer.
--
-- /Using this module/
--
-- Given an 'OutputStream' taking 'ByteString':
--
-- > someOutputStream :: OutputStream ByteString
--
-- You create a new output stream wrapping the original one that accepts
-- 'Builder' values:
--
--
-- @
-- do
--     newStream <- Streams.'builderStream' someOutputStream
--     Streams.'write' ('Blaze.ByteString.Builder.fromByteString' \"hello\") newStream
--     ....
-- @
--
--
-- You can flush the output buffer using 'Blaze.ByteString.Builder.flush':
--
-- @
--     ....
--     Streams.'write' 'Blaze.ByteString.Builder.flush' newStream
--     ....
-- @
--
-- As a convention, 'builderStream' will write the empty string to the wrapped
-- 'OutputStream' upon a builder buffer flush. Output streams which receive
-- 'ByteString' should either ignore the empty string or interpret it as a
-- signal to flush their own buffers, as the "System.IO.Streams.Zlib" functions
-- do.
--
-- /Example/
--
-- > example :: IO [ByteString]
-- > example = do
-- >     let l1 = intersperse " " ["the", "quick", "brown", "fox"]
-- >     let l2 = intersperse " " ["jumped", "over", "the"]
-- >     let l  = map fromByteString l1 ++ [flush] ++ map fromByteString l2
-- >
-- >     is          <- Streams.fromList l
-- >     (os0, grab) <- Streams.listOutputStream
-- >     os          <- Streams.builderStream os0
-- >
-- >     connect is os >> grab
--
-- > ghci> example
-- > ["the quick brown fox","","jumped over the"]
--
module System.IO.Streams.Blaze
 ( -- * Blaze builder conversion
   builderStream
 , unsafeBuilderStream
 , builderStreamWith
 ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal
                   ( defaultBufferSize )
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal.Types
                   ( Builder(..)
                   , BuildSignal(..)
                   , BufRange(..)
                   , buildStep
                   )
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal.Buffer
                   ( Buffer
                   , BufferAllocStrategy
                   , allNewBuffersStrategy
                   , execBuildStep
                   , reuseBufferStrategy
                   , unsafeFreezeBuffer
                   , unsafeFreezeNonEmptyBuffer
                   , updateEndOfSlice
                   )
------------------------------------------------------------------------------
import           Control.Monad         (when)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
------------------------------------------------------------------------------
import           System.IO.Streams.Internal
                   ( OutputStream
                   , Sink(..)
                   , nullSink
                   , sinkToStream
                   , write
                   )


------------------------------------------------------------------------------
-- | Converts a 'ByteString' sink into a 'Builder' sink.
builderStream :: OutputStream ByteString -> IO (OutputStream Builder)
builderStream = builderStreamWith (allNewBuffersStrategy defaultBufferSize)
{- TODO: Perhaps the equivalent generator version might be more intuitive for
         some users:

    InputStream Builder -> IO (InputStream ByteString) -}


------------------------------------------------------------------------------
-- | Unsafe variation on 'builderStream' that reuses an existing buffer for
-- efficiency.
--
-- /NOTE/: because the buffer is reused, subsequent 'ByteString' values written
-- to the wrapped 'OutputString' will cause previous yielded strings to change.
-- Do not retain references to these 'ByteString' values inside the
-- 'OutputStream' you pass to this function, or you will violate referential
-- transparency.
--
-- If you /must/ retain copies of these values, then please use
-- 'Data.ByteString.copy' to ensure that you have a fresh copy of the
-- underlying string.
unsafeBuilderStream :: IO Buffer
                    -> OutputStream ByteString
                    -> IO (OutputStream Builder)
unsafeBuilderStream = builderStreamWith . reuseBufferStrategy


------------------------------------------------------------------------------
-- Note: will not yield empty string unless it wants downstream to flush.
--
-- | A customized version of 'builderStream', using the specified
-- 'BufferAllocStrategy'.
builderStreamWith :: BufferAllocStrategy
                  -> OutputStream ByteString
                  -> IO (OutputStream Builder)
builderStreamWith (ioBuf0, nextBuf) os = do
    sinkToStream (sink ioBuf0)
  where
    sink ioBuf = Sink $ maybe eof chunk
      where
        eof = do
            buf <- ioBuf
            case unsafeFreezeNonEmptyBuffer buf of
              Nothing    -> write Nothing os
              x@(Just s) -> do
                  when (not $ S.null s) $ write x os
                  write Nothing os

            return nullSink

        chunk c = feed (unBuilder c (buildStep finalStep)) ioBuf


    finalStep !(BufRange pf _) = return $! Done pf $! ()

    feed bStep ioBuf = do
        !buf   <- ioBuf
        signal <- execBuildStep bStep buf

        case signal of
          Done op' _ -> return $ sink (return (updateEndOfSlice buf op'))

          BufferFull minSize op' bStep' -> do
              let buf' = updateEndOfSlice buf op'
                  {-# INLINE cont #-}
                  cont = do
                      ioBuf' <- nextBuf minSize buf'
                      feed bStep' ioBuf'

              write (Just $! unsafeFreezeBuffer buf') os
              cont

          InsertByteString op' bs bStep' -> do
              let buf' = updateEndOfSlice buf op'

              case unsafeFreezeNonEmptyBuffer buf' of
                Nothing -> return $! ()
                x       -> write x os

              -- empty string here notifies downstream of flush
              write (Just bs) os

              ioBuf' <- nextBuf 1 buf'
              feed bStep' ioBuf'
