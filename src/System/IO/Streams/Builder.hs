{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Buffering for output streams based on bytestring builders.
--
-- Buffering an output stream can often improve throughput by reducing the
-- number of system calls made through the file descriptor. The @blaze-builder@
-- package provides an efficient set of primitives for serializing values
-- directly to an output buffer.
--
-- (/N.B./: most of the @blaze-builder@ package has been moved into
-- @bytestring@ in versions \>= 0.10; once two or three Haskell Platform
-- editions have been released that contain @bytestring@ 0.10 or higher, the
-- dependency on @blaze-builder@ will be dropped in favor of the native support
-- for 'Builder' contained in the @bytestring@ package.)
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
--     Streams.'write' ('Just' $ 'Blaze.ByteString.Builder.fromByteString' \"hello\") newStream
--     ....
-- @
--
--
-- You can flush the output buffer using 'Blaze.ByteString.Builder.flush':
--
-- @
--     ....
--     Streams.'write' ('Just' 'Blaze.ByteString.Builder.flush') newStream
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
-- @
-- example :: IO [ByteString]
-- example = do
--     let l1 = 'Data.List.intersperse' \" \" [\"the\", \"quick\", \"brown\", \"fox\"]
--     let l2 = 'Data.List.intersperse' \" \" [\"jumped\", \"over\", \"the\"]
--     let l  = map 'Blaze.ByteString.Builder.fromByteString' l1 ++ ['Blaze.ByteString.Builder.flush'] ++ map 'Blaze.ByteString.Builder.fromByteString' l2
--     is          \<- Streams.'System.IO.Streams.fromList' l
--     (os0, grab) \<- Streams.'System.IO.Streams.listOutputStream'
--     os          \<- Streams.'builderStream' os0
--     Streams.'System.IO.Streams.connect' is os >> grab
--
-- ghci> example
-- [\"the quick brown fox\",\"\",\"jumped over the\"]
-- @
--
module System.IO.Streams.Builder
 ( -- * Blaze builder conversion
   builderStream
 , unsafeBuilderStream
 , builderStreamWith
 ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal        (defaultBufferSize)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal.Types  (BufRange (..),
                                                           BuildSignal (..),
                                                           Builder (..),
                                                           buildStep)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal.Buffer (Buffer, BufferAllocStrategy, allNewBuffersStrategy,
                                                           execBuildStep, reuseBufferStrategy,
                                                           unsafeFreezeBuffer, unsafeFreezeNonEmptyBuffer,
                                                           updateEndOfSlice)
------------------------------------------------------------------------------
import           Control.Monad                            (when)
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.ByteString.Char8                    as S
------------------------------------------------------------------------------
import           System.IO.Streams.Internal               (OutputStream,
                                                           Sink (..),
                                                           nullSink,
                                                           sinkToStream,
                                                           write)


------------------------------------------------------------------------------
-- | Converts a 'ByteString' sink into a 'Builder' sink.
--
-- Note that if the generated builder receives a
-- 'Blaze.ByteString.Builder.flush', by convention it will send an empty string
-- to the supplied @'OutputStream' 'ByteString'@ to indicate that any output
-- buffers are to be flushed.
--
builderStream :: OutputStream ByteString -> IO (OutputStream Builder)
builderStream = builderStreamWith (allNewBuffersStrategy defaultBufferSize)


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
--
-- You can create a Buffer with
-- 'Blaze.ByteString.Builder.Internal.Buffer.allocBuffer'.
--
--
unsafeBuilderStream :: IO Buffer
                    -> OutputStream ByteString
                    -> IO (OutputStream Builder)
unsafeBuilderStream = builderStreamWith . reuseBufferStrategy


------------------------------------------------------------------------------
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
