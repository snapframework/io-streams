{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Buffering for output streams based on bytestring builders.
--
-- Buffering an output stream can often improve throughput by reducing the
-- number of system calls made through the file descriptor. The @bytestring@
-- package provides an efficient monoidal datatype used for serializing values
-- directly to an output buffer, called a 'Builder', originally implemented in
-- the @blaze-builder@ package by Simon Meier. When compiling with @bytestring@
-- versions older than 0.10.4, (i.e. GHC <= 7.6) users must depend on the
-- @bytestring-builder@ library to get the new builder implementation. Since we
-- try to maintain compatibility with the last three GHC versions, the
-- dependency on @bytestring-builder@ can be dropped after the release of GHC
-- 7.12.
--
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
--     Streams.'write' ('Just' $ 'Data.ByteString.Builder.byteString' \"hello\") newStream
--     ....
-- @
--
--
-- You can flush the output buffer using 'Data.ByteString.Builder.Extra.flush':
--
-- @
--     ....
--     Streams.'write' ('Just' 'Data.ByteString.Builder.Extra.flush') newStream
--     ....
-- @
--
-- As a convention, 'builderStream' will write the empty string to the wrapped
-- 'OutputStream' upon a builder buffer flush. Output streams which receive
-- 'ByteString' should either ignore the empty string or interpret it as a
-- signal to flush their own buffers, as the @handleToOutputStream@ and
-- "System.IO.Streams.Zlib" functions do.
--
-- /Example/
--
-- @
-- example :: IO [ByteString]
-- example = do
--     let l1 = 'Data.List.intersperse' \" \" [\"the\", \"quick\", \"brown\", \"fox\"]
--     let l2 = 'Data.List.intersperse' \" \" [\"jumped\", \"over\", \"the\"]
--     let l  = map 'Data.ByteString.Builder.byteString' l1 ++ ['Data.ByteString.Builder.Extra.flush'] ++ map 'Data.ByteString.Builder.byteString' l2
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
 , builderStreamWithBufferSize
 , unsafeBuilderStream
 ) where

------------------------------------------------------------------------------
import           Control.Monad                    (when)
import           Data.ByteString.Builder.Internal (Buffer (..), BufferRange (..), Builder (Builder), byteStringFromBuffer, defaultChunkSize, fillWithBuildStep, newBuffer, runBuilder)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import           Data.IORef                       (newIORef, readIORef, writeIORef)

------------------------------------------------------------------------------
import           System.IO.Streams.Internal       (OutputStream, makeOutputStream, write, writeTo)


------------------------------------------------------------------------------
builderStreamWithBufferFunc :: IO Buffer
                            -> OutputStream ByteString
                            -> IO (OutputStream Builder)
builderStreamWithBufferFunc mkNewBuf os = do
    ref <- newIORef Nothing
    makeOutputStream $ chunk ref
  where
    chunk ref Nothing = do
        mbuf <- readIORef ref
        case mbuf of
          -- If we existing buffer leftovers, write them to the output.
          Nothing  -> return $! ()
          Just buf -> writeBuf buf
        write Nothing os
    chunk ref (Just builder) = runStep ref $ runBuilder builder

    getBuf ref = readIORef ref >>= maybe mkNewBuf return

    bumpBuf (Buffer fp (BufferRange !_ endBuf)) endPtr =
        Buffer fp (BufferRange endPtr endBuf)

    updateBuf ref buf endPtr = writeIORef ref $! Just $! bumpBuf buf endPtr

    writeBuf buf = do
        let bs = byteStringFromBuffer buf
        when (not . S.null $ bs) $ writeTo os $! Just bs

    bufRange (Buffer _ rng) = rng

    runStep ref step = do
        buf <- getBuf ref
        fillWithBuildStep step (cDone buf) (cFull buf) (cInsert buf)
                          (bufRange buf)
      where
        cDone buf endPtr !() = updateBuf ref buf endPtr
        cFull buf !endPtr !_ newStep = do
            writeBuf $! bumpBuf buf endPtr
            writeIORef ref Nothing
            runStep ref newStep
        cInsert buf !endPtr !bs newStep = do
            writeBuf $! bumpBuf buf endPtr
            writeIORef ref Nothing
            writeTo os $! Just bs
            runStep ref newStep


------------------------------------------------------------------------------
-- | Converts a 'ByteString' sink into a 'Builder' sink, using the supplied
-- buffer size.
--
-- Note that if the generated builder receives a
-- 'Blaze.ByteString.Builder.flush', by convention it will send an empty string
-- to the supplied @'OutputStream' 'ByteString'@ to indicate that any output
-- buffers are to be flushed.
--
-- /Since: 1.3.0.0./
builderStreamWithBufferSize :: Int -> OutputStream ByteString -> IO (OutputStream Builder)
builderStreamWithBufferSize bufsiz = builderStreamWithBufferFunc (newBuffer bufsiz)


------------------------------------------------------------------------------
-- | Converts a 'ByteString' sink into a 'Builder' sink.
--
-- Note that if the generated builder receives a
-- 'Blaze.ByteString.Builder.flush', by convention it will send an empty string
-- to the supplied @'OutputStream' 'ByteString'@ to indicate that any output
-- buffers are to be flushed.
--
builderStream :: OutputStream ByteString -> IO (OutputStream Builder)
builderStream = builderStreamWithBufferSize defaultChunkSize


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
-- You can create a Buffer with 'Data.ByteString.Builder.Internal.newBuffer'.
--
unsafeBuilderStream :: IO Buffer
                    -> OutputStream ByteString
                    -> IO (OutputStream Builder)
unsafeBuilderStream mkBuf os = do
    buf <- mkBuf
    builderStreamWithBufferFunc (return buf) os
