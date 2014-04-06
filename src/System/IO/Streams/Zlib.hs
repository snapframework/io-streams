-- | Interface to @zlib@ and @gzip@ compression for 'Bytestring's and 'Builder's

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.IO.Streams.Zlib
 ( -- * ByteString decompression
   gunzip
 , decompress
   -- * ByteString compression
 , gzip
 , compress
   -- * Builder compression
 , gzipBuilder
 , compressBuilder
   -- * Compression level
 , CompressionLevel(..)
 , defaultCompressionLevel
 ) where

------------------------------------------------------------------------------
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString                          as S
import           Data.IORef                               (newIORef, readIORef, writeIORef)
import           Prelude                                  hiding (read)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (fromByteString)
import           Blaze.ByteString.Builder.Internal        (Builder, defaultBufferSize, flush)
import           Blaze.ByteString.Builder.Internal.Buffer (allocBuffer)
import           Codec.Zlib                               (Deflate, Inflate, Popper, WindowBits (..), feedDeflate, feedInflate, finishDeflate, finishInflate, flushDeflate, flushInflate, initDeflate, initInflate)
------------------------------------------------------------------------------
import           System.IO.Streams.Builder                (unsafeBuilderStream)
import           System.IO.Streams.Internal               (InputStream, OutputStream, makeInputStream, makeOutputStream, read, write)


------------------------------------------------------------------------------
gzipBits :: WindowBits
gzipBits = WindowBits 31


------------------------------------------------------------------------------
compressBits :: WindowBits
compressBits = WindowBits 15


------------------------------------------------------------------------------
-- | Decompress an 'InputStream' of strict 'ByteString's from the @gzip@ format
gunzip :: InputStream ByteString -> IO (InputStream ByteString)
gunzip input = initInflate gzipBits >>= inflate input


------------------------------------------------------------------------------
-- | Decompress an 'InputStream' of strict 'ByteString's from the @zlib@ format
decompress :: InputStream ByteString -> IO (InputStream ByteString)
decompress input = initInflate compressBits >>= inflate input


------------------------------------------------------------------------------
-- Note: bytes pushed back to this input stream are not propagated back to the
-- source InputStream.
data IS = Input
        | Popper Popper
        | Done

inflate :: InputStream ByteString -> Inflate -> IO (InputStream ByteString)
inflate input state = do
    ref <- newIORef Input
    makeInputStream $ stream ref

  where
    stream ref = go
      where
        go = readIORef ref >>= \st ->
             case st of
               Input    -> read input >>= maybe eof chunk
               Popper p -> pop p
               Done     -> return Nothing

        eof = do
            x <- finishInflate state
            writeIORef ref Done
            if (not $ S.null x)
              then return $! Just x
              else return Nothing

        chunk s =
            if S.null s
              then do
                  out <- flushInflate state
                  return $! Just out
              else feedInflate state s >>= \popper -> do
                  writeIORef ref $ Popper popper
                  pop popper

        pop popper = popper >>= maybe backToInput (return . Just)
        backToInput = writeIORef ref Input >> read input >>= maybe eof chunk


------------------------------------------------------------------------------
deflateBuilder :: OutputStream Builder
               -> Deflate
               -> IO (OutputStream Builder)
deflateBuilder stream state = do
    zippedStr <- makeOutputStream bytestringStream >>=
                 \x -> deflate x state

    -- we can use unsafeBuilderStream here because zlib is going to consume the
    -- stream
    unsafeBuilderStream (allocBuffer defaultBufferSize) zippedStr

  where
    bytestringStream x = write (fmap cvt x) stream

    cvt s | S.null s  = flush
          | otherwise = fromByteString s


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' that consumes compressed 'Builder's into an
-- 'OutputStream' that consumes uncompressed 'Builder's in the @gzip@ format
gzipBuilder :: CompressionLevel
            -> OutputStream Builder
            -> IO (OutputStream Builder)
gzipBuilder level output =
    initDeflate (clamp level) gzipBits >>= deflateBuilder output


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' that consumes compressed 'Builder's into an
-- 'OutputStream' that consumes uncompressed 'Builder's in the @zlib@ format
compressBuilder :: CompressionLevel
                -> OutputStream Builder
                -> IO (OutputStream Builder)
compressBuilder level output =
    initDeflate (clamp level) compressBits >>= deflateBuilder output


------------------------------------------------------------------------------
deflate :: OutputStream ByteString
        -> Deflate
        -> IO (OutputStream ByteString)
deflate output state = makeOutputStream stream
  where
    stream Nothing = popAll (finishDeflate state) >> write Nothing output

    stream (Just s) = do
        -- Empty string means flush
        if S.null s
          then do
              popAll (flushDeflate state)
              write (Just S.empty) output

          else feedDeflate state s >>= popAll


    popAll popper = go
      where
        go = popper >>= maybe (return $! ()) (\s -> write (Just s) output >> go)


------------------------------------------------------------------------------
-- | Parameter that defines the tradeoff between speed and compression ratio
newtype CompressionLevel = CompressionLevel Int
  deriving (Read, Eq, Show, Num)


------------------------------------------------------------------------------
-- | A compression level that balances speed with compression ratio
defaultCompressionLevel :: CompressionLevel
defaultCompressionLevel = CompressionLevel 5


------------------------------------------------------------------------------
clamp :: CompressionLevel -> Int
clamp (CompressionLevel x) = min 9 (max x 0)


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' that consumes compressed 'ByteString's into an
-- 'OutputStream' that consumes uncompressed 'ByteString's in the @gzip@ format
gzip :: CompressionLevel
     -> OutputStream ByteString
     -> IO (OutputStream ByteString)
gzip level output = initDeflate (clamp level) gzipBits >>= deflate output


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' that consumes compressed 'ByteString's into an
-- 'OutputStream' that consumes uncompressed 'ByteString's in the @zlib@ format
compress :: CompressionLevel
         -> OutputStream ByteString
         -> IO (OutputStream ByteString)
compress level output = initDeflate (clamp level) compressBits >>=
                        deflate output
