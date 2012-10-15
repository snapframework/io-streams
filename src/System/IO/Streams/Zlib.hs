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
import           Blaze.ByteString.Builder (fromByteString)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal
                   ( Builder
                   , defaultBufferSize
                   , flush
                   )
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal.Buffer ( allocBuffer )
------------------------------------------------------------------------------
import           Codec.Zlib
                   ( Deflate
                   , Inflate
                   , WindowBits(..)
                   , feedInflate
                   , feedDeflate
                   , finishDeflate
                   , finishInflate
                   , flushDeflate
                   , flushInflate
                   , initDeflate
                   , initInflate
                   )
------------------------------------------------------------------------------
import           Control.Monad   (liftM)
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Prelude         hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Blaze (unsafeBuilderStream)
import           System.IO.Streams.Internal
                   ( InputStream
                   , OutputStream
                   , SP(..)
                   , makeOutputStream
                   , nullSource
                   , produce
                   , read
                   , sourceToStream
                   , withDefaultPushback
                   , write
                   )


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
inflate :: InputStream ByteString -> Inflate -> IO (InputStream ByteString)
inflate input state = sourceToStream source
  where
    source  = withDefaultPushback $ read input >>= maybe eof chunk
    eof     = do
        x <- finishInflate state
        if (not $ S.null x)
          then return $! SP nullSource (Just x)
          else return $! SP nullSource Nothing

    chunk s =
        if S.null s
          then do
              out <- liftM Just $ flushInflate state
              return $! SP source out
          else feedInflate state s >>= popAll

    popAll popper = go
      where
        go = popper >>=
             maybe (produce source)
                   (\s -> return $! SP (withDefaultPushback go) (Just s))


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
-- TODO: I don't know a better way to phrase that while not confusing the users.
-- These function would be much more intuitive for users if it transformed
-- 'InputStream's rather than 'OutputStream's

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
        go = popper >>= maybe (return ()) (\s -> write (Just s) output >> go)


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
