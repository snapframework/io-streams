{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.IO.Streams.Zlib
 ( gunzip
 , gzip
 , gzipBuilder
 , compress
 , compressBuilder
 , decompress
 , BufferMode(..)
 ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Internal
import           Blaze.ByteString.Builder.Internal.Buffer
import           Codec.Zlib
import           Control.Applicative        ((<$>))
import           Control.Monad              (join)
import qualified Data.ByteString            as S
import           Data.ByteString            (ByteString)
import           Data.Monoid
import           Prelude                    hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Blaze
import           System.IO.Streams.Internal


------------------------------------------------------------------------------
data BufferMode = Buffered | Unbuffered
  deriving (Show, Read, Eq, Enum, Ord)


------------------------------------------------------------------------------
gzipBits :: WindowBits
gzipBits = WindowBits 31


------------------------------------------------------------------------------
compressBits :: WindowBits
compressBits = WindowBits 15


------------------------------------------------------------------------------
gunzip :: InputStream ByteString -> IO (InputStream ByteString)
gunzip input = initInflate gzipBits >>= inflate input


------------------------------------------------------------------------------
decompress :: InputStream ByteString -> IO (InputStream ByteString)
decompress input = initInflate compressBits >>= inflate input


------------------------------------------------------------------------------
maybeNonempty :: ByteString -> Maybe ByteString
maybeNonempty s = if S.null s then Nothing else Just s


------------------------------------------------------------------------------
inflate :: InputStream ByteString -> Inflate -> IO (InputStream ByteString)
inflate input state = makeInputStream stream
  where
    stream = maybeNonempty <$> go
      where
        go      = read input >>= maybe (finishInflate state) chunk
        chunk s = join (feedInflate state s) >>= maybe go return


------------------------------------------------------------------------------
deflateBuilder :: BufferMode
               -> OutputStream Builder
               -> Deflate
               -> IO (OutputStream Builder)
deflateBuilder bufferMode stream state = do
    zippedStr <- makeOutputStream bytestringStream >>=
                 \x -> deflate bufferMode x state

    -- we can use unsafeBuilderStream here because zlib is going to consume the
    -- stream
    unsafeBuilderStream (allocBuffer defaultBufferSize) zippedStr

  where
    bytestringStream | bufferMode == Unbuffered = noBufferStream
                     | otherwise                = bufferStream

    noBufferStream x     = write (fmap ((`mappend` flush) . fromByteString) x)
                                 stream
    bufferStream   x     = write (fmap fromByteString x) stream


------------------------------------------------------------------------------
gzipBuilder :: CompressionLevel
            -> BufferMode
            -> OutputStream Builder
            -> IO (OutputStream Builder)
gzipBuilder level bufferMode output =
    initDeflate (clamp level) gzipBits >>= deflateBuilder bufferMode output


------------------------------------------------------------------------------
compressBuilder :: CompressionLevel
                -> BufferMode
                -> OutputStream Builder
                -> IO (OutputStream Builder)
compressBuilder level bufferMode output =
    initDeflate (clamp level) compressBits >>=
    deflateBuilder bufferMode output


------------------------------------------------------------------------------
deflate :: BufferMode
        -> OutputStream ByteString
        -> Deflate
        -> IO (OutputStream ByteString)
deflate bufferMode output state = makeOutputStream stream
  where
    stream Nothing = do
        m <- maybe Nothing maybeNonempty <$>
             finishDeflate state

        maybe (write Nothing output)
              (const $ write m output >> write Nothing output)
              m

    stream (Just s)
        | bufferMode == Buffered = do
              m <- join (feedDeflate state s)
              maybe (return ())
                    (const $ write m output)
                    m

        | otherwise = do
              m <- maybe Nothing maybeNonempty <$> join (feedDeflate state s)
              n <- maybe (flushDeflate state) (return . Just) m

              maybe (return ()) (const $ write n output) n


------------------------------------------------------------------------------
newtype CompressionLevel = CompressionLevel Int
  deriving (Read, Show, Num)


------------------------------------------------------------------------------
clamp :: CompressionLevel -> Int
clamp (CompressionLevel x) = min 9 (max x 0)


------------------------------------------------------------------------------
gzip :: CompressionLevel
     -> BufferMode
     -> OutputStream ByteString
     -> IO (OutputStream ByteString)
gzip level bufferMode output = initDeflate (clamp level) gzipBits >>=
                               deflate bufferMode output


------------------------------------------------------------------------------
compress :: CompressionLevel
         -> BufferMode
         -> OutputStream ByteString
         -> IO (OutputStream ByteString)
compress level bufferMode output = initDeflate (clamp level) compressBits >>=
                                   deflate bufferMode output
