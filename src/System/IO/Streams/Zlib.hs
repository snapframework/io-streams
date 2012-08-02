{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.IO.Streams.Zlib
 ( gunzip
 , gzip
 , gzipBuilder
 , compress
 , compressBuilder
 , decompress
 , CompressionLevel(..)
 , defaultCompressionLevel
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
import           Prelude                    hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Blaze
import           System.IO.Streams.Internal


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
gzipBuilder :: CompressionLevel
            -> OutputStream Builder
            -> IO (OutputStream Builder)
gzipBuilder level output =
    initDeflate (clamp level) gzipBits >>= deflateBuilder output


------------------------------------------------------------------------------
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
    stream Nothing = do
        m <- maybe Nothing maybeNonempty <$>
             finishDeflate state

        maybe (write Nothing output)
              (const $ write m output >> write Nothing output)
              m

    stream (Just s) = do
        -- Empty string means flush
        if S.null s
          then do
              flushDeflate state >>= maybe (return ()) (flip write output . Just)
              write (Just S.empty) output

          else join (feedDeflate state s) >>=
               maybe (return ())
                     (flip write output . Just)


------------------------------------------------------------------------------
newtype CompressionLevel = CompressionLevel Int
  deriving (Read, Eq, Show, Num)


------------------------------------------------------------------------------
defaultCompressionLevel :: CompressionLevel
defaultCompressionLevel = CompressionLevel 5


------------------------------------------------------------------------------
clamp :: CompressionLevel -> Int
clamp (CompressionLevel x) = min 9 (max x 0)


------------------------------------------------------------------------------
gzip :: CompressionLevel
     -> OutputStream ByteString
     -> IO (OutputStream ByteString)
gzip level output = initDeflate (clamp level) gzipBits >>= deflate output


------------------------------------------------------------------------------
compress :: CompressionLevel
         -> OutputStream ByteString
         -> IO (OutputStream ByteString)
compress level output = initDeflate (clamp level) compressBits >>=
                        deflate output
