{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Zlib (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib as Z
import           Control.Monad hiding (mapM)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Functor.Contravariant
import           Data.IORef
import           Data.Monoid
import           Prelude hiding (mapM, read)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test)
import           Test.QuickCheck hiding (output)
import           Test.QuickCheck.Monadic
------------------------------------------------------------------------------
import           System.IO.Streams
import           System.IO.Streams.Tests.Common

tests :: [Test]
tests = [ testIdGzip
        , testIdCompress
        , testBuilderFlushGZip
        , testBuilderFlushCompress
        , testTrivials
        ]


------------------------------------------------------------------------------
testIdGzip :: Test
testIdGzip = testProperty "zlib/idGZip" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: [ByteString] -> PropertyM IO ()
    prop l = propId "idGZip" GZ.decompress GZ.compress gunzip gzip l


------------------------------------------------------------------------------
testIdCompress :: Test
testIdCompress = testProperty "zlib/idCompress" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: [ByteString] -> PropertyM IO ()
    prop l = propId "idCompress" Z.decompress Z.compress
                    decompress compress l


------------------------------------------------------------------------------
propId :: String
       -> (L.ByteString -> L.ByteString)
       -> (L.ByteString -> L.ByteString)
       -> (InputStream ByteString -> IO (InputStream ByteString))
       -> (CompressionLevel -> OutputStream ByteString
                            -> IO (OutputStream ByteString))
       -> [ByteString]
       -> PropertyM IO ()
propId name inf def infStr defStr l0 = liftQ $ do
    let l   = L.fromChunks l0
    let inp = def l

    is          <- fromList (L.toChunks inp) >>= infStr
    (os0, grab) <- listOutputStream
    os          <- defStr defaultCompressionLevel os0

    connect is os
    outp <- liftM L.fromChunks grab

    assertEqual name l (inf outp)


------------------------------------------------------------------------------
testBuilderFlushGZip :: Test
testBuilderFlushGZip = testProperty "zlib/builderFlushGZip" $ monadicIO $
                       forAllM arbitrary prop
  where
    prop :: (ByteString, ByteString) -> PropertyM IO ()
    prop (a,b) = propBuilderFlush "gzip" GZ.decompress gzipBuilder a b


------------------------------------------------------------------------------
testBuilderFlushCompress :: Test
testBuilderFlushCompress = testProperty "zlib/builderFlushCompress" $
                           monadicIO $ forAllM arbitrary prop
  where
    prop :: (ByteString, ByteString) -> PropertyM IO ()
    prop (a,b) = propBuilderFlush "zlib" Z.decompress compressBuilder a b


------------------------------------------------------------------------------
propBuilderFlush :: String
                 -> (L.ByteString -> L.ByteString)
                 -> (CompressionLevel
                     -> OutputStream Builder -> IO (OutputStream Builder))
                 -> ByteString
                 -> ByteString
                 -> PropertyM IO ()
propBuilderFlush name inf comp a b = do
    pre (not (S.null a) && not (S.null b))
    liftQ $ do
        let input = [ fromByteString a, flush, fromByteString b ]

        (os0, grab) <- listOutputStream
        os          <- builderStream os0 >>= comp defaultCompressionLevel

        fromList input >>= connectTo os
        xs <- grab

        assertEqual (name ++ "/len") 3 (length xs)
        assertEqual (name ++ "/midflush") "" (xs !! 1)

        let outp = inf $ L.fromChunks xs

        assertEqual (name ++ "/eq") (L.fromChunks [a,b]) outp


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "zlib/trivials" $ do
    let cl = CompressionLevel 4
    coverReadInstance cl
    coverShowInstance cl
    coverEqInstance   cl
