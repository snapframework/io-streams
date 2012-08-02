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
        , testBigString
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
testIdCompress = testProperty "zlib/idCompress" $ monadicIO $
                 forAllM arbitrary prop
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
propId name inf def infStr defStr l0 = do
    pre (not (null l0) && L.length (L.fromChunks l0) > 0)
    liftQ $ do
        let l   = L.fromChunks $ l0 ++
                  [ S.concat $ L.toChunks $ L.take 32000 $ L.fromChunks $
                    cycle l0 ]
        let inp = def l

        is          <- fromList (L.toChunks inp) >>= infStr
        (os0, grab) <- listOutputStream
        os          <- defStr defaultCompressionLevel os0

        connect is os
        outp        <- liftM L.fromChunks grab

        assertEqual name l (inf outp)



------------------------------------------------------------------------------
testBigString :: Test
testBigString = testCase "zlib/bigString" $ do
    let l = S.concat $ L.toChunks $ L.take 640000 $ L.fromChunks $ cycle
            [ "lfkdsjflkdshflkjdhsfkljhdslkfhdslakjfhlkdsjhflkjdsahflkjhsa"
            , "39287647893264987368947632198746328974698327649873216498713"
            , "bznmbxz879hJKHYG^&%^&^%*&^%*&^%*&^%&*^%&*65tykjhdgbmdnvkjch"
            , "VBUYDUHKJC*(HJKDHLCJBUYEOUIHJCHUOY&*^(*)@HJDNM<NCKJHVUKHLKJ"
            , "())))))A(JXNZMWJ#*&^&YIUHDHKJHKJLNCnkfdhkfy32huukhksnc8310s"
            , "|||||A!@2iuozukx|djklu8udjlm3n3888****8ijlfkdjvnchiuyuis';'"
            , "56%^%^%^%^4dnmcnjjk3NJKNKJCiejnhfj[]][}{[][]f[][]d]299ikkjn"
            , "fjlkdjklfozi898888****2jkmdc,x.//,.,/asl;oi39uijdp'''qwjkjh"
            ]

    (os0, grab) <- listOutputStream
    os <- gzip 5 os0

    fromList [l] >>= connectTo os

    out <- liftM L.fromChunks grab



    let o1 = L.fromChunks [l]
    let o2 = GZ.decompress out

    when (o1 /= o2) $ do
        putStrLn "o1 /= o2"
        putStrLn $ "o1 = " ++ (show $ S.concat $ L.toChunks $ L.take 1000 o1)
                     ++ "..."
        putStrLn $ "o2 = " ++ (show $ S.concat $ L.toChunks $ L.take 1000 o2)
                     ++ "..."

        putStrLn $ "len(o1)=" ++ show (L.length o1)
        putStrLn $ "len(o2)=" ++ show (L.length o2)

    assertBool "bigString1" $ o1 == o2

    is2 <- fromList ([""] ++ L.toChunks out ++ [""]) >>= gunzip
    (os1, grab') <- listOutputStream
    connect is2 os1
    out' <- liftM L.fromChunks grab'

    assertBool "bigString2" $ o1 == out'



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
        t 7 [ fromByteString a, flush, flush, fromByteString b
            , flush, flush ]

        t 4 [ fromByteString a, flush, flush, fromByteString b ]

  where
    t expected input = do
        (os0, grab) <- listOutputStream
        os          <- builderStream os0 >>= comp defaultCompressionLevel

        fromList input >>= connectTo os
        xs <- grab

        when (length xs /= expected) $ putStrLn $ "xs is " ++ show xs
        assertEqual (name ++ "/len") expected (length xs)

        let outp = inf $ L.fromChunks xs

        assertEqual (name ++ "/eq") (L.fromChunks [a,b]) outp


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "zlib/trivials" $ do
    let cl = CompressionLevel 4
    coverReadInstance cl
    coverShowInstance cl
    coverEqInstance   cl
