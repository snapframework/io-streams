{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Builder (tests) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.ByteString.Builder          (byteString, toLazyByteString)
import           Data.ByteString.Builder.Extra    (flush)
import           Data.ByteString.Builder.Internal (newBuffer)
import qualified Data.ByteString.Char8            as S
import qualified Data.ByteString.Lazy.Char8       as L
import           Data.List
import           Data.Monoid
import           System.IO.Streams                hiding (intersperse, map, take)
import qualified System.IO.Streams                as Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                       hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testBuilderStream
        , testRepeatedConnects
        , testUnsafeBuilderStream
        , testSmallBuffer
        , testSmallBufferWithLargeOutput
        , testNullStream
        ]


------------------------------------------------------------------------------
testBuilderStream :: Test
testBuilderStream = testCase "builder/builderStream" $ do
    let l1 = intersperse " " ["the", "quick", "brown", "fox"]
    let l2 = intersperse " " ["jumped", "over", "the"]
    let l  = map byteString l1 ++ [flush] ++ map byteString l2

    is          <- fromList l
    (os0, grab) <- listOutputStream
    os          <- builderStream os0

    connect is os
    output <- grab
    assertEqual "properly buffered"
                [ "the quick brown fox"
                , ""
                , "jumped over the"
                ]
                output


------------------------------------------------------------------------------
testRepeatedConnects :: Test
testRepeatedConnects = testCase "builder/repeatedConnects" $ do
    (os0, grab)  <- Streams.listOutputStream
    os <- Streams.builderStream os0
    is0 <- Streams.fromList ["Hello, world!\n"]
             >>= Streams.map byteString
    is1 <- Streams.fromList ["Bye, world!\n"]
             >>= Streams.map byteString
    Streams.connect is0 os
    Streams.connect is1 os
    Streams.write Nothing os

    grab >>= assertEqual "repeated connect" ["Hello, world!\n"]


------------------------------------------------------------------------------
testUnsafeBuilderStream :: Test
testUnsafeBuilderStream = testCase "builder/unsafeBuilderStream" $ do
    let l1 = intersperse " " ["the", "quick", "brown", "fox"]
    let l2 = intersperse " " ["jumped", "over", "the"]
    let l  = map byteString l1 ++ [flush] ++ map byteString l2

    is          <- fromList l
    (os0, grab) <- listOutputStream
    os1         <- contramapM (return . S.copy) os0

    os          <- unsafeBuilderStream (newBuffer 1024) os1

    connect is os
    output <- grab
    assertEqual "properly buffered"
                [ "the quick brown fox"
                , ""
                , "jumped over the"
                ]
                output

------------------------------------------------------------------------------
testSmallBuffer :: Test
testSmallBuffer = testCase "builder/smallBuffer" $ do
    (os0, grab) <- listOutputStream
    os          <- builderStreamWithBufferSize 10 os0
    let l1 = intersperse " " ["the", "quick", "brown"]
    let l2 = [" fooooooooooooooooox"]
    let l = map byteString l1 ++ [flush, flush, flush]
              ++ map byteString l2

    is          <- fromList l
    connect is os
    output <- liftM S.concat grab

    assertEqual "short buffer" "the quick brown fooooooooooooooooox" output


------------------------------------------------------------------------------
testSmallBufferWithLargeOutput :: Test
testSmallBufferWithLargeOutput =
    testCase "builder/smallBufferWithLargeOutput" $ do
        (os0, grab) <- listOutputStream
        os1         <- contramapM (return . S.copy) os0
        os          <- unsafeBuilderStream (newBuffer 10) os1

        let l = take 3000 $ cycle $
                replicate 20 (byteString "bloooooooort") ++ [flush]

        is <- fromList l
        let s = S.concat $ L.toChunks $ toLazyByteString $ mconcat l

        connect is os
        output <- liftM S.concat grab

        assertEqual "short buffer 2" s output

        write (Just $ byteString "ok") os
        write Nothing os

        fout <- grab

        -- no output should be sent because of nullSink
        assertEqual "nullSink" [] fout


------------------------------------------------------------------------------
testNullStream :: Test
testNullStream = testCase "builder/nullStream" $ do
    (os0, grab) <- listOutputStream
    os <- builderStream os0

    is <- fromList []
    connect is os

    l <- grab
    assertEqual "null stream" [] l
