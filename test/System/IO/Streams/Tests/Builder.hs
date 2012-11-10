{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Builder (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Internal.Buffer
import           Control.Monad
import qualified Data.ByteString.Char8 as S
import           Data.List
import           Data.Monoid
import           System.IO.Streams hiding (intersperse, map, take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testBuilderStream
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
    let l  = map fromByteString l1 ++ [flush] ++ map fromByteString l2

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
testUnsafeBuilderStream :: Test
testUnsafeBuilderStream = testCase "builder/unsafeBuilderStream" $ do
    let l1 = intersperse " " ["the", "quick", "brown", "fox"]
    let l2 = intersperse " " ["jumped", "over", "the"]
    let l  = map fromByteString l1 ++ [flush] ++ map fromByteString l2

    is          <- fromList l
    (os0, grab) <- listOutputStream
    os1         <- contramapM (return . S.copy) os0

    os          <- unsafeBuilderStream (allocBuffer 1024) os1

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
    os          <- builderStreamWith (allNewBuffersStrategy 10) os0
    let l1 = intersperse " " ["the", "quick", "brown"]
    let l2 = [" fooooooooooooooooox"]
    let l = map fromByteString l1 ++ [flush, flush, flush]
              ++ map fromByteString l2

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
        os          <- unsafeBuilderStream (allocBuffer 10) os1

        let l = take 3000 $ cycle $
                replicate 20 (fromByteString "bloooooooort") ++ [flush]

        is <- fromList l
        let s = toByteString $ mconcat l

        connect is os
        output <- liftM S.concat grab

        assertEqual "short buffer 2" s output

        write (Just $ fromByteString "ok") os
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
