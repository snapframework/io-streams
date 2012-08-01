{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.ByteString (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import           Data.Monoid
import           Prelude hiding (read)
import           System.IO.Streams
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.QuickCheck hiding (output)
import           Test.QuickCheck.Monadic
import           Test.HUnit hiding (Test)
import           System.IO.Streams.Tests.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testCountInput
        , testCountOutput
        , testReadNoMoreThan
        , testReadNoMoreThan2
        , testReadNoMoreThan3
        , testTakeNoMoreThan
        , testTakeNoMoreThan2
        , testWriteNoMoreThan
        , testKillIfTooSlow
        , testTrivials
        ]


------------------------------------------------------------------------------
testCountInput :: Test
testCountInput = testProperty "bytestring/countInput" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: [ByteString] -> PropertyM IO ()
    prop l = liftQ $ do
        is          <- fromList l
        (is', grab) <- countInput is

        x <- toList is'
        n <- grab

        assertEqual "countInput1" (L.length $ L.fromChunks l) n
        assertEqual "countInput2" (L.length $ L.fromChunks x) n


------------------------------------------------------------------------------
testCountOutput :: Test
testCountOutput = testProperty "bytestring/countOutput" $
                  monadicIO $
                  forAllM arbitrary prop
  where
    prop :: [ByteString] -> PropertyM IO ()
    prop l = liftQ $ do
        is            <- fromList l
        (os0, grab)   <- listOutputStream
        (os, grabLen) <- countOutput os0

        connect is os

        xs <- grab
        n  <- grabLen

        assertEqual "countOutput1" l xs
        assertEqual "countOutput2" (L.length $ L.fromChunks l) n


------------------------------------------------------------------------------
testReadNoMoreThan :: Test
testReadNoMoreThan = testProperty "bytestring/readNoMoreThan" $
                     monadicIO $
                     forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop l = pre (L.length l > 5) >> liftQ (do
        let (a,b) = L.splitAt 4 l

        is  <- fromList (L.toChunks l)
        is' <- readNoMoreThan 4 is

        x   <- liftM L.fromChunks $ toList is'
        y   <- liftM L.fromChunks $ toList is

        assertEqual "readNoMoreThan1" a x
        assertEqual "readNoMoreThan2" b y
        )


------------------------------------------------------------------------------
testReadNoMoreThan2 :: Test
testReadNoMoreThan2 = testProperty "bytestring/readNoMoreThan2" $
                      monadicIO $
                      forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop l = liftQ $ do
        is  <- fromList (L.toChunks l)
        is' <- readNoMoreThan 0 is

        x   <- toList is'
        y   <- liftM L.fromChunks $ toList is

        assertEqual "readNoMoreThan3" [] x
        assertEqual "readNoMoreThan4" l y


------------------------------------------------------------------------------
testReadNoMoreThan3 :: Test
testReadNoMoreThan3 = testCase "bytestring/readNoMoreThan3" $ do
    is <- fromList ["The", "quick", "brown", "fox"::ByteString] >>=
          readNoMoreThan 100

    _  <- toList is
    m  <- read is

    assertEqual "readNoMoreThan3" Nothing m


------------------------------------------------------------------------------
testTakeNoMoreThan :: Test
testTakeNoMoreThan = testProperty "bytestring/takeNoMoreThan" $
                     monadicIO $
                     forAllM arbitrary prop

  where
    prop :: L.ByteString -> PropertyM IO ()
    prop l = do
        pre (L.length l > 5)

        liftQ $ do
            is  <- fromList $ L.toChunks l
            is' <- takeNoMoreThan 4 is
            expectExceptionH $ toList is'


------------------------------------------------------------------------------
testTakeNoMoreThan2 :: Test
testTakeNoMoreThan2 = testProperty "bytestring/takeNoMoreThan2" $
                      monadicIO $
                      forAllM arbitrary prop

  where
    prop :: L.ByteString -> PropertyM IO ()
    prop l = do
        let n = L.length l

        liftQ $ do
            is  <- fromList $ L.toChunks l
            is' <- takeNoMoreThan (n + 1) is
            l'  <- liftM L.fromChunks $ toList is'
            assertEqual "takeNoMoreThan2" l l'

            m   <- read is'
            assertEqual "takeNoMoreThan2-2" Nothing m


------------------------------------------------------------------------------
testWriteNoMoreThan :: Test
testWriteNoMoreThan = testProperty "bytestring/writeNoMoreThan" $
                      monadicIO $
                      forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop l = do
        pre (L.length l > 5)
        let a = L.take 4 l

        liftQ $ do
            is         <- fromList (L.toChunks l)
            (os, grab) <- listOutputStream
            os'        <- writeNoMoreThan 4 os

            connect is os'
            write Nothing os'

            x <- liftM L.fromChunks grab

            assertEqual "writeNoMoreThan1" a x

        liftQ $ do
            is <- fromList $ L.toChunks a
            (os, grab) <- listOutputStream
            os'        <- writeNoMoreThan 10 os

            connect is os'
            write Nothing os'
            x <- liftM L.fromChunks grab
            assertEqual "writeNoMoreThan2" a x


------------------------------------------------------------------------------
testKillIfTooSlow :: Test
testKillIfTooSlow = testCase "bytestring/killIfTooSlow" $ do
    is <- mkList
    expectExceptionH $ trickleFrom is

    is' <- mkList
    void $ toList is'
    x <- read is'

    assertEqual "killIfTooSlow" Nothing x

  where
    mkList = (fromList $ take 100 $ cycle $
              intersperse " " ["the", "quick", "brown", "fox"]) >>=
             killIfTooSlow (return ()) 10 2

    trickleFrom is = go
      where
        go = read is >>= maybe (return ())
                               (\x -> x `seq` (threadDelay 2000000 >> go))


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "bytestring/testTrivials" $ do
    coverTypeableInstance (undefined :: TooManyBytesReadException)
    coverShowInstance     (undefined :: TooManyBytesReadException)
    coverTypeableInstance (undefined :: RateTooSlowException)
    coverShowInstance     (undefined :: RateTooSlowException)
