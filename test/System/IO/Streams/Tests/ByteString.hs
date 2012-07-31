{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.ByteString (tests) where

------------------------------------------------------------------------------
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
    prop l = prop1 l >> prop2 l

    prop1 l = pre (L.length l > 5) >> liftQ (do
        let (a,b) = L.splitAt 4 l

        is  <- fromList (L.toChunks l)
        is' <- readNoMoreThan 4 is

        x   <- liftM L.fromChunks $ toList is'
        y   <- liftM L.fromChunks $ toList is

        assertEqual "readNoMoreThan1" a x
        assertEqual "readNoMoreThan2" b y
        )

    prop2 l = liftQ $ do
        is  <- fromList (L.toChunks l)
        is' <- readNoMoreThan 0 is

        x   <- toList is'
        y   <- liftM L.fromChunks $ toList is

        assertEqual "readNoMoreThan3" [] x
        assertEqual "readNoMoreThan4" l y


------------------------------------------------------------------------------
testReadNoMoreThan2 :: Test
testReadNoMoreThan2 = testCase "bytestring/readNoMoreThan/2" $ do
    is <- fromList ["The", "quick", "brown", "fox"::ByteString] >>=
          readNoMoreThan 100

    _  <- toList is
    m  <- read is

    assertEqual "readNoMoreThan2" Nothing m
