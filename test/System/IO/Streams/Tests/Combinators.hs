{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Combinators (tests) where

------------------------------------------------------------------------------
import           Control.Monad hiding (mapM)
import           Data.List
import           Prelude hiding (mapM, read)
import           System.IO.Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testFoldMWorksTwice
        , testMapM
        , testSkipToEof
        ]


------------------------------------------------------------------------------
testFoldMWorksTwice :: Test
testFoldMWorksTwice = testCase "combinators/foldMWorksTwice" $ do
    (os, grab) <- nullOutput >>= outputFoldM f (0::Int)

    let l = [1,2,3]
    fromList l >>= supplyTo os
    m <- grab
    assertEqual "foldm1" (sum l) m

    let l2 = [4,5,6]
    fromList l2 >>= supplyTo os
    m2 <- grab
    assertEqual "foldm2" (sum l2) m2

    (is, grab2) <- fromList l >>= inputFoldM f (0::Int)
    _ <- read is
    x <- grab2
    assertEqual "foldm3" 1 x

    _ <- read is >> read is
    y <- grab2
    assertEqual "foldm4" 5 y

  where
    f a b = return (a+b)


------------------------------------------------------------------------------
testMapM :: Test
testMapM = testCase "combinators/mapM" $ do
    is <- fromList [1,2,3::Int] >>= mapM (return . (1+))
    l  <- toList is

    assertEqual "mapM" [2,3,4] l


------------------------------------------------------------------------------
testSkipToEof :: Test
testSkipToEof = testCase "combinators/skipToEof" $ do
    is <- fromList [1,2,3::Int]
    !_ <- skipToEof is
    x <- read is

    assertEqual "skipToEof" Nothing x
