{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Combinators (tests) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad hiding (filterM, mapM, mapM_)
import           Data.IORef
import           Data.List
import           Prelude hiding (mapM, mapM_, read)
import           System.IO.Streams
import qualified System.IO.Streams as S
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testFilterM
        , testFoldMWorksTwice
        , testMap
        , testContramap
        , testMapM
        , testMapM_
        , testContramapM_
        , testSkipToEof
        , testZipM
        , testUnzipM
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

    read is >>= assertEqual "eof" Nothing

  where
    f a b = return (a+b)


------------------------------------------------------------------------------
testMapM :: Test
testMapM = testCase "combinators/mapM" $ do
    is <- fromList [1,2,3::Int] >>= mapM (return . (1+))
    l  <- toList is

    assertEqual "mapM" [2,3,4] l


------------------------------------------------------------------------------
testMap :: Test
testMap = testCase "combinators/map" $ do
    is <- fromList [1,2,3::Int] >>= S.map (1+)
    l  <- toList is

    assertEqual "map" [2,3,4] l


------------------------------------------------------------------------------
testContramap :: Test
testContramap = testCase "combinators/contramap" $ do
    is  <- fromList [1,2,3::Int]
    l   <- outputToList (contramap (+1) >=> connect is)
    assertEqual "contramap" [2,3,4] l


------------------------------------------------------------------------------
testMapM_ :: Test
testMapM_ = testCase "combinators/mapM_" $ do
    ref <- newIORef 0
    is  <- fromList [1,2,3::Int] >>= mapM_ (modifyIORef ref . (+))
    _   <- toList is

    readIORef ref >>= assertEqual "mapM_" 6


------------------------------------------------------------------------------
testContramapM_ :: Test
testContramapM_ = testCase "combinators/contramapM_" $ do
    ref <- newIORef 0
    is  <- fromList [1,2,3::Int]
    _   <- outputToList (contramapM_ (modifyIORef ref . (+)) >=> connect is)

    readIORef ref >>= assertEqual "contramapM_" 6


------------------------------------------------------------------------------
testSkipToEof :: Test
testSkipToEof = testCase "combinators/skipToEof" $ do
    is <- fromList [1,2,3::Int]
    !_ <- skipToEof is
    x <- read is

    assertEqual "skipToEof" Nothing x


------------------------------------------------------------------------------
testFilterM :: Test
testFilterM = testCase "list/filterM" $ do
    is  <- fromList [1..10::Int]
    is' <- filterM (return . even) is

    read is' >>= assertEqual "read1" (Just 2)
    unRead 3 is'

    peek is >>= assertEqual "pushback" (Just 3)
    toList is' >>= assertEqual "rest" [4,6..10]

    unRead 20 is'

    peek is >>= assertEqual "pushback2" (Just 20)
    toList is' >>= assertEqual "rest2" [20]
    toList is' >>= assertEqual "eof" []


------------------------------------------------------------------------------
testZipM :: Test
testZipM = testCase "list/zipM" $ do
    let l1 = [1 .. 10 :: Int]
    let l2 = [2 .. 10 :: Int]

    (join $ zipM <$> fromList l1 <*> fromList l2) >>= toList
        >>= assertEqual "zip1" (l1 `zip` l2)

    (join $ zipM <$> fromList l2 <*> fromList l1) >>= toList
        >>= assertEqual "zip2" (l2 `zip` l1)

    is1   <- fromList l1
    is2   <- fromList l2
    isZip <- zipM is1 is2

    _     <- toList isZip
    read is1 >>= assertEqual "remainder" (Just 10)


------------------------------------------------------------------------------
testUnzipM :: Test
testUnzipM = testCase "list/unzipM" $ do
    let l1 = [1 .. 10 :: Int]
        l2 = [2 .. 10 :: Int]
        l  = l1 `zip` l2

    (is1, is2) <- fromList l >>= unzipM
    toList is1 >>= assertEqual "unzip1-a" (fst $ unzip l)
    toList is2 >>= assertEqual "unzip1-b" (snd $ unzip l)
    read is1 >>= assertEqual "unzip1-read-a" Nothing
    read is2 >>= assertEqual "unzip1-read-b" Nothing

    (is3, is4) <- fromList l >>= unzipM
    toList is4 >>= assertEqual "unzip2-b" (snd $ unzip l)
    toList is3 >>= assertEqual "unzip2-a" (fst $ unzip l)
    read is4 >>= assertEqual "unzip2-read-b" Nothing
    read is3 >>= assertEqual "unzip2-read" Nothing

