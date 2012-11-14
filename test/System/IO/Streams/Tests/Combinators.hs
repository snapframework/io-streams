{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module System.IO.Streams.Tests.Combinators (tests) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                        hiding (filterM, mapM,
                                                       mapM_)
import qualified Control.Monad                        as CM
import           Data.IORef
import           Data.List                            hiding (drop, filter,
                                                       take)
import           Prelude                              hiding (drop, filter,
                                                       mapM, mapM_, read, take)
import qualified Prelude
import           System.IO.Streams                    hiding (all, any, maximum,
                                                       minimum)
import qualified System.IO.Streams                    as S
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck                      hiding (output)
import           Test.QuickCheck.Monadic
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testFilter
        , testFilterM
        , testFilterOutput
        , testFilterOutputM
        , testFoldMWorksTwice
        , testFold
        , testFoldM
        , testPredicates
        , testMap
        , testContramap
        , testMapM
        , testMapM_
        , testContramapM_
        , testSkipToEof
        , testZipM
        , testUnzipM
        , testTake
        , testDrop
        , testGive
        , testIgnore
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
testFilter :: Test
testFilter = testCase "combinators/filter" $ do
    is  <- fromList [1..10::Int]
    is' <- filter even is

    read is' >>= assertEqual "read1" (Just 2)
    unRead 3 is'

    peek is >>= assertEqual "pushback" (Just 3)
    toList is' >>= assertEqual "rest" [4,6..10]

    unRead 20 is'

    peek is >>= assertEqual "pushback2" (Just 20)
    toList is' >>= assertEqual "rest2" [20]
    toList is' >>= assertEqual "eof" []


------------------------------------------------------------------------------
testFilterM :: Test
testFilterM = testCase "combinators/filterM" $ do
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
testFilterOutput :: Test
testFilterOutput = testCase "combinators/filterOutput" $ do
    is  <- fromList [1..10::Int]
    l   <- outputToList (\os -> filterOutput even os >>= connect is)
    assertEqual "filterOutput" (Prelude.filter even [1..10]) l


------------------------------------------------------------------------------
testFilterOutputM :: Test
testFilterOutputM = testCase "combinators/filterOutputM" $ do
    is  <- fromList [1..10::Int]
    l   <- outputToList (\os -> filterOutputM (return . even) os >>= connect is)
    assertEqual "filterOutputM" (Prelude.filter even [1..10]) l


------------------------------------------------------------------------------
testFold :: Test
testFold = testCase "combinators/fold" $ do
    fromList [1..10::Int] >>= S.fold (+) 0
                          >>= assertEqual "fold1" (sum [1..10])

------------------------------------------------------------------------------
testFoldM :: Test
testFoldM = testCase "combinators/foldM" $ do
    fromList [1..10::Int] >>= S.foldM ((return .) . (+)) 0
                          >>= assertEqual "fold2" (sum [1..10])


------------------------------------------------------------------------------
data StreamPred = forall c . (Eq c, Show c) =>
                  P ([Int] -> c, InputStream Int -> IO c, String)

testPredicates :: Test
testPredicates = testProperty "combinators/predicates" $ monadicIO $ forAllM arbitrary prop
  where
    predicates :: [StreamPred]
    predicates = [ P (all even   , S.all even , "all"     )
                 , P (any even   , S.any even , "any"     )
                 , P (nl maximum , S.maximum  , "maximum" )
                 , P (nl minimum , S.minimum  , "minimum" )
                 ]

    nl f l = if null l then Nothing else Just (f l)

    prop :: [Int] -> PropertyM IO ()
    prop l = liftQ $ CM.mapM_ (p l) predicates

    p :: [Int] -> StreamPred -> IO ()
    p l (P (pPred, pStream, name)) =
        fromList l >>= pStream >>= assertEqual name (pPred l)


------------------------------------------------------------------------------
testZipM :: Test
testZipM = testCase "combinators/zipM" $ do
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
testUnzipM = testCase "combinators/unzipM" $ do
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


------------------------------------------------------------------------------
testTake :: Test
testTake = testCase "combinators/take" $ do
    fromList ([]::[Int]) >>= take 0 >>= toList >>= assertEqual "empty 0" []
    fromList ([]::[Int]) >>= take 10 >>= toList >>= assertEqual "empty 10" []

    forM_ [0..4] $ \n -> fromList [1,2,3::Int] >>=
                         take n >>=
                         toList >>=
                         assertEqual ("for " ++ show n)
                                       (Prelude.take (fromEnum n) [1..3])

    is  <- fromList [1,2,3::Int]
    is' <- take 2 is
    void $ read is'
    unRead 0 is'
    peek is >>= assertEqual "pb" (Just 0)
    toList is' >>= assertEqual "toList" [0,2]
    unRead 7 is'
    peek is >>= assertEqual "pb2" (Just 7)
    toList is' >>= assertEqual "toList2" [7]


------------------------------------------------------------------------------
testDrop :: Test
testDrop = testCase "combinators/drop" $ do
    fromList ([]::[Int]) >>= take 0 >>= toList >>= assertEqual "empty 0" []
    fromList ([]::[Int]) >>= take 10 >>= toList >>= assertEqual "empty 10" []

    forM_ [0..4] $ \n -> fromList [1,2,3::Int] >>=
                         drop n >>=
                         toList >>=
                         assertEqual ("for " ++ show n)
                                     (Prelude.drop (fromEnum n) [1..3])

    is  <- fromList [1,2,3::Int]
    is' <- drop 1 is
    read is' >>= assertEqual "read" (Just 2)
    unRead 0 is'
    peek is >>= assertEqual "pb" (Just 0)
    toList is' >>= assertEqual "toList" [0,3]
    unRead 7 is'
    peek is >>= assertEqual "pb2" (Just 7)
    toList is' >>= assertEqual "toList2" [7]
    toList is' >>= assertEqual "toList2_empty" []

    is2  <- fromList [1,2,3::Int]
    is2' <- drop 1 is2
    read is2' >>= assertEqual "read2" (Just 2)
    unRead 2 is2'
    unRead 1 is2'
    unRead 0 is2'
    toList is2' >>= assertEqual "toList3" [2,3]


------------------------------------------------------------------------------
testGive :: Test
testGive = testCase "combinators/give" $ forM_ [0..12] tgive
  where
    tgive n = fromList [1..10::Int] >>= \is ->
              outputToList (\os -> give n os >>= connect is) >>=
              assertEqual ("give" ++ show n)
                          (Prelude.take (fromEnum n) [1..10])


------------------------------------------------------------------------------
testIgnore :: Test
testIgnore = testCase "combinators/ignore" $ forM_ [0..12] tign
  where
    tign n = fromList [1..10::Int] >>= \is ->
             outputToList (\os -> ignore n os >>= connect is) >>=
             assertEqual ("ignore" ++ show n)
                         (Prelude.drop (fromEnum n) [1..10])
