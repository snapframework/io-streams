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
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck hiding (output)
import           Test.QuickCheck.Monadic
import           Test.HUnit hiding (Test)
import           System.IO.Streams.Tests.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testBoyerMoore
        , testCountInput
        , testCountOutput
        , testKillIfTooSlow
        , testReadExactly
        , testReadNoMoreThan
        , testReadNoMoreThan2
        , testReadNoMoreThan3
        , testTakeNoMoreThan
        , testTakeNoMoreThan2
        , testTrivials
        , testWriteLazyByteString
        , testWriteNoMoreThan
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
        is2 <- readNoMoreThan 0 is

        x   <- toList is2
        y   <- liftM L.fromChunks $ toList is

        assertEqual "readNoMoreThan3" [] x
        assertEqual "readNoMoreThan4" l y

        -- Test that pushback makes it back to the source inputstream
        is3 <- readNoMoreThan 20 is
        void $ toList is3
        unRead "ok2" is3
        unRead "ok1" is3

        z   <- toList is
        assertEqual "readNoMoreThan5" ["ok1", "ok2"] z


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

            unRead "ok2" is'
            unRead "ok1" is'
            z   <- toList is
            assertEqual "takeNoMoreThan2-3" ["ok1", "ok2"] z


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

    src  <- mkSrc
    src' <- killIfTooSlow (return ()) 10 2 src

    void $ toList src'
    unRead "ok2" src'
    unRead "ok1" src'
    l <- toList src

    assertEqual "killIfTooSlow/pushback" ["ok1", "ok2"] l


  where
    mkSrc = fromList $ take 100 $ cycle $
            intersperse " " ["the", "quick", "brown", "fox"]

    mkList = mkSrc >>= killIfTooSlow (return ()) 10 2

    trickleFrom is = go
      where
        go = read is >>= maybe (return ())
                               (\x -> x `seq` (threadDelay 2000000 >> go))


------------------------------------------------------------------------------
testBoyerMoore :: Test
testBoyerMoore = testProperty "bytestring/boyerMoore" $
                 monadicIO $ forAllM arbitrary prop
  where
    prop :: (ByteString, [ByteString]) -> PropertyM IO ()
    prop (needle, haystack') = do
        let lneedle   = L.fromChunks [needle]
        let lhaystack = L.fromChunks haystack'

        pre ((not $ S.null needle) &&
             (not $ L.null lhaystack) &&
             (not $ S.isInfixOf needle $ S.concat haystack'))


        (lhay, toklist0) <- insertNeedle lneedle lhaystack
        let stream  = L.toChunks $ L.concat [lneedle, lhay]
        let toklist = (Match needle) : toklist0

        -- there should be exactly three matches
        out <- liftQ (fromList stream >>= boyerMooreHorspool needle >>= toList)

        let nMatches = length $ filter isMatch out

        let out' = concatAdj Nothing id out

        when (nMatches /= 3 || out' /= toklist) $ liftQ $ do
            putStrLn "got wrong output!!"
            putStrLn "needle:\n"
            putStrLn $ show lneedle
            putStrLn "\nhaystack:\n"
            mapM_ (putStrLn . show) stream
            putStrLn "\noutput stream:"
            mapM_ (putStrLn . show) out
            putStrLn "\noutput stream (minified):"
            mapM_ (putStrLn . show) out'
            putStrLn "\nexpected output:"
            mapM_ (putStrLn . show) toklist
            putStrLn ""

        liftQ $ do
            assertEqual "boyer-moore matches" 3 nMatches
            assertEqual "boyer-moore output" toklist out'


    isMatch (Match _) = True
    isMatch _         = False

    concatAdj :: Maybe MatchInfo
              -> ([MatchInfo] -> [MatchInfo])
              -> [MatchInfo]
              -> [MatchInfo]
    concatAdj pre dl []     = dl $ maybe [] (:[]) pre
    concatAdj pre dl (x:xs) =
        maybe (concatAdj (Just x) dl xs)
              (\p -> maybe (concatAdj (Just x) (dl . (p:)) xs)
                           (\x' -> concatAdj (Just x') dl xs)
                           (merge p x))
              pre

      where
        merge (NoMatch x) y
            | S.null x  = Just y
            | otherwise = case y of
                            NoMatch x' -> Just $ NoMatch $ x `mappend` x'
                            _          -> Nothing

        merge (Match _) _ = Nothing

    insertNeedle lneedle lhaystack = do
        idxL  <- pick $ choose (0, lenL-1)
        idxN  <- pick $ choose (0, lenN-1)
        idxN2 <- pick $ choose (0, lenN-1)
        let (l1, l2) = L.splitAt (toEnum idxL) lhaystack
        let (n1, n2) = L.splitAt (toEnum idxN) lneedle
        let (n3, n4) = L.splitAt (toEnum idxN2) lneedle

        let out1 = L.concat [ l1, n1, n2, l2, n3, n4 ]

        let res = concatAdj Nothing id
                      [ NoMatch $ strict l1
                      , Match   $ strict lneedle
                      , NoMatch $ strict l2
                      , Match   $ strict lneedle
                      ]

        return (out1, res)

      where
        nonEmpty (Match _)   = True
        nonEmpty (NoMatch x) = not $ S.null x

        strict = S.concat . L.toChunks

        lenN = fromEnum $ L.length lneedle
        lenL = fromEnum $ L.length lhaystack


------------------------------------------------------------------------------
testWriteLazyByteString :: Test
testWriteLazyByteString = testProperty "bytestring/writeLazy" $
                          monadicIO $
                          forAllM arbitrary prop
  where
    prop :: [ByteString] -> PropertyM IO ()
    prop l0 = liftQ $ do
        let l = filter (not . S.null) l0
        let s = L.fromChunks l
        (os, grab) <- listOutputStream
        writeLazyByteString s os

        l' <- grab
        assertEqual "writeLazy" l l'


------------------------------------------------------------------------------
testReadExactly :: Test
testReadExactly = testProperty "bytestring/readExactly" $
                  monadicIO $
                  forAllM arbitrary prop
  where
    prop l0 = liftQ $ do
        let l = filter (not . S.null) l0
        is <- fromList l

        let s = L.fromChunks l
        let n = fromEnum $ L.length s

        t <- readExactly n is
        assertEqual "eq" s $ L.fromChunks [t]

        unRead t is
        expectExceptionH $ readExactly (n+1) is

        when (n > 0) $ do
            is' <- fromList l
            u   <- readExactly (n-1) is'
            assertEqual "eq2" (L.take (toEnum $ n-1) s) (L.fromChunks [u])
            v   <- readExactly 1 is'
            assertEqual "eq3" (L.drop (toEnum $ n-1) s) (L.fromChunks [v])


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "bytestring/testTrivials" $ do
    coverTypeableInstance (undefined :: TooManyBytesReadException)
    coverShowInstance     (undefined :: TooManyBytesReadException)
    coverTypeableInstance (undefined :: RateTooSlowException)
    coverShowInstance     (undefined :: RateTooSlowException)
    coverTypeableInstance (undefined :: ReadTooShortException)

    coverEqInstance $ Match ""

    coverShowInstance $ Match ""
    coverShowInstance $ NoMatch ""
