{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Vector (tests) where

------------------------------------------------------------------------------
import           Control.Monad                  hiding (mapM)
import qualified Data.Vector                    as V
import           Prelude                        hiding (mapM, read)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------
import qualified System.IO.Streams              as S
import           System.IO.Streams.List
import           System.IO.Streams.Vector
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common (expectExceptionH)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testChunk
        , testWrite
        , testVectorOutputStream
        , testFromTo
        ]


------------------------------------------------------------------------------
testChunk :: Test
testChunk = testCase "vector/chunkVector" $ do
    let zeroLen :: IO ([V.Vector Int])
        zeroLen = fromList [1..10::Int] >>= chunkVector 0 >>= toList
    expectExceptionH zeroLen

    fromList [1..10 :: Int] >>= chunkVector 3
                            >>= toList
                            >>= assertEqual "chunkVector"
                                  (map V.fromList [ [1,2,3]
                                                  , [4,5,6]
                                                  , [7,8,9]
                                                  , [10]
                                                  ])
    fromList [1..12 :: Int] >>= chunkVector 3
                            >>= toList
                            >>= assertEqual "chunkVector2"
                                  (map V.fromList [ [1,2,3]
                                                  , [4,5,6]
                                                  , [7,8,9]
                                                  , [10,11,12]
                                                  ])


------------------------------------------------------------------------------
testWrite :: Test
testWrite = testCase "vector/writeVector" $
            outputToVector act >>=
            assertEqual "testWrite" (V.fromList [1..10::Int])
  where
    act str = do
      writeVector (V.fromList [1..10]) str
      S.write Nothing str
      S.write Nothing str


------------------------------------------------------------------------------
testVectorOutputStream :: Test
testVectorOutputStream = testCase "vector/vectorOutputStream" $ test1 >> test2
  where
    test1 = do
        (os, flush) <- vectorOutputStream
        fromList [1,2,3::Int] >>= S.connectTo os
        flush >>= assertEqual "v1" (V.fromList [1,2,3::Int])
        S.write (Just 4) os
        flush >>= assertEqual "v2" V.empty

    test2 = do
        (os, flush) <- vectorOutputStream
        fromList [1,2,3::Int] >>= S.supplyTo os
        flush >>= assertEqual "v1" (V.fromList [1,2,3::Int])
        S.write (Just 4) os
        flush >>= assertEqual "v2" (V.singleton (4::Int))


------------------------------------------------------------------------------
testFromTo :: Test
testFromTo = testCase "vector/fromVector" $ do
    fromVector V.empty >>= toVector
                       >>= assertEqual "f1" (V.empty :: V.Vector Int)
    fromVector vtest >>= toVector >>= assertEqual "f2" vtest

  where
    vtest = V.fromList [1..100::Int]
