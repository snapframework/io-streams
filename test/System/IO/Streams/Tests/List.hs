{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.List (tests) where

------------------------------------------------------------------------------
import           Control.Monad                  hiding (mapM)
import           Prelude                        hiding (mapM, read)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------
import           System.IO.Streams.List
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common (expectExceptionH)

tests :: [Test]
tests = [ testChunkJoin, testChunkWithJoin ]



testChunkJoin :: Test
testChunkJoin = testCase "list/chunkList and join" $ do
    expectExceptionH (fromList [1..10::Int] >>= chunkList 0 >>= toList)

    fromList [1..10 :: Int] >>= chunkList 3
                            >>= toList
                            >>= assertEqual "chunkList" [ [1,2,3]
                                                        , [4,5,6]
                                                        , [7,8,9]
                                                        , [10]
                                                        ]
    fromList [1..12 :: Int] >>= chunkList 3
                            >>= concatLists
                            >>= toList
                            >>= assertEqual "concatlists" [1..12]

testChunkWithJoin :: Test
testChunkWithJoin = testCase "list/chunkListWith and join" $ do
    fromList [1..10 :: Int] >>= chunkListWith (\_ n -> n>=3)
                            >>= toList
                            >>= assertEqual "chunkListWith" [ [1,2,3]
                                                            , [4,5,6]
                                                            , [7,8,9]
                                                            , [10]
                                                            ]
    fromList [1..12 :: Int] >>= chunkListWith (\_ n -> n>=3)
                            >>= concatLists
                            >>= toList
                            >>= assertEqual "concatlists" [1..12]

    fromList ['a'..'z' :: Char]
        >>= chunkListWith (\x n -> n>=4 && x `elem` ("aeiouy" :: String))
        >>= toList
        >>= assertEqual "chunkListWith" [ "abcde"
                                        , "fghi"
                                        , "jklmno"
                                        , "pqrstu"
                                        , "vwxy"
                                        , "z"
                                        ]
