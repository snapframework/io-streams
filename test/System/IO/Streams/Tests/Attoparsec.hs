{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Attoparsec (tests) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            ( ByteString )
import           Prelude                   hiding ( takeWhile )
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                hiding ( Test )
import           System.IO.Streams
import           System.IO.Streams.Attoparsec
import           System.IO.Streams.Tests.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testParseFromStream
        , testParseFromStreamError
        , testParseFromStreamError2
        , testTrivials
        ]


------------------------------------------------------------------------------
testParser :: Parser (Maybe Int)
testParser = do
    end <- atEnd
    if end
      then return Nothing
      else do
          _ <- takeWhile (\c -> isSpace c || c == ',')
          liftM Just decimal


------------------------------------------------------------------------------
testParser2 :: Parser (Maybe ByteString)
testParser2 = do
    end <- atEnd
    if end
      then return Nothing
      else liftM Just $ string "bork"


------------------------------------------------------------------------------
testParseFromStream :: Test
testParseFromStream = testCase "attoparsec/parseFromStream" $ do
    is <- fromList ["1", "23", ", 4", ", 5, 6, 7"]
    x0 <- parseFromStream testParser is

    assertEqual "first parse" (Just 123) x0

    l  <- parserToInputStream testParser is >>= toList

    assertEqual "rest" [4, 5, 6, 7] l


------------------------------------------------------------------------------
testParseFromStreamError :: Test
testParseFromStreamError = testCase "attoparsec/parseFromStreamError" $ do
    is <- fromList ["1", "23", ", 4", ",xxxx 5, 6, 7"] >>=
          parserToInputStream testParser

    expectExceptionH $ toList is


------------------------------------------------------------------------------
testParseFromStreamError2 :: Test
testParseFromStreamError2 = testCase "attoparsec/parseFromStreamError2" $ do
    l <- fromList ["borkbork", "bork"] >>= p
    assertEqual "ok" ["bork", "bork", "bork"] l

    expectExceptionH $ fromList ["bork", "bo"] >>= p
    expectExceptionH $ fromList ["xxxxx"] >>= p

  where
    p = parserToInputStream testParser2 >=> toList

------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "attoparsec/trivials" $ do
    coverTypeableInstance (undefined :: ParseException)
