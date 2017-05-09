{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Attoparsec.Text (tests, testParserU) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Attoparsec.Text                  hiding (eitherResult)
import           Data.Char                             (isAlpha, isSpace)
import           Data.Text                             (Text)
import           Prelude                               hiding (takeWhile)
import           System.IO.Streams
import           System.IO.Streams.Attoparsec.Text
import           System.IO.Streams.Internal.Attoparsec (eitherResult, parseFromStreamInternal)
import           System.IO.Streams.Tests.Common
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                            hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testParseFromStream
        , testParseFromStreamError
        , testParseFromStreamError2
        , testPartialParse
        , testEmbeddedNull
        , testTrivials
        , testParseFromStreamU
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
testParser2 :: Parser (Maybe Text)
testParser2 = do
    end <- atEnd
    if end
      then return Nothing
      else liftM Just $ string "bork"


------------------------------------------------------------------------------
testParserU :: Parser (Maybe Text)
testParserU = do
    end <- atEnd
    if end
      then return Nothing
      else do
          _ <- takeWhile (not . isAlpha)
          liftM Just (takeWhile isAlpha)


------------------------------------------------------------------------------
testParseFromStream :: Test
testParseFromStream = testCase "attoparsec/parseFromStream" $ do
    is <- fromList ["1", "23", ", 4", ", 5, 6, 7"]
    x0 <- parseFromStream testParser is

    assertEqual "first parse" (Just 123) x0

    l  <- parserToInputStream testParser is >>= toList

    assertEqual "rest" [4, 5, 6, 7] l
    toList is >>= assertEqual "double eof" []


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
    p = parserToInputStream ((testParser2 <?> "foo") <?> "bar") >=> toList


------------------------------------------------------------------------------
testPartialParse :: Test
testPartialParse = testCase "attoparsec/partialParse" $ do
    is <- fromList ["1,", "2,", "3"]
    expectExceptionH $ parseFromStreamInternal parseFunc feedFunc testParser is

  where
    result    = Partial (const result)
    parseFunc = const $ const $ result
    feedFunc  = const $ const $ result

------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "attoparsec/trivials" $ do
    coverTypeableInstance (undefined :: ParseException)
    let (Right x) = eitherResult $ Done undefined 4 :: Either (Text, [String], String) Int
    assertEqual "eitherResult" 4 x

------------------------------------------------------------------------------
testEmbeddedNull :: Test
testEmbeddedNull = testCase "attoparsec/embeddedNull" $ do
    is <- fromList ["", "1", "23", "", ", 4", ", 5, 6, 7"]
    x0 <- parseFromStream testParser is

    assertEqual "first parse" (Just 123) x0

    l  <- parserToInputStream testParser is >>= toList

    assertEqual "rest" [4, 5, 6, 7] l

------------------------------------------------------------------------------
testParseFromStreamU :: Test
testParseFromStreamU = testCase "attoparsec/parseFromStreamU" $ do
    is <- fromList ["123æø", "å", "💻⛇⛄☃Š", "š5ŧđ6naå7"]
    x0 <- parseFromStream testParserU is

    assertEqual "first parse" (Just "æøå") x0

    l  <- parserToInputStream testParserU is >>= toList

    assertEqual "rest" ["Šš", "ŧđ", "naå", ""] l
    toList is >>= assertEqual "double eof" []

