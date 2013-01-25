{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Debug (tests) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8          as S
import qualified System.IO.Streams              as Streams
import qualified System.IO.Streams.Debug        as Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testDebugInput
        , testDebugOutput
        ]


------------------------------------------------------------------------------
testDebugInput :: Test
testDebugInput = testCase "debug/input" $ do
    s  <- Streams.fromList [S.replicate 100 'a', "foo"]
    (ds, getDebugOutput) <- Streams.listOutputStream
    s' <- Streams.debugInputBS "foo" ds s
    Streams.unRead "blah" s'
    Streams.skipToEof s'

    l <- getDebugOutput

    assertEqual "debugInput" expected l
  where
    expected = [
        "foo: pushback: \"blah\"\n"
      , "foo: got chunk: \"blah\"\n"
      , "foo: got chunk: \"aaaaaaaaaaaaaa ... aaaaaaaaaaaaaa\" (100 bytes)\n"
      , "foo: got chunk: \"foo\"\n"
      , "foo: got EOF\n"
      ]


------------------------------------------------------------------------------
testDebugOutput :: Test
testDebugOutput = testCase "debug/output" $ do
    is <- Streams.fromList [S.replicate 100 'a', "foo"]
    o  <- Streams.makeOutputStream f
    (ds, getDebugOutput) <- Streams.listOutputStream
    o' <- Streams.debugOutputBS "foo" ds o
    Streams.connect is o'

    l <- getDebugOutput

    assertEqual "debugInput" expected l
  where
    f !_ = return ()

    expected = [
        "foo: got chunk: \"aaaaaaaaaaaaaa ... aaaaaaaaaaaaaa\" (100 bytes)\n"
      , "foo: got chunk: \"foo\"\n"
      , "foo: got EOF\n"
      ]

