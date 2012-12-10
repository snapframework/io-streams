{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Text (tests) where

------------------------------------------------------------------------------
import           Control.Monad                  ((>=>))
import           Data.Text.Encoding.Error
import qualified System.IO.Streams.Internal     as Streams
import qualified System.IO.Streams.List         as Streams
import           System.IO.Streams.Tests.Common
import qualified System.IO.Streams.Text         as Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testDecodeOK
        , testStrictDecodeError
        , testEncode
        ]


------------------------------------------------------------------------------
testEncode :: Test
testEncode = testCase "text/encodeUtf8" $ do
    is <- Streams.fromList ["\x3BC", "ok", ""]
    Streams.outputToList (Streams.encodeUtf8 >=> Streams.connect is)
        >>= assertEqual "ok encode" ["\xCE\xBC", "ok", ""]


------------------------------------------------------------------------------
testDecodeOK :: Test
testDecodeOK = testCase "text/decodeUtf8/wholeChunk" $ do
    Streams.fromList ["\xCE\xBC", "ok", ""]
        >>= Streams.decodeUtf8
        >>= Streams.toList
        >>= assertEqual "ok decode" ["\x3BC", "ok", ""]

    Streams.fromList ["\xCE", "\xBC", "ok", "foo\xCE", "\xBC"]
        >>= Streams.decodeUtf8
        >>= Streams.toList
        >>= assertEqual "ok decode 2" ["\x3BC", "ok", "foo", "\x3BC"]

    Streams.fromList ["\xE2\xB6", "\x8E"]
        >>= Streams.decodeUtf8
        >>= Streams.toList
        >>= assertEqual "ok decode 3" ["\x2D8E"]

    Streams.fromList ["\xF0\x90\x80\x83"]
        >>= Streams.decodeUtf8
        >>= Streams.toList
        >>= assertEqual "ok decode 4" ["\x10003"]

    Streams.fromList []
        >>= Streams.decodeUtf8With strictDecode
        >>= Streams.toList
        >>= assertEqual "ok strict empty" []


------------------------------------------------------------------------------
testStrictDecodeError :: Test
testStrictDecodeError = testCase "text/decodeUtf8/error" $ do
    expectExceptionH (Streams.fromList ["\x87"] >>=
                      Streams.decodeUtf8With strictDecode >>=
                      Streams.toList)
    expectExceptionH (Streams.fromList ["o\x87\x87"] >>=
                      Streams.decodeUtf8With strictDecode >>=
                      Streams.toList)
