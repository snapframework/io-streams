{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Handle (tests) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad                  hiding (mapM)
import           Data.ByteString.Builder        (byteString)
import qualified Data.ByteString.Char8          as S
import           Data.List
import           Foreign.Marshal.Alloc          (allocaBytes)
import           Foreign.Marshal.Utils          (copyBytes)
import           Foreign.Ptr                    (castPtr)
import qualified GHC.IO.Buffer                  as HB
import qualified GHC.IO.BufferedIO              as H
import qualified GHC.IO.Device                  as H
import           Prelude                        hiding (mapM, read)
import           System.Directory
import           System.FilePath
import           System.IO                      hiding (stderr, stdin, stdout)
import qualified System.IO                      as IO
import           System.IO.Streams              (OutputStream)
import qualified System.IO.Streams              as Streams
import qualified System.IO.Streams.Internal     as Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common

tests :: [Test]
tests = [ testHandle
        , testStdHandles
        , testRepeatedConnects
        , testInputStreamToHandle
        , testOutputStreamToHandle
        , testStreamPairToHandle
        , testHandleInstances
        , testHandleBadnesses
        ]


------------------------------------------------------------------------------
testHandle :: Test
testHandle = testCase "handle/files" $ do
    createDirectoryIfMissing False "tmp"
    tst `finally` eatException (removeFile fn >> removeDirectory "tmp")

  where
    fn = "tmp" </> "data"

    tst = do
        withBinaryFile fn WriteMode $ \h -> do
            let l = "" : (intersperse " " ["the", "quick", "brown", "fox"])
            os <- Streams.handleToOutputStream h
            Streams.fromList l >>= Streams.connectTo os

        withBinaryFile fn ReadMode $ \h -> do
            l <- liftM S.concat (Streams.handleToInputStream h >>=
                                 Streams.toList)
            assertEqual "testFiles" "the quick brown fox" l


------------------------------------------------------------------------------
testRepeatedConnects :: Test
testRepeatedConnects = testCase "handle/repeatedConnects" $ do
    createDirectoryIfMissing False dirname
    tst `finally` eatException (removeFile fn >> removeDirectory dirname)
  where
    dirname = "tmp_r_c"
    fn = dirname </> "data"

    tst = do
        withBinaryFile fn WriteMode $ \h -> do
            os0 <- Streams.handleToOutputStream h
            os  <- Streams.builderStream os0

            let l1 = map byteString ["the ", "quick ", "brown "]
            let l2 = map byteString ["fox ", "jumped"]
            Streams.fromList l1 >>= Streams.connectTo os
            Streams.fromList l2 >>= Streams.connectTo os
        S.readFile fn >>= assertEqual "eof should close" "the quick brown "

------------------------------------------------------------------------------
testStdHandles :: Test
testStdHandles = testCase "handle/stdHandles" $ do
    hClose IO.stdin
    -- Should generate exception: handle is closed.
    expectExceptionH (Streams.toList Streams.stdin)
    Streams.write (Just "") Streams.stdout
    Streams.write (Just "") Streams.stderr
    return ()


------------------------------------------------------------------------------
testInputStreamToHandle :: Test
testInputStreamToHandle = testCase "handle/inputStreamToHandle" $ do
    h <- Streams.fromList ["foo", "bar", "baz"] >>=
         Streams.inputStreamToHandle
    S.hGetContents h >>= assertEqual "inputStreamToHandle" "foobarbaz"


------------------------------------------------------------------------------
testOutputStreamToHandle :: Test
testOutputStreamToHandle = testCase "handle/outputStreamToHandle" $ do
    (os, getInput) <- Streams.listOutputStream
    h <- Streams.outputStreamToHandle os
    S.hPutStrLn h "foo"
    liftM S.concat getInput >>= assertEqual "outputStreamToHandle" "foo\n"


------------------------------------------------------------------------------
testStreamPairToHandle :: Test
testStreamPairToHandle = testCase "handle/streamPairToHandle" $ do
    is             <- Streams.fromList ["foo", "bar", "baz"]
    (os, getInput) <- Streams.listOutputStream

    h <- Streams.streamPairToHandle is os
    S.hPutStrLn h "foo"
    S.hGetContents h >>= assertEqual "input stream" "foobarbaz"
    liftM S.concat getInput >>= assertEqual "output stream" "foo\n"


------------------------------------------------------------------------------
testHandleBadnesses :: Test
testHandleBadnesses = testCase "handle/badness" $ do
    h <- Streams.fromList ["foo", "bar", "baz"] >>= Streams.inputStreamToHandle
    _ <- S.hGetContents h
    expectExceptionH $ S.hGetContents h

    h' <- Streams.fromList ["foo", "bar", "baz"] >>= Streams.inputStreamToHandle
    expectExceptionH $ S.hPutStrLn h' "foo"

    (os, _) <- Streams.listOutputStream
    h'' <- Streams.outputStreamToHandle os
    expectExceptionH $ S.hGetContents h''

    is <- Streams.fromList ["foo"]
    h''' <- Streams.streamPairToHandle is os
    _ <- S.hGetContents h'''
    expectExceptionH $ S.hGetContents h'''


------------------------------------------------------------------------------
testHandleInstances :: Test
testHandleInstances = testCase "handle/ghc-instances" $ do
    is            <- Streams.fromList ["foo", "bar", "baz" :: S.ByteString]
    (os, getList) <- Streams.listOutputStream
    let sp   = Streams.SP is (os :: OutputStream S.ByteString)
    expectExceptionH $ withZeroOffset H.write is undefined undefined
    expectExceptionH $ withZeroOffset H.writeNonBlocking is undefined undefined
    expectExceptionH $ H.flushWriteBuffer is undefined
    expectExceptionH $ H.flushWriteBuffer0 is undefined

    expectExceptionH $ withZeroOffset H.read os undefined undefined
    expectExceptionH $ withZeroOffset H.writeNonBlocking os undefined undefined

    expectExceptionH $ H.fillReadBuffer0 is undefined
    expectExceptionH $ H.fillReadBuffer0 os undefined
    expectExceptionH $ H.fillReadBuffer0 sp undefined

    H.ready is False 0 >>= assertEqual "ready input" True
    H.ready os False 0 >>= assertEqual "ready output" True
    H.ready sp False 0 >>= assertEqual "ready pair" True

    H.devType is >>= assertBool "devtype input"  . (== H.Stream)
    H.devType os >>= assertBool "devtype output" . (== H.Stream)
    H.devType sp >>= assertBool "devtype pair"   . (== H.Stream)

    expectExceptionH $ withZeroOffset H.readNonBlocking is undefined undefined
    expectExceptionH $ withZeroOffset H.readNonBlocking os undefined undefined
    expectExceptionH $ withZeroOffset H.readNonBlocking sp undefined undefined
    expectExceptionH $ withZeroOffset H.writeNonBlocking is undefined undefined
    expectExceptionH $ withZeroOffset H.writeNonBlocking os undefined undefined
    expectExceptionH $ withZeroOffset H.writeNonBlocking sp undefined undefined

    S.useAsCStringLen "foo" $ \(cstr, l) -> do
        withZeroOffset H.write os (castPtr cstr) l
        liftM S.concat getList >>= assertEqual "H.write 1" "foo"
        withZeroOffset H.write sp (castPtr cstr) l
        liftM S.concat getList >>= assertEqual "H.write 2" "foo"
        buf <- H.newBuffer sp HB.WriteBuffer
        HB.withBuffer buf $ \ptr -> copyBytes ptr (castPtr cstr) 3
        (l', !buf') <- H.flushWriteBuffer0 sp $ buf { HB.bufR = 3 }
        assertEqual "flushWriteBuffer0" 3 l'
        assertEqual "bufR" 0 $ HB.bufR buf'
        liftM S.concat getList >>= assertEqual "write 3" "foo"


    allocaBytes 3 $ \buf -> do
        l <- withZeroOffset H.read is buf 3
        assertEqual "3 byte read" 3 l
        S.packCStringLen (castPtr buf, l) >>= assertEqual "first read" "foo"
        l' <- withZeroOffset H.read sp buf 3
        assertEqual "3 byte read #2" 3 l'
        S.packCStringLen (castPtr buf, l') >>= assertEqual "second read" "bar"
        expectExceptionH $ withZeroOffset H.read os buf 3
  where
#if MIN_VERSION_base(4,15,0)
    withZeroOffset :: Num off => (a -> ptr -> off -> n -> ioint) -> a -> ptr -> n -> ioint
    withZeroOffset f a ptr n = f a ptr 0 n
#else
    withZeroOffset :: a -> a
    withZeroOffset = id
#endif
