{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Streams.Tests.Process (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad                  (liftM, void)
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as S
import qualified System.IO.Streams              as Streams
import           System.Timeout
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------

tests :: [Test]
#ifndef ENABLE_PROCESS_TESTS
tests = []
#else
tests = [ testInteractiveCommand
        , testInteractiveProcess
        ]


------------------------------------------------------------------------------
testInteractiveCommand :: Test
testInteractiveCommand = testCase "process/interactiveCommand" $ do
    (out, err) <- Streams.runInteractiveCommand "cat" >>= run [expected]
    assertEqual "interactiveCommand" expected out
    assertEqual "interactiveCommand" "" err

  where
    expected = "testing 1-2-3"


------------------------------------------------------------------------------
testInteractiveProcess :: Test
testInteractiveProcess = testCase "process/interactiveProcess" $ do
    (out, err) <- Streams.runInteractiveProcess "tr" ["a-z", "A-Z"]
                                                Nothing Nothing
                      >>= run [inputdata]
    assertEqual "interactiveProcess" expected out
    assertEqual "interactiveProcess" "" err

  where
    inputdata = "testing 1-2-3"
    expected = "TESTING 1-2-3"


------------------------------------------------------------------------------
run :: [ByteString]
    -> (Streams.OutputStream ByteString,
        Streams.InputStream S.ByteString,
        Streams.InputStream S.ByteString,
        Streams.ProcessHandle)
     -> IO (S.ByteString, S.ByteString)
run input (stdin, stdout, stderr, processHandle) = tout 5000000 $ do
    me   <- myThreadId
    outM <- newEmptyMVar
    errM <- newEmptyMVar
    bracket (mkThreads me outM errM) killThreads $ go outM errM

  where
    tout t m = timeout t m >>= maybe (error "timeout") return

    barfTo me (e :: SomeException) = throwTo me e

    killMe restore me m =
        void (try (restore m) >>= either (barfTo me) return)

    mkThreads me outM errM = mask $ \restore -> do
        tid1 <- forkIO $ killMe restore me $ snarf stdout outM
        tid2 <- forkIO $ killMe restore me $ snarf stderr errM
        return (tid1, tid2)

    killThreads (t1, t2) = do
        mapM_ killThread [t1, t2]
        Streams.waitForProcess processHandle

    go outM errM _ = do
        Streams.fromList input >>= Streams.connectTo stdin
        out <- takeMVar outM
        err <- takeMVar errM
        return (out, err)

    snarf is mv = liftM S.concat (Streams.toList is) >>= putMVar mv

-- ENABLE_PROCESS_TESTS
#endif
