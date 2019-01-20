module System.IO.Streams.Tests.Concurrent (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Prelude                              hiding (lines, read, takeWhile, unlines, unwords, unwords, words)
import qualified System.IO.Streams                    as Streams
import qualified System.IO.Streams.Concurrent         as Streams
import           System.IO.Streams.Tests.Common
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck                      hiding (output)
import           Test.QuickCheck.Monadic
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testMakeChanPipe
        , testConcurrentMerge
        , testConcurrentMergeException
        , testInputOutput
        ]


------------------------------------------------------------------------------
testMakeChanPipe :: Test
testMakeChanPipe = testProperty "concurrent/makeChanPipe" $
                   monadicIO $
                   forAllM arbitrary prop
  where
    prop :: [Int] -> PropertyM IO ()
    prop l = liftQ $ do
        (is, os) <- Streams.makeChanPipe
        _        <- forkIO $ Streams.writeList l os >> Streams.write Nothing os
        Streams.toList is >>= assertEqual "makeChanPipe" l


------------------------------------------------------------------------------
testConcurrentMerge :: Test
testConcurrentMerge = testCase "concurrent/concurrentMerge" $ do
    mvars <- replicateM nthreads newEmptyMVar
    chans <- replicateM nthreads newChan
    let firstMVar = head mvars

    mapM_ (forkIO . ring) $ zip3 mvars (take nthreads $ drop 1 $ cycle mvars)
                                 chans
    inputs <- mapM Streams.chanToInput chans
    resultMVar <- newEmptyMVar
    _ <- forkIO (Streams.concurrentMerge inputs >>= Streams.toList
                                           >>= putMVar resultMVar)
    putMVar firstMVar 0
    result <- takeMVar resultMVar
    assertEqual "concurrent merge" [0..10] result

  where
    maxval   = 10 :: Int
    nthreads = 4  :: Int

    ring (prev, next, chan) = loop
      where
        loop = do x <- takeMVar prev
                  if x > maxval
                    then do writeChan chan Nothing
                            putMVar next x
                    else do writeChan chan $ Just x
                            threadDelay 100000
                            putMVar next $! x + 1
                            loop

------------------------------------------------------------------------------
testConcurrentMergeException :: Test
testConcurrentMergeException =
    testCase "concurrent/concurrentMerge/exception" $ do
        inp <- Streams.makeInputStream (error "bad") >>=
               Streams.concurrentMerge . (:[])
        expectExceptionH (Streams.toList inp)


------------------------------------------------------------------------------
testInputOutput :: Test
testInputOutput = testCase "concurrent/input-output" $ do
    is   <- Streams.fromList [1..10::Int]
    chan <- newChan
    is'  <- Streams.chanToInput chan
    Streams.inputToChan is chan
    Streams.toList is' >>= assertEqual "input-output" [1..10]
