module System.IO.Streams.Tests.Concurrent (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Prelude                              hiding (lines, read,
                                                       takeWhile, unlines,
                                                       unwords, unwords,
                                                       words)
import qualified Prelude
import           System.IO.Streams                    hiding (filter,
                                                       intersperse, mapM_)
import           System.IO.Streams.Concurrent
import           System.IO.Streams.Tests.Common
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck                      hiding (output)
import           Test.QuickCheck.Monadic
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testMakeChanPipe
        ]


------------------------------------------------------------------------------
testMakeChanPipe :: Test
testMakeChanPipe = testProperty "concurrent/makeChanPipe" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: [Int] -> PropertyM IO ()
    prop l = liftQ $ do
        (is, os) <- makeChanPipe
        _        <- forkIO $ writeList l os >> write Nothing os
        toList is >>= assertEqual "makeChanPipe" l
