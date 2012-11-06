module System.IO.Streams.Tests.List (tests) where

------------------------------------------------------------------------------
import           Prelude hiding (read)
import           System.IO.Streams.List
import           System.IO.Streams.Internal
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testFilterM ]


------------------------------------------------------------------------------
testFilterM :: Test
testFilterM = testCase "list/filterM" $ do
    is  <- fromList [1..10::Int]
    is' <- filterM (return . even) is

    read is' >>= assertEqual "read1" (Just 2)
    unRead 3 is'

    peek is >>= assertEqual "pushback" (Just 3)
    toList is' >>= assertEqual "rest" [4,6..10]

    unRead 20 is'

    peek is >>= assertEqual "pushback2" (Just 20)
    toList is' >>= assertEqual "rest2" [20]
    toList is' >>= assertEqual "eof" []
