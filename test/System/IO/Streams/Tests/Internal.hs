{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Internal (tests) where

------------------------------------------------------------------------------
import           Control.Monad hiding (mapM)
import           Data.Functor.Contravariant
import           Data.IORef
import           Data.Monoid
import           Prelude hiding (mapM, read)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal
import           System.IO.Streams.List

tests :: [Test]
tests = [ testSourceFunctor
        , testSinkContravariant
        , testNullInput
        , testCoverLockingStream
        ]


------------------------------------------------------------------------------
testSourceFunctor :: Test
testSourceFunctor = testCase "internal/sourceFunctor" $ do
    is <- sourceToStream $ fmap (+4) $ mconcat $
          map singletonSource [1::Int, 2, 3]

    l  <- toList is

    assertEqual "sourceFunctor" [5,6,7] l


------------------------------------------------------------------------------
testSinkContravariant :: Test
testSinkContravariant = testCase "internal/sinkContravariant" $ do
    ref <- newIORef 0
    os  <- sinkToStream $ contramap (+4) $ sink ref
    fromList [1,2,3::Int] >>= connectTo os

    out <- readIORef ref

    assertEqual "sinkContravariant" 18 out

  where
    sink ref = Sink $
               maybe (return nullSink)
                     (\s -> atomicModifyIORef ref $ \t -> (s+t, sink ref))

------------------------------------------------------------------------------
testNullInput :: Test
testNullInput = testCase "internal/nullInput" $ do
    is <- nullInput
    xs <- replicateM 10 $ read (is :: InputStream Int)
    assertEqual "nullInput" (replicate 10 Nothing) xs


------------------------------------------------------------------------------
testCoverLockingStream :: Test
testCoverLockingStream = testCase "internal/coverLockingStreams" $ do
    is <- fromList [1::Int, 2, 3] >>= lockingInputStream
    (os0, grab) <- listOutputStream
    os <- lockingOutputStream os0

    connect is os
    xs <- grab

    assertEqual "lockingStreams" [1,2,3] xs
