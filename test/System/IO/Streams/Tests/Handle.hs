{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Handle (tests) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad hiding (mapM)
import qualified Data.ByteString.Char8 as S
import           Data.List
import           Prelude hiding (mapM, read)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common

tests :: [Test]
tests = [ testFiles
        ]


------------------------------------------------------------------------------
testFiles :: Test
testFiles = testCase "handle/files" $ do
    createDirectoryIfMissing False "tmp"
    tst `finally` eatException (removeFile fn >> removeDirectory "tmp")

  where
    fn = "tmp" </> "data"

    tst = do
        withFileAsOutputStream fn WriteMode $ \os -> do
            let l = "" : (intersperse " " ["the", "quick", "brown", "fox"])
            fromList l >>= connectTo os

        l <- liftM S.concat $ withFileAsInputStream fn toList
        assertEqual "testFiles" "the quick brown fox" l
