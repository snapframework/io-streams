module Main where

import Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import qualified System.IO.Streams.Tests.Blaze as Blaze
import qualified System.IO.Streams.Tests.ByteString as ByteString

------------------------------------------------------------------------------


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Blaze" Blaze.tests
            , testGroup "Tests.ByteString" ByteString.tests
            ]
