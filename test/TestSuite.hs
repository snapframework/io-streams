module Main where

import Test.Framework (defaultMain, testGroup)
import qualified System.IO.Streams.Tests.Attoparsec as Attoparsec
import qualified System.IO.Streams.Tests.Builder as Builder
import qualified System.IO.Streams.Tests.ByteString as ByteString
import qualified System.IO.Streams.Tests.Combinators as Combinators
import qualified System.IO.Streams.Tests.File as File
import qualified System.IO.Streams.Tests.Handle as Handle
import qualified System.IO.Streams.Tests.Internal as Internal
import qualified System.IO.Streams.Tests.List as List
import qualified System.IO.Streams.Tests.Zlib as Zlib


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Attoparsec" Attoparsec.tests
            , testGroup "Tests.Builder" Builder.tests
            , testGroup "Tests.ByteString" ByteString.tests
            , testGroup "Tests.Combinators" Combinators.tests
            , testGroup "Tests.File" File.tests
            , testGroup "Tests.Handle" Handle.tests
            , testGroup "Tests.Internal" Internal.tests
            , testGroup "Tests.List" List.tests
            , testGroup "Tests.Zlib" Zlib.tests
            ]
