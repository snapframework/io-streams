module Main where

import qualified System.IO.Streams.Tests.Attoparsec.ByteString as AttoparsecByteString
import qualified System.IO.Streams.Tests.Attoparsec.Text       as AttoparsecText
import qualified System.IO.Streams.Tests.Builder               as Builder
import qualified System.IO.Streams.Tests.ByteString            as ByteString
import qualified System.IO.Streams.Tests.Combinators           as Combinators
import qualified System.IO.Streams.Tests.Concurrent            as Concurrent
import qualified System.IO.Streams.Tests.Debug                 as Debug
import qualified System.IO.Streams.Tests.File                  as File
import qualified System.IO.Streams.Tests.Handle                as Handle
import qualified System.IO.Streams.Tests.Internal              as Internal
import qualified System.IO.Streams.Tests.List                  as List
import qualified System.IO.Streams.Tests.Network               as Network
import qualified System.IO.Streams.Tests.Process               as Process
import qualified System.IO.Streams.Tests.Text                  as Text
import qualified System.IO.Streams.Tests.Vector                as Vector
import qualified System.IO.Streams.Tests.Zlib                  as Zlib
import           Test.Framework                                (defaultMain, testGroup)


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Attoparsec.ByteString" AttoparsecByteString.tests
            , testGroup "Tests.Attoparsec.Text" AttoparsecText.tests
            , testGroup "Tests.Builder" Builder.tests
            , testGroup "Tests.ByteString" ByteString.tests
            , testGroup "Tests.Debug" Debug.tests
            , testGroup "Tests.Combinators" Combinators.tests
            , testGroup "Tests.Concurrent" Concurrent.tests
            , testGroup "Tests.File" File.tests
            , testGroup "Tests.Handle" Handle.tests
            , testGroup "Tests.Internal" Internal.tests
            , testGroup "Tests.List" List.tests
            , testGroup "Tests.Network" Network.tests
            , testGroup "Tests.Process" Process.tests
            , testGroup "Tests.Text" Text.tests
            , testGroup "Tests.Vector" Vector.tests
            , testGroup "Tests.Zlib" Zlib.tests
            ]
