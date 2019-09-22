{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module System.IO.Streams.Tests.Network (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent                 (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad                      (join)
import qualified Data.ByteString.Char8              as S
import           Data.IORef                         (atomicModifyIORef, newIORef)
import qualified Network.Socket                     as N
import           System.IO.Error                    (eofErrorType, mkIOError)
import           System.Timeout                     (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                         hiding (Test)
#if MIN_VERSION_network(2,7,0)
#else
import           Data.List                          (intercalate)
#endif
------------------------------------------------------------------------------
import qualified System.IO.Streams.Internal         as Streams
import qualified System.IO.Streams.Internal.Network as Streams
import qualified System.IO.Streams.List             as Streams
------------------------------------------------------------------------------
import           System.IO.Streams.Tests.Common     (expectExceptionH)

tests :: [Test]
tests = [ testSocket
        , testSocketWithError
        ]

testSocket :: Test
testSocket = testCase "network/socket" $
             N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    -- compats
#if MIN_VERSION_network(2,7,0)
    mkAddr = return . N.tupleToHostAddress
    defaultPort = N.defaultPort
    close = N.close
    bind = N.bind
#else
    mkAddr (o1,o2,o3,o4) = N.inet_addr . intercalate "." $ map show [o1,o2,o3,o4]
    defaultPort = N.aNY_PORT
    close = N.sClose
    bind = N.bindSocket
#endif

    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        _ <- forkIO $ client portMVar resultMVar
        server portMVar
        l <- takeMVar resultMVar
        assertEqual "testSocket" l ["ok"]

    client mvar resultMVar = do
        port <- takeMVar mvar
        sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
        addr <- mkAddr (127, 0, 0, 1)
        let saddr = N.SockAddrInet port addr
        N.connect sock saddr
        (is, os) <- Streams.socketToStreams sock
        Streams.fromList ["", "ok"] >>= Streams.connectTo os
        N.shutdown sock N.ShutdownSend
        Streams.toList is >>= putMVar resultMVar
        close sock

    server mvar = do
        sock  <- N.socket N.AF_INET N.Stream N.defaultProtocol
        addr <- mkAddr (127, 0, 0, 1)
        let saddr = N.SockAddrInet defaultPort addr
        bind sock saddr
        N.listen sock 5
        port  <- N.socketPort sock
        putMVar mvar port
        (csock, _) <- N.accept sock
        (is, os) <- Streams.socketToStreams csock
        Streams.toList is >>= flip Streams.writeList os
        close csock
        close sock

testSocketWithError :: Test
testSocketWithError = testCase "network/socket-error" $ N.withSocketsDo $ do
    codes1 <- newIORef [ return 1
                       , ioError $ mkIOError eofErrorType "eof" Nothing Nothing ]
    codes2 <- newIORef [ return 1
                       , ioError $ userError "foo" ]

    (is1, _)  <- Streams.socketToStreamsWithBufferSizeImpl (rbuf codes1) 64 (error "z")
    (Just s1) <- Streams.read is1
    assertEqual "one byte" 1 $ S.length s1
    Nothing   <- Streams.read is1

    (is2, _)  <- Streams.socketToStreamsWithBufferSizeImpl (rbuf codes2) 64 undefined
    (Just s2) <- Streams.read is2
    assertEqual "one byte" 1 $ S.length s2
    expectExceptionH $ Streams.read is2

  where
    rbuf rcodes _ _ _ = join $ atomicModifyIORef rcodes $ \codes ->
                        (tail codes, head codes)
