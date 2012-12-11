{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Tests.Network (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, newEmptyMVar,
                                                 putMVar, takeMVar)
import qualified Network.Socket                 as N
import           System.Timeout                 (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------
import qualified System.IO.Streams.Internal     as Streams
import qualified System.IO.Streams.List         as Streams
import qualified System.IO.Streams.Network      as Streams
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testSocket ]

testSocket :: Test
testSocket = testCase "network/socket" $
             N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ client portMVar resultMVar
        server portMVar
        l <- takeMVar resultMVar
        assertEqual "testSocket" l ["ok"]

    client mvar resultMVar = do
        port <- takeMVar mvar
        sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
        addr <- N.inet_addr "127.0.0.1"
        let saddr = N.SockAddrInet port addr
        N.connect sock saddr
        (is, os) <- Streams.socketToStreams sock
        Streams.fromList ["", "ok"] >>= Streams.connectTo os
        N.shutdown sock N.ShutdownSend
        Streams.toList is >>= putMVar resultMVar
        N.close sock

    server mvar = do
        sock  <- N.socket N.AF_INET N.Stream N.defaultProtocol
        addr  <- N.inet_addr "127.0.0.1"
        let saddr = N.SockAddrInet N.aNY_PORT addr
        N.bind sock saddr
        N.listen sock 5
        port  <- N.socketPort sock
        putMVar mvar port
        (csock, _) <- N.accept sock
        (is, os) <- Streams.socketToStreams csock
        Streams.toList is >>= flip Streams.writeList os
        N.close csock
        N.close sock
