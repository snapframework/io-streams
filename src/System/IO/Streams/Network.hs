{-# LANGUAGE CPP #-}

-- | Converting network 'Socket's to streams.
module System.IO.Streams.Network
  ( -- * Sockets to Streams
    socketToStreams
  , socketToStreamsWithBufferSize
  ) where

------------------------------------------------------------------------------
import           System.IO.Streams.Internal.Network (socketToStreams, socketToStreamsWithBufferSize)

