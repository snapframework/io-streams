-- | Converting network 'Socket's to streams.
module System.IO.Streams.Network
  ( -- * Sockets to Streams
    socketToStreams
  , socketToStreamsWithBufferSize
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import           Foreign.Storable           (sizeOf)
import           Network.Socket             (Socket)
import qualified Network.Socket.ByteString  as N
import           System.IO.Streams.Internal (InputStream, OutputStream)
import qualified System.IO.Streams.Internal as Streams


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 8192 - overhead
  where
    overhead = 4 * (sizeOf $! (0 :: Int))


------------------------------------------------------------------------------
-- | Converts a 'Socket' to an 'InputStream' \/ 'OutputStream' pair. Note that,
-- as is usually the case in @io-streams@, writing a 'Nothing' to the generated
-- 'OutputStream' does not cause the underlying 'Socket' to be closed.
socketToStreams :: Socket
                -> IO (InputStream ByteString, OutputStream ByteString)
socketToStreams = socketToStreamsWithBufferSize bUFSIZ


------------------------------------------------------------------------------
-- | Converts a 'Socket' to an 'InputStream' \/ 'OutputStream' pair, with
-- control over the size of the receive buffers. Note that, as is usually the
-- case in @io-streams@, writing a 'Nothing' to the generated 'OutputStream'
-- does not cause the underlying 'Socket' to be closed.
socketToStreamsWithBufferSize
    :: Int                      -- ^ how large the receive buffer should be
    -> Socket                   -- ^ network socket
    -> IO (InputStream ByteString, OutputStream ByteString)
socketToStreamsWithBufferSize bufsiz socket = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return $! (is, os)

  where
    input = do
        s <- N.recv socket bufsiz
        return $! if S.null s then Nothing else Just s

    output Nothing  = return $! ()
    output (Just s) = if S.null s then return $! () else N.sendAll socket s
