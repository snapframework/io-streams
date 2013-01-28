-- | Converting network 'Socket's to streams.
module System.IO.Streams.Network
  ( -- * Sockets to Streams
    socketToStreams
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import           Network.Socket             (Socket)
import qualified Network.Socket.ByteString  as N
import           System.IO.Streams.Internal (InputStream, OutputStream)
import qualified System.IO.Streams.Internal as Streams


------------------------------------------------------------------------------
-- | Converts a 'Socket' to an 'InputStream' \/ 'OutputStream' pair. Note that,
-- as is usually the case in @io-streams@, writing a 'Nothing' to the generated
-- 'OutputStream' does not cause the underlying 'Socket' to be closed.
socketToStreams :: Socket
                -> IO (InputStream ByteString, OutputStream ByteString)
socketToStreams socket = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return $! (is, os)

  where
    bUFSIZ = 32752

    input = do
        s <- N.recv socket bUFSIZ
        return $! if S.null s then Nothing else Just s

    output Nothing  = return $! ()
    output (Just s) = N.sendAll socket s
