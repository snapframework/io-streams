{-# LANGUAGE CPP #-}

module System.IO.Streams.Internal.Network
  ( socketToStreams
  , socketToStreamsWithBufferSize
  , socketToStreamsWithBufferSizeImpl
  ) where


------------------------------------------------------------------------------
import           Control.Exception          (catch)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Internal   as S
import           Data.Word                  (Word8)
import           Foreign.ForeignPtr         (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc      (finalizerFree, mallocBytes)
import           Foreign.Ptr                (Ptr)
import           Network.Socket             (Socket)
import qualified Network.Socket             as N
import qualified Network.Socket.ByteString  as NB
import           Prelude                    (IO, Int, Maybe (..), return, ($!), (<=), (>>=))
import           System.IO.Error            (ioError, isEOFError)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal (InputStream, OutputStream)
import qualified System.IO.Streams.Internal as Streams


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 4096


------------------------------------------------------------------------------
-- | Converts a 'Socket' to an 'InputStream' \/ 'OutputStream' pair. Note that,
-- as is usually the case in @io-streams@, writing a 'Nothing' to the generated
-- 'OutputStream' does not cause the underlying 'Socket' to be closed.
socketToStreams :: Socket
                -> IO (InputStream S.ByteString, OutputStream S.ByteString)
socketToStreams = socketToStreamsWithBufferSize bUFSIZ


------------------------------------------------------------------------------
-- | Converts a 'Socket' to an 'InputStream' \/ 'OutputStream' pair, with
-- control over the size of the receive buffers. Note that, as is usually the
-- case in @io-streams@, writing a 'Nothing' to the generated 'OutputStream'
-- does not cause the underlying 'Socket' to be closed.
socketToStreamsWithBufferSize
    :: Int                      -- ^ how large the receive buffer should be
    -> Socket                   -- ^ network socket
    -> IO (InputStream S.ByteString, OutputStream S.ByteString)
#if MIN_VERSION_network(2,4,0)
socketToStreamsWithBufferSize = socketToStreamsWithBufferSizeImpl N.recvBuf
#else
socketToStreamsWithBufferSize bufsiz socket = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return $! (is, os)

  where
    input = do
        s <- NB.recv socket bufsiz
        return $! if S.null s then Nothing else Just s

    output Nothing  = return $! ()
    output (Just s) = if S.null s then return $! () else NB.sendAll socket s
#endif


------------------------------------------------------------------------------
-- | Dependency-injected implementation of socketToStreamsWithBufferSize (for
-- testing)
socketToStreamsWithBufferSizeImpl
    :: (N.Socket -> Ptr Word8 -> Int -> IO Int)  -- ^ recvBuf
    -> Int                                       -- ^ how large the receive
                                                 --   buffer should be
    -> Socket                                    -- ^ network socket
    -> IO (InputStream S.ByteString, OutputStream S.ByteString)
socketToStreamsWithBufferSizeImpl _recvBuf bufsiz socket = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return $! (is, os)

  where
    recv buf = _recvBuf socket buf bufsiz `catch` \ioe ->
               if isEOFError ioe then return 0 else ioError ioe

    mkFp = mallocBytes bufsiz >>= newForeignPtr finalizerFree

    input = do
        fp <- mkFp
        n  <- withForeignPtr fp recv
        return $! if n <= 0
                    then Nothing
                    else Just $! S.fromForeignPtr fp 0 n

    output Nothing  = return $! ()
    output (Just s) = if S.null s then return $! () else NB.sendAll socket s
