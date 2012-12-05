{-# LANGUAGE CPP         #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- | Input and output streams for file 'Handle's.
module System.IO.Streams.Handle
 ( -- * Handle conversions
   handleToInputStream
 , handleToOutputStream
 , stdin
 , stdout
 , stderr
 ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as S
import           System.IO                  (Handle, hFlush)
import qualified System.IO                  as IO
import           System.IO.Unsafe           (unsafePerformIO)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal (InputStream, OutputStream,
                                             lockingInputStream,
                                             lockingOutputStream,
                                             makeInputStream,
                                             makeOutputStream)


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 32752


------------------------------------------------------------------------------
-- | Converts a read-only handle into an 'InputStream' of strict 'ByteString's.
handleToInputStream :: Handle -> IO (InputStream ByteString)
handleToInputStream h = makeInputStream f
  where
    f = do
        x <- S.hGetSome h bUFSIZ
        return $! if S.null x then Nothing else Just x


------------------------------------------------------------------------------
-- | Converts a writable handle into an 'OutputStream' of strict 'ByteString's.
handleToOutputStream :: Handle -> IO (OutputStream ByteString)
handleToOutputStream h = makeOutputStream f
  where
    f Nothing  = return $! ()
    f (Just x) = if S.null x
                   then hFlush h
                   else S.hPut h x


------------------------------------------------------------------------------
-- | An 'InputStream' for 'IO.stdin'.
stdin :: InputStream ByteString
stdin = unsafePerformIO (handleToInputStream IO.stdin >>= lockingInputStream)


------------------------------------------------------------------------------
-- | An 'OutputStream' for 'IO.stdout'.
stdout :: OutputStream ByteString
stdout = unsafePerformIO (handleToOutputStream IO.stdout >>=
                          lockingOutputStream)


------------------------------------------------------------------------------
-- | An 'OutputStream' for 'IO.stderr'.
stderr :: OutputStream ByteString
stderr = unsafePerformIO (handleToOutputStream IO.stderr >>=
                          lockingOutputStream)
