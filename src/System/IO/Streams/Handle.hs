module System.IO.Streams.Handle
 ( handleToInputStream
 , handleToOutputStream
 ) where

import qualified Data.ByteString            as S
import           Data.ByteString            (ByteString)
import           System.IO
------------------------------------------------------------------------------
import           System.IO.Streams.Internal


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 32752


------------------------------------------------------------------------------
handleToInputStream :: Handle -> IO (InputStream ByteString)
handleToInputStream h = makeInputStream f
  where
    f = do
        x <- S.hGetSome h bUFSIZ
        return $! if S.null x then Nothing else Just x


------------------------------------------------------------------------------
handleToOutputStream :: Handle -> IO (OutputStream ByteString)
handleToOutputStream h = makeOutputStream f
  where
    f Nothing  = return $! ()
    f (Just x) = if S.null x
                   then hFlush h
                   else S.hPut h x
