module System.IO.Streams.Handle
 ( handleToInputStream
 , handleToOutputStream
 , withFileAsInputStream
 , withFileAsOutputStream
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
    f (Just x) = S.hPut h x


------------------------------------------------------------------------------
withFileAsInputStream :: FilePath
                      -> (InputStream ByteString -> IO a)
                      -> IO a
withFileAsInputStream fp m =
    withBinaryFile fp ReadMode ((m =<<) . handleToInputStream)


------------------------------------------------------------------------------
withFileAsOutputStream :: FilePath
                       -> IOMode
                       -> (OutputStream ByteString -> IO a)
                       -> IO a
withFileAsOutputStream fp mode m =
    withBinaryFile fp mode ((m =<<) . handleToOutputStream)
