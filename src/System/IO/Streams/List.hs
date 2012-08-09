module System.IO.Streams.List
 ( fromList
 , toList
 , writeList
 , listOutputStream
 ) where

import Control.Concurrent.MVar
import System.IO.Streams.Internal


------------------------------------------------------------------------------
fromList :: [c] -> IO (InputStream c)
fromList = sourceToStream . f
  where
    f []     = Source $ return (nullSource, Nothing)
    f (x:xs) = Source $ return (f xs, Just x)
{-# INLINE fromList #-}


------------------------------------------------------------------------------
listOutputStream :: IO (OutputStream c, IO [c])
listOutputStream = do
    r <- newMVar id
    c <- sinkToStream $ consumer r
    return (c, flush r)

  where
    consumer r = Sink $ maybe (return nullSink)
                              (\c -> do
                                   modifyMVar_ r $ \dl -> return (dl . (c:))
                                   return $ consumer r)

    flush r = modifyMVar r $ \dl -> return (id, dl [])
{-# INLINE listOutputStream #-}


------------------------------------------------------------------------------
toList :: InputStream a -> IO [a]
toList is = do
    (os, grab) <- listOutputStream
    connect is os >> grab
{-# INLINE toList #-}


------------------------------------------------------------------------------
writeList :: [a] -> OutputStream a -> IO ()
writeList xs os = mapM_ (flip write os . Just) xs
{-# INLINE writeList #-}


