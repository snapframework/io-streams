module System.IO.Streams.List
 ( fromList
 , toList
 , writeList
 , listOutputStream
 ) where

import Control.Concurrent.MVar    ( modifyMVar
                                  , modifyMVar_
                                  , newMVar
                                  )
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , Sink(..)
                                  , SP(..)
                                  , connect
                                  , nullSink
                                  , nullSource
                                  , sinkToStream
                                  , sourceToStream
                                  , withDefaultPushback
                                  , write
                                  )


------------------------------------------------------------------------------
-- | Transform a list into an 'InputStream' that produces no side effects
fromList :: [c] -> IO (InputStream c)
fromList = sourceToStream . f
  where
    f []     = withDefaultPushback $ return $! SP (nullSource) Nothing
    f (x:xs) = withDefaultPushback $ return $! SP (f xs) (Just x)
{-# INLINE fromList #-}


------------------------------------------------------------------------------
-- | 'listOutputStream' returns an 'OutputStream' which stores values fed into
-- it and an action which flushes all stored values to a list.
--
-- The flush action resets the store.
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
-- | Drain an 'InputStream', converting it to a list
toList :: InputStream a -> IO [a]
toList is = do
    (os, grab) <- listOutputStream
    connect is os >> grab
{-# INLINE toList #-}


------------------------------------------------------------------------------
-- | Feed a list to an 'OutputStream'
--
-- 'writeList' does not supply a 'Nothing' upon list completion in order to
-- permit further output.
writeList :: [a] -> OutputStream a -> IO ()
writeList xs os = mapM_ (flip write os . Just) xs
{-# INLINE writeList #-}


