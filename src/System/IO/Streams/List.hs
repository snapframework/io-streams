{-# LANGUAGE BangPatterns #-}

-- | List conversions and utilities

module System.IO.Streams.List
 ( -- * List conversions
   fromList
 , toList
 , outputToList
 , writeList

   -- * Utility
 , chunkList
 , joinLists
 , listOutputStream
 ) where

import Control.Concurrent.MVar    ( modifyMVar
                                  , modifyMVar_
                                  , newMVar
                                  )
import Prelude hiding             ( read )
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , Sink(..)
                                  , SP(..)
                                  , connect
                                  , makeInputStream
                                  , nullSink
                                  , nullSource
                                  , read
                                  , sinkToStream
                                  , sourceToStream
                                  , withDefaultPushback
                                  , write
                                  )


------------------------------------------------------------------------------
-- | Transforms a list into an 'InputStream' that produces no side effects.
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
--
-- Note that this function /will/ buffer any input sent to it on the heap.
-- Please don't use this unless you're sure that the amount of input provided
-- is bounded and will fit in memory without issues.
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
-- | Drains an 'InputStream', converting it to a list.
toList :: InputStream a -> IO [a]
toList is = outputToList (connect is)
{-# INLINE toList #-}


------------------------------------------------------------------------------
-- | Given an IO action that requires an 'OutputStream', creates one and
-- captures all the output the action sends to it as a list.
--
-- Example:
--
-- @
-- ghci> import "Control.Applicative"
-- ghci> ('connect' <$> 'fromList' [\"a\", \"b\", \"c\"]) >>= 'outputToList'
-- ["a","b","c"]
-- @
outputToList :: (OutputStream a -> IO b) -> IO [a]
outputToList f = do
    (os, getList) <- listOutputStream
    _ <- f os
    getList
{-# INLINE outputToList #-}


------------------------------------------------------------------------------
-- | Feeds a list to an 'OutputStream'. Does /not/ write an end-of-stream to
-- the list.
writeList :: [a] -> OutputStream a -> IO ()
writeList xs os = mapM_ (flip write os . Just) xs
{-# INLINE writeList #-}


------------------------------------------------------------------------------
-- | Splits an input stream into chunks of at most size @n@.
--
-- Example:
--
-- @
-- ghci> 'fromList' [1..14::Int] >>= 'chunkList' 4 >>= 'toList'
-- [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14]]
-- @
chunkList :: Int                   -- ^ chunk size
          -> InputStream a         -- ^ stream to process
          -> IO (InputStream [a])
chunkList n input = makeInputStream $ do
    read input >>= maybe (return Nothing) (go (n-1) . (:))
  where
    finish !dl = return $! Just $! dl []

    go !k dl = if k <= 0
                 then finish dl
                 else read input >>=
                      maybe (finish dl) (go (k-1) . (dl .) . (:))


------------------------------------------------------------------------------
joinLists = undefined
