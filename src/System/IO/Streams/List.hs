{-# LANGUAGE BangPatterns #-}

-- | List conversions and utilities.

module System.IO.Streams.List
 ( -- * List conversions
   fromList
 , toList
 , outputToList
 , writeList

   -- * Utility
 , chunkList
 , concatLists
 , listOutputStream
 ) where

import           Control.Concurrent.MVar    (modifyMVar, modifyMVar_, newMVar)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Prelude                    hiding (read)
import           System.IO.Streams.Internal (InputStream, OutputStream,Sink (..), connect, fromGenerator,
                                             makeInputStream, nullSink,
                                             read, sinkToStream,
                                              write, yield)


------------------------------------------------------------------------------
-- | Transforms a list into an 'InputStream' that produces no side effects.
fromList :: [c] -> IO (InputStream c)
fromList inp = newIORef inp >>= makeInputStream . f
  where
    f ref = readIORef ref >>= \l ->
            case l of
              []     -> return Nothing
              (x:xs) -> writeIORef ref xs >> return (Just x)
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
    consumer r = go
      where
        go = Sink $ maybe (return nullSink)
                          (\c -> do
                               modifyMVar_ r $ \dl -> return (dl . (c:))
                               return go)

    flush r = modifyMVar r $ \dl -> return (id, dl [])
{-# INLINE listOutputStream #-}


------------------------------------------------------------------------------
-- | Drains an 'InputStream', converting it to a list. N.B. that this function
-- reads the entire 'InputStream' strictly into memory and as such is not
-- recommended for streaming applications or where the size of the input is not
-- bounded or known.
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
-- the stream.
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
chunkList n input = if n <= 0
                      then error $ "chunkList: bad size: " ++ show n
                      else fromGenerator $ go n id
  where
    go !k dl | k <= 0    = yield (dl []) >> go n id
             | otherwise = do
                   liftIO (read input) >>= maybe finish chunk
      where
        finish  = let l = dl []
                  in if null l then return $! () else yield l
        chunk x = go (k - 1) (dl . (x:))


------------------------------------------------------------------------------
-- | Given an input stream containing lists, produces a new input stream that
-- will yield the concatenation of these lists. See 'Prelude.concat'.
--
-- Example:
--
-- @
-- ghci> Streams.'fromList' [[1,2,3::Int], [4,5,6]] >>=
--       Streams.'concatLists' >>=
--       Streams.'toList'
-- [1,2,3,4,5,6]
-- @
concatLists :: InputStream [a] -> IO (InputStream a)
concatLists input = fromGenerator go
  where
    go      = liftIO (read input) >>= maybe (return $! ()) chunk
    chunk l = sequence_ (map yield l) >> go
