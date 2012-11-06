-- | List conversions and utilities

module System.IO.Streams.List
 ( -- * List conversions
   fromList
 , toList
 , outputToList
 , writeList

   -- * Utility
 , filterM
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
                                  , Source(..)
                                  , SP(..)
                                  , connect
                                  , nullSink
                                  , nullSource
                                  , read
                                  , sinkToStream
                                  , sourceToStream
                                  , unRead
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
-- | Drops chunks from an input stream if they fail to match a given filter
-- predicate. See 'Prelude.filter'.
--
-- Items pushed back to the returned stream are propagated back upstream.
--
-- Example:
--
-- @
-- 'fromList' [\"the\", \"quick\", \"brown\", \"fox\"] >>=
--     'filterM' ('return' . (/= \"brown\")) >>= 'toList'
-- ghci> [\"the\",\"quick\",\"fox\"]
-- @
filterM :: (a -> IO Bool)
        -> InputStream a
        -> IO (InputStream a)
filterM p src = sourceToStream source
  where
    source = Source {
               produce  = prod
             , pushback = pb
             }

    prod = read src >>= maybe eof chunk

    chunk s = do
        b <- p s
        if b then return $! SP source (Just s)
             else prod

    eof = return $! flip SP Nothing Source {
            produce  = eof
          , pushback = pb
          }

    pb s = unRead s src >> return source
